#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>

#include <erl_nif.h>
#include <wasm_export.h>


static char* read_wasm_binary_to_buffer(const char* wasm_file,
					uint32_t* size)
{
  FILE* ff;
  int fd;
  struct stat statbuf;
  char* buf;

  ff = fopen(wasm_file, "r");
  if (!ff) {
    fprintf(stderr, "File not found: %s\n", wasm_file);
    perror("fopen");
    exit(1);
  }
  fd = fileno(ff);
  if (fstat(fd, &statbuf) != 0) {
    fprintf(stderr, "Could not stat: %s\n", wasm_file);
    perror("fstat");
    exit(1);
  }

  buf = malloc(statbuf.st_size);
  if (fread(buf, 1, statbuf.st_size, ff) != statbuf.st_size) {
    fprintf(stderr, "Could not read: %s\n", wasm_file);
    perror("fread");
    exit(1);
  }
  if (fclose(ff) != 0) {
    fprintf(stderr, "Could not close: %s\n", wasm_file);
    perror("fclose");
    exit(1);
  }
  *size = statbuf.st_size;
  return buf;
}

static int enif_printf_I(wasm_exec_env_t exec_env, const char* fmt, int arg1)
{
  return enif_fprintf(stderr, fmt, arg1);
}

static int64_t enif_printf_L(wasm_exec_env_t exec_env, const char* fmt, int64_t arg1)
{
  return (int64_t) enif_fprintf(stderr, fmt, arg1);
}

static int enif_printf_F(wasm_exec_env_t exec_env, const char* fmt, double arg1)
{
  return enif_fprintf(stderr, fmt, arg1);
}

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_void;
static ERL_NIF_TERM atom_wasm_error;
static ErlNifResourceType* the_module_exec_rt;

#define DEFAULT_N_TERMS 16

typedef uint64_t ERL_NIF_TERM_wasm;  // ERL_NIF_TERM in wasm land
typedef uint32_t ErlNifEnv_wasm;   // ErlNifEnv in wasm land

struct term_env
{
    ErlNifEnv* env;
    wasm_module_inst_t mi;
    unsigned n_alloc_terms;
    unsigned n_used_terms;
    ERL_NIF_TERM* terms;
    ERL_NIF_TERM default_terms[DEFAULT_N_TERMS];
};

struct module_exec_resrc
{
    wasm_module_inst_t module_inst;
    wasm_exec_env_t exec_env;
    ErlNifMutex* lock;
    ErlNifPid proc;
    struct term_env call_env;
};

static void init_term_env(struct term_env* tenv, ErlNifEnv* env)
{
    tenv->env = env;
    tenv->n_alloc_terms = DEFAULT_N_TERMS;
    tenv->n_used_terms = 0;
    tenv->terms = tenv->default_terms;
}

static void clear_term_env(struct term_env* tenv)
{
    if (tenv->terms != tenv->default_terms)
        enif_free(tenv->terms);
    tenv->env = NULL;
    tenv->terms = NULL;
}


#define CALL_ENV_OFFS 1

static bool term_env_from_wasm(wasm_exec_env_t exec_env,
                               ErlNifEnv_wasm env_w,
                               struct term_env** tenv_p)
{
    wasm_module_inst_t mi = wasm_runtime_get_module_inst(exec_env);
    struct module_exec_resrc* resrc = wasm_runtime_get_custom_data(mi);
    struct term_env* tenv = &resrc->call_env;

    if (env_w != CALL_ENV_OFFS || tenv->env == NULL) {
        if (!wasm_runtime_get_exception(mi)) {
            wasm_runtime_set_exception(mi, "Invalid ErlNifEnv argument");
        }
        return false;
    }

    tenv->mi = mi;
    *tenv_p = tenv;
    return true;
}

static ERL_NIF_TERM_wasm term_to_wasm(struct term_env *tenv, ERL_NIF_TERM term)
{
    unsigned ix;

    if ((term & 3) == 3) {
        return (uint64_t)term;
    }
    for (ix=0; ix < tenv->n_used_terms; ix++) {
        if (tenv->terms[ix] == term)
            goto done;
    }
    if (tenv->n_used_terms == tenv->n_alloc_terms) {
        tenv->n_alloc_terms *= 2;
        if (tenv->terms == tenv->default_terms) {
            tenv->terms = enif_alloc(tenv->n_alloc_terms * sizeof(ERL_NIF_TERM));
            memcpy(tenv->terms, tenv->default_terms, sizeof(tenv->default_terms));
        }
        else {
            tenv->terms = enif_realloc(tenv->terms,
                                       tenv->n_alloc_terms * sizeof(ERL_NIF_TERM));
        }
    }
    tenv->terms[ix] = term;
    tenv->n_used_terms++;

done:
    return ix << 2;
}

static bool term_from_wasm(struct term_env *tenv,
                           ERL_NIF_TERM_wasm term_w,
                           ERL_NIF_TERM* term_p)
{
    unsigned ix;

    if ((term_w & 3) == 3 || enif_is_exception(tenv->env, term_w)) {
        *term_p = (ERL_NIF_TERM) term_w;
        return true;
    }
    ix = term_w >> 2;
    if (ix < tenv->n_used_terms) {
        *term_p = tenv->terms[ix];
        return true;
    }
    if (!wasm_runtime_get_exception(tenv->mi)) {
        wasm_runtime_set_exception(tenv->mi, "Invalid wasm term value");
    }
    return false;
}

static bool ptr_from_wasm(struct term_env* tenv, uint32_t ptr_offs,
                          uint32_t n_bytes, void** ptr_p)
{
    if (!wasm_runtime_validate_app_addr(tenv->mi, ptr_offs, n_bytes))
        return false;

    *ptr_p = wasm_runtime_addr_app_to_native(tenv->mi, ptr_offs);
    return true;
}

static int32_t enif_wasm_get_int32(wasm_exec_env_t exec_env,
                                   ErlNifEnv_wasm env_w,
                                   uintptr_t term,
                                   uint32_t int_offs)
{
    wasm_module_inst_t mi;
    struct term_env* tenv;
    int32_t* int_ptr;

    if (!term_env_from_wasm(exec_env, env_w, &tenv)
        || !ptr_from_wasm(tenv, int_offs, sizeof(int32_t), (void**)&int_ptr))
        return 0; // raise wasm exception?

    return enif_get_int(tenv->env, (ERL_NIF_TERM)term, int_ptr);
}

static ERL_NIF_TERM_wasm enif_wasm_make_int32(wasm_exec_env_t exec_env,
                                              ErlNifEnv_wasm env_w,
                                              int32_t value)
{
    struct term_env* tenv;

    if (!term_env_from_wasm(exec_env, env_w, &tenv))
        return atom_wasm_error;

    return enif_make_int(tenv->env, value);
}

static ERL_NIF_TERM_wasm enif_wasm_make_badarg(wasm_exec_env_t exec_env,
                                               ErlNifEnv_wasm env_w)
{
    struct term_env* tenv;

    if (!term_env_from_wasm(exec_env, env_w, &tenv))
        return atom_wasm_error;

    return enif_make_badarg(tenv->env);
}


static int32_t enif_wasm_get_list_cell(wasm_exec_env_t exec_env,
                                       ErlNifEnv_wasm env_w,
                                       ERL_NIF_TERM_wasm list_w,
                                       uint32_t head_offs,
                                       uint32_t tail_offs)
{
    struct term_env* tenv;
    ERL_NIF_TERM list_term, head_term, tail_term;
    ERL_NIF_TERM *head_ptr, *tail_ptr;

    if (!term_env_from_wasm(exec_env, env_w, &tenv)
        || !ptr_from_wasm(tenv, head_offs, sizeof(ERL_NIF_TERM), (void**)&head_ptr)
        || !ptr_from_wasm(tenv, tail_offs, sizeof(ERL_NIF_TERM), (void**)&tail_ptr)
        || !term_from_wasm(tenv, list_w, &list_term)) {
        return 0;
    }

    if (!enif_get_list_cell(tenv->env, list_term, &head_term, &tail_term)) {
        return 0;
    }
    *head_ptr = term_to_wasm(tenv, head_term);
    *tail_ptr = term_to_wasm(tenv, tail_term);
    return 1;
}

static int32_t enif_wasm_is_empty_list(wasm_exec_env_t exec_env,
                                       ErlNifEnv_wasm env_w,
                                       ERL_NIF_TERM_wasm term_w)
{
    struct term_env* tenv;
    ERL_NIF_TERM term;
    if (!term_env_from_wasm(exec_env, env_w, &tenv)
        || !term_from_wasm(tenv, term_w, &term))
        return 0;

    return enif_is_empty_list(tenv->env, term);
}

static int32_t enif_wasm_get_binary_size(wasm_exec_env_t exec_env,
                                         ErlNifEnv_wasm env_w,
                                         ERL_NIF_TERM_wasm binary_w,
                                         uint32_t size_offs)
{
    struct term_env* tenv;
    ERL_NIF_TERM binary;
    ErlNifBinary bin;
    uint32_t* size_p;

    if (!term_env_from_wasm(exec_env, env_w, &tenv)
        || !term_from_wasm(tenv, binary_w, &binary)
        || !ptr_from_wasm(tenv, size_offs, 4, (void**)&size_p)
        || !enif_inspect_binary(tenv->env, binary, &bin)
        || bin.size > UINT32_MAX)
        return 0;

    *size_p = bin.size;
    return 1;
}

static int32_t enif_wasm_get_binary_bytes(wasm_exec_env_t exec_env,
                                          ErlNifEnv_wasm env_w,
                                          ERL_NIF_TERM_wasm binary_w,
                                          uint32_t offset,
                                          uint32_t nbytes,
                                          uint32_t dest_offs)
{
    struct term_env* tenv;
    ERL_NIF_TERM binary;
    ErlNifBinary bin;
    unsigned char* dest_p;

    if (!term_env_from_wasm(exec_env, env_w, &tenv)
        || !term_from_wasm(tenv, binary_w, &binary)
        || !ptr_from_wasm(tenv, dest_offs, nbytes, (void**)&dest_p)
        || !enif_inspect_binary(tenv->env, binary, &bin)
        || offset + nbytes > bin.size)
        return 0;

    memcpy(dest_p, bin.data, nbytes);
    return 1;
}

static ERL_NIF_TERM_wasm enif_wasm_make_binary(wasm_exec_env_t exec_env,
                                               ErlNifEnv_wasm env_w,
                                               uint32_t src_offs,
                                               uint32_t nbytes)
{
    struct term_env* tenv;
    const unsigned char* src_p;
    unsigned char* dst_p;
    ERL_NIF_TERM binary;

    if (!term_env_from_wasm(exec_env, env_w, &tenv)
        || !ptr_from_wasm(tenv, src_offs, nbytes, (void**)&src_p))
        return atom_wasm_error;

    dst_p = enif_make_new_binary(tenv->env, nbytes, &binary);
    memcpy(dst_p, src_p, nbytes);
    return term_to_wasm(tenv, binary);
}


/* the native functions that will be exported to WASM app */
static NativeSymbol native_symbols[] = {
    EXPORT_WASM_API_WITH_SIG(enif_printf_I, "($i)i"),
    EXPORT_WASM_API_WITH_SIG(enif_printf_L, "($I)I"),
    EXPORT_WASM_API_WITH_SIG(enif_printf_F, "($F)i"),

    EXPORT_WASM_API_WITH_SIG(enif_wasm_get_int32, "(iIi)i"),
    EXPORT_WASM_API_WITH_SIG(enif_wasm_make_int32, "(ii)I"),
    EXPORT_WASM_API_WITH_SIG(enif_wasm_make_badarg, "(i)I"),
    EXPORT_WASM_API_WITH_SIG(enif_wasm_get_list_cell, "(iIii)i"),
    EXPORT_WASM_API_WITH_SIG(enif_wasm_is_empty_list, "(iI)i"),
    EXPORT_WASM_API_WITH_SIG(enif_wasm_get_binary_size, "(iIi)i"),
    EXPORT_WASM_API_WITH_SIG(enif_wasm_get_binary_bytes, "(iIiii)i"),
    EXPORT_WASM_API_WITH_SIG(enif_wasm_make_binary, "(iii)I"),
};


static ERL_NIF_TERM call_raw(ErlNifEnv*, struct module_exec_resrc*,
                            ERL_NIF_TERM func_atom, ERL_NIF_TERM arg_list);
static ERL_NIF_TERM call(ErlNifEnv*, struct module_exec_resrc*,
                         ERL_NIF_TERM func_atom, ERL_NIF_TERM arg_list);
static ERL_NIF_TERM print_func(ErlNifEnv* env, struct module_exec_resrc* resrc,
                               ERL_NIF_TERM func_atom);


static const char* type_str(enum wasm_valkind_enum type)
{
    switch (type) {
    case WASM_I32: return "I32";
    case WASM_I64: return "I64";
    case WASM_F32: return "F32";
    case WASM_F64: return "F64";
    case WASM_ANYREF: return "ANYREF";
    case WASM_FUNCREF: return "FUNCREF";
    }
    return "INVALID TYPE";
}

wasm_module_t the_module;
static char error_buf[128];

static bool call_func(wasm_exec_env_t exec_env,
                      wasm_function_inst_t func,
                      uint32_t n_args, wasm_val_t* args,
                      uint32_t n_ret, wasm_val_t* result)
{
    bool ok;
    ok = wasm_runtime_init_thread_env();
    assert(ok);

    /* call the WASM function */
    ok = wasm_runtime_call_wasm_a(exec_env, func, n_ret, result, n_args, args);
    wasm_runtime_destroy_thread_env();

    return ok;
}

static const uint32_t stack_size = 8092, heap_size = 8092;

static void module_exec_destructor(ErlNifEnv* caller_env, void* obj)
{
    struct module_exec_resrc* resrc = (struct module_exec_resrc*) obj;

    wasm_runtime_destroy_exec_env(resrc->exec_env);
    wasm_runtime_deinstantiate(resrc->module_inst);
    if (resrc->lock)
        enif_mutex_destroy(resrc->lock);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    char *buffer;
    uint32_t size, stack_size = 8092, heap_size = 8092;

    atom_ok   = enif_make_atom(env, "ok");
    atom_void = enif_make_atom(env, "void");
    atom_wasm_error = enif_make_atom(env, "wasm_error__should_not_leak_out");

    the_module_exec_rt = enif_open_resource_type(env, NULL, "module_exec",
                                                 module_exec_destructor,
                                                 ERL_NIF_RT_CREATE, NULL);
    if (!the_module_exec_rt)
        return __LINE__;

    /* all the runtime memory allocations are retricted in the global_heap_buf array */
    static char global_heap_buf[512 * 1024];
    RuntimeInitArgs init_args;
    memset(&init_args, 0, sizeof(RuntimeInitArgs));

    /* configure the memory allocator for the runtime */
    init_args.mem_alloc_type = Alloc_With_Pool;
    init_args.mem_alloc_option.pool.heap_buf = global_heap_buf;
    init_args.mem_alloc_option.pool.heap_size = sizeof(global_heap_buf);

    /* configure the native functions being exported to WASM app */
    init_args.native_module_name = "env";
    init_args.n_native_symbols = sizeof(native_symbols) / sizeof(NativeSymbol);
    init_args.native_symbols = native_symbols;

    /* set maximum thread number if needed when multi-thread is enabled,
       the default value is 4 */
    init_args.max_thread_num = 4;

    /* initialize runtime environment with user configurations*/
    if (!wasm_runtime_full_init(&init_args)) {
      return -1;
    }

    /* read WASM file into a memory buffer */
    buffer = read_wasm_binary_to_buffer("./add.wasm", &size);

    /* parse the WASM file from buffer and create a WASM module */
    the_module = wasm_runtime_load(buffer, size, error_buf, sizeof(error_buf));

    return 0;
}
static void unload(ErlNifEnv* caller_env, void* priv_data)
{
}

static ERL_NIF_TERM make_str(ErlNifEnv* env, const char* str)
{
    return enif_make_string(env, str, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM raise_exception(ErlNifEnv* env, const char* str)
{
    return enif_raise_exception(env, make_str(env, str));
}

enum { N_ARGS_MAX = 10 };

static int get_func_info(ErlNifEnv* env,
                         wasm_module_inst_t module_inst,
                         ERL_NIF_TERM func_atom,
                         wasm_function_inst_t* func_p,
                         uint32_t* n_args_p,
                         wasm_valkind_t arg_types[N_ARGS_MAX],
                         uint32_t* n_ret_p,
                         wasm_valkind_t* ret_type_p,
                         ERL_NIF_TERM* err_ret)
{
    char func_name[20];
    wasm_function_inst_t* func;

    if (!enif_get_atom(env, func_atom, func_name, sizeof(func_name), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    func = wasm_runtime_lookup_function(module_inst, func_name, NULL);
    if (!func) {
        *err_ret = raise_exception(env, "Function undefined");
        return 0;
    }

    *n_args_p = wasm_func_get_param_count(func, module_inst);
    if (*n_args_p > N_ARGS_MAX) {
        *err_ret = raise_exception(env, "Function arity notsup");
        return 0;
    }
    wasm_func_get_param_types(func, module_inst, arg_types);

    *n_ret_p = wasm_func_get_result_count(func, module_inst);
    if (*n_ret_p > 1) {
        *err_ret = raise_exception(env, "Function multiple return notsup");
        return 0;
    }
    if (ret_type_p)
        wasm_func_get_result_types(func, module_inst, ret_type_p);

    *func_p = func;
    return 1;
}

static bool try_lock_resrc(ErlNifEnv* env, ERL_NIF_TERM inst_term,
                           struct module_exec_resrc** resrc_p,
                           ERL_NIF_TERM* error_ret_p)
{
    struct module_exec_resrc* resrc;
    const char* except;

    *resrc_p = NULL;
    if (!enif_get_resource(env, inst_term, the_module_exec_rt, (void**)&resrc)) {
        *error_ret_p = enif_make_badarg(env);
        return false;
    }
    if (resrc->lock) {
        enif_mutex_lock(resrc->lock);
    }
    else {
        ErlNifPid self;
        enif_self(env, &self);
        if (enif_compare_pids(&self, &resrc->proc) != 0) {
            *error_ret_p = raise_exception(env, "called by wrong process");
            return false;
        }
    }
    except = wasm_runtime_get_exception(resrc->module_inst);
    if (except) {
        ERL_NIF_TERM tpl =
            enif_make_tuple2(env,
                             make_str(env, "Wasm modul already invalidated by"),
                             make_str(env, except));
        if (resrc->lock)
            enif_mutex_unlock(resrc->lock);
        *error_ret_p = enif_raise_exception(env, tpl);
        return false;
    }

    *resrc_p = resrc;
    return true;
}


static void unlock_resrc(struct module_exec_resrc* resrc)
{
    if (resrc && resrc->lock)
        enif_mutex_unlock(resrc->lock);
}

static ERL_NIF_TERM call_raw_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct module_exec_resrc* resrc;
    ERL_NIF_TERM ret;

    if (try_lock_resrc(env, argv[0], &resrc, &ret)) {
        ret = call_raw(env, resrc, argv[1], argv[2]);
    }
    unlock_resrc(resrc);
    return ret;
}

static ERL_NIF_TERM call_raw(ErlNifEnv* env, struct module_exec_resrc* resrc,
                             ERL_NIF_TERM func_atom,
                             ERL_NIF_TERM arg_list)
{
    uint32_t n_args, n_ret;
    wasm_val_t args[N_ARGS_MAX];
    wasm_val_t result;
    wasm_valkind_t arg_types[N_ARGS_MAX];
    wasm_function_inst_t func;
    ERL_NIF_TERM head, tail, err_ret;
    const char* error;
    uint32_t i;

    if (!get_func_info(env, resrc->module_inst, func_atom, &func, &n_args, arg_types,
                       &n_ret, NULL, &err_ret))
        return err_ret;

    i = 0;
    for (head = arg_list; !enif_is_empty_list(env, head); head = tail, i++) {
        int ok;
        if (i >= n_args || !enif_get_list_cell(env, head, &head, &tail))
            return enif_make_badarg(env);

        args[i].kind = arg_types[i];
        switch (arg_types[i]) {
        case WASM_I32: ok = enif_get_int(env, head, &args[i].of.i32); break;
        case WASM_I64: ok = enif_get_int64(env, head, &args[i].of.i64); break;
        case WASM_F64: ok = enif_get_double(env, head, &args[i].of.f64); break;
        default:
            return raise_exception(env, "Function argument type notsup");
        }
        if (!ok)
            return enif_make_badarg(env);
    }
    if (i != n_args)
        return enif_make_badarg(env);

    if (!call_func(resrc->exec_env, func, n_args, args, n_ret, &result)) {
        const char* error;
        /* exception is thrown if call fails */
        error = wasm_runtime_get_exception(wasm_runtime_get_module_inst(resrc->exec_env));
        return raise_exception(env, error);
    }
    if (n_ret == 0)
        return atom_void;

    switch (result.kind) {
    case WASM_I32: return enif_make_int(env, result.of.i32);
    case WASM_I64: return enif_make_int64(env, result.of.i64);
    case WASM_F64: return enif_make_double(env, result.of.f64);
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM call_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct module_exec_resrc* resrc;
    ERL_NIF_TERM ret;

    if (try_lock_resrc(env, argv[0], &resrc, &ret)) {
        ret = call(env, resrc, argv[1], argv[2]);
    }
    unlock_resrc(resrc);
    return ret;
}

static ERL_NIF_TERM call(ErlNifEnv* env, struct module_exec_resrc* resrc,
                         ERL_NIF_TERM func_atom,
                         ERL_NIF_TERM arg_list)
{
    uint32_t n_args, n_ret;
    wasm_val_t args[N_ARGS_MAX];
    wasm_val_t result;
    wasm_valkind_t arg_types[N_ARGS_MAX];
    wasm_function_inst_t func;
    ERL_NIF_TERM head, tail, ret_term;
    uint32_t i;

    if (!get_func_info(env, resrc->module_inst, func_atom, &func,
                       &n_args, arg_types,
                       &n_ret, &result.kind, &ret_term))
        return ret_term;

    if (n_ret != 1 || result.kind != WASM_I64)
        return raise_exception(env, "Wasm NIF incorrect return type");

    if (n_args < 1)
        return raise_exception(env, "Wasm NIF argument 1 missing");

    if (arg_types[0] != WASM_I32)
        return raise_exception(env, "Wasm NIF argument 1 incorrect type");

    init_term_env(&resrc->call_env, env);

    args[0].kind = WASM_I32;
    args[0].of.i32 = CALL_ENV_OFFS;

    i = 1;
    for (head = arg_list; !enif_is_empty_list(env, head); head = tail, i++) {
        int ok;
        if (i >= n_args || !enif_get_list_cell(env, head, &head, &tail))
            return raise_exception(env, "Wasm NIF too many arguments in list");

        args[i].kind = arg_types[i];
        if (arg_types[i] != WASM_I64)
            return raise_exception(env, "Wasm NIF incorrect ERL_NIF_TERM type");

        args[i].of.i64 = term_to_wasm(&resrc->call_env, head);
    }
    if (i != n_args)
        return raise_exception(env, "Wasm NIF too few arguments in list");

    if (!call_func(resrc->exec_env, func, n_args, args, n_ret, &result)
        || !term_from_wasm(&resrc->call_env, result.of.i64, &ret_term))
    {
        const char* error;
        clear_term_env(&resrc->call_env);
        /* exception is thrown if call fails */
        error = wasm_runtime_get_exception(wasm_runtime_get_module_inst(resrc->exec_env));

        ret_term = raise_exception(env, error);
        //wasm_runtime_clear_exception(resrc->module_inst);
        return ret_term;
    }

    clear_term_env(&resrc->call_env);
    return ret_term;
}

static ERL_NIF_TERM print_func_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    struct module_exec_resrc* resrc;
    ERL_NIF_TERM ret;

    if (try_lock_resrc(env, argv[0], &resrc, &ret)) {
        ret = print_func(env, resrc, argv[1]);
    }
    unlock_resrc(resrc);
    return ret;
}

static ERL_NIF_TERM print_func(ErlNifEnv* env, struct module_exec_resrc* resrc,
                               ERL_NIF_TERM func_atom)
{
    char func_name[20];
    wasm_function_inst_t func;

    if (!enif_get_atom(env, func_atom, func_name, sizeof(func_name), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    func = wasm_runtime_lookup_function(resrc->module_inst, func_name, NULL);
    if (!func)
        return raise_exception(env, "Function undefined");

    {
        uint32_t narg = wasm_func_get_param_count(func, resrc->module_inst);
        uint32_t nret = wasm_func_get_result_count(func, resrc->module_inst);
        wasm_valkind_t arg_types[narg];
        wasm_valkind_t ret_types[nret];
        uint32_t i;
        const char* delim;

        wasm_func_get_param_types(func, resrc->module_inst, arg_types);
        enif_fprintf(stdout,"\n%s(", func_name);
        delim = "";
        for (i=0; i < narg; i++) {
            enif_fprintf(stdout,"%s%s", delim, type_str(arg_types[i]));
            delim = ",";
        }
        enif_fprintf(stdout,") -> ");
        wasm_func_get_result_types(func, resrc->module_inst, ret_types);
        delim = "";
        for (i=0; i < nret; i++) {
            enif_fprintf(stdout,"%s%s", delim, type_str(ret_types[i]));
            delim = ",";
        }
        enif_fprintf(stdout,"\n");
    }

    return atom_ok;
}

static ERL_NIF_TERM new_instance_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    enum { PROCESS_BOUND=1 };
    int flags;
    char error_buf[100];
    struct module_exec_resrc* resrc;
    ERL_NIF_TERM ret;
    wasm_exec_env_t ee;
    wasm_module_inst_t mi;
    bool ok;

    if (!enif_get_int(env, argv[0], &flags)
        || flags & ~PROCESS_BOUND)
        return enif_make_badarg(env);

    ok = wasm_runtime_init_thread_env();
    assert(ok); (void)ok;

    mi = wasm_runtime_instantiate(the_module, stack_size, heap_size,
                                  error_buf, sizeof(error_buf));
    if (!mi) {
        ret = raise_exception(env, error_buf);
        goto done;
    }

    ee = wasm_runtime_create_exec_env(mi, stack_size);
    if (!ee) {
        wasm_runtime_deinstantiate(mi);
        ret = raise_exception(env, "wasm_runtime_create_exec_env FAILED");
        goto done;
    }

    resrc = enif_alloc_resource(the_module_exec_rt,
                                sizeof(struct module_exec_resrc));
    assert(resrc);
    resrc->module_inst = mi;
    resrc->exec_env = ee;
    resrc->call_env.env = NULL;
    resrc->call_env.terms = NULL;
    if (flags & PROCESS_BOUND) {
        enif_self(env, &resrc->proc);
        resrc->lock = NULL;
    }
    else {
        enif_set_pid_undefined(&resrc->proc);
        resrc->lock = enif_mutex_create("wasm-instance");
    }

    wasm_runtime_set_custom_data(mi, resrc);

    ret = enif_make_resource(env, resrc);
    enif_release_resource(resrc);

done:
    wasm_runtime_destroy_thread_env();
    return ret;
}

static ErlNifFunc nif_funcs[] =
{
    {"print_func", 2, print_func_nif},
    {"new_instance", 1, new_instance_nif},
    {"call", 3, call_nif},
    {"call_raw", 3, call_raw_nif}
};

ERL_NIF_INIT(wasm_runtime_nif,nif_funcs,load,NULL,NULL,unload)

