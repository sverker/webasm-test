#include <string.h>

#include <erl_nif.h>
#include <wasm_export.h>

#include "tester.h"

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_void;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok   = enif_make_atom(env, "ok");
    atom_void = enif_make_atom(env, "void");
    return tester_init();
}

static ERL_NIF_TERM hello_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tester_run();
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM raise_exception(ErlNifEnv* env, const char* str)
{
    return enif_raise_exception(env, enif_make_string(env, str, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM apply_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char func_name[20];
    uint32_t n_args, n_ret;
    enum { N_ARGS_MAX = 10 };
    wasm_val_t args[N_ARGS_MAX];
    wasm_val_t result;
    wasm_valkind_t arg_types[N_ARGS_MAX];
    wasm_function_inst_t func;
    ERL_NIF_TERM head, tail;
    const char* error;
    uint32_t i;

    if (!enif_get_atom(env, argv[0], func_name, sizeof(func_name), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    func = wasm_runtime_lookup_function(module_inst, func_name, NULL);
    if (!func)
        return raise_exception(env, "Function undefined");

    n_args = wasm_func_get_param_count(func, module_inst);
    if (n_args > N_ARGS_MAX)
        return raise_exception(env, "Function arity notsup");

    n_ret = wasm_func_get_result_count(func, module_inst);
    if (n_ret > 1)
        return raise_exception(env, "Function multiple return notsup");

    wasm_func_get_param_types(func, module_inst, arg_types);

    i = 0;
    for (head = argv[1]; !enif_is_empty_list(env, head); head = tail, i++) {
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

    if (!tester_call_func(func, n_args, args, n_ret, &result, &error)) {
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

static ERL_NIF_TERM print_func_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char func_name[20];
    wasm_function_inst_t func;

    if (!enif_get_atom(env, argv[0], func_name, sizeof(func_name), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    func = wasm_runtime_lookup_function(module_inst, func_name, NULL);
    if (!func)
        return raise_exception(env, "Function undefined");

    print_func(func_name, func, module_inst);
    return atom_ok;
}

static ERL_NIF_TERM arg_binary_alloc_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    uint32_t app_offset;

    if (!enif_inspect_binary(env, argv[0], &bin))
        return enif_make_badarg(env);

    app_offset = wasm_runtime_module_dup_data(module_inst, bin.data, bin.size);
    return enif_make_uint(env, app_offset);
}

static ERL_NIF_TERM arg_binary_free_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32_t app_offset;

    if (!enif_get_uint(env, argv[0], &app_offset)
        || !wasm_runtime_validate_app_addr(module_inst, app_offset, 1))
        return enif_make_badarg(env);

    wasm_runtime_module_free(module_inst, app_offset);
    return atom_ok;
}

static ERL_NIF_TERM ret_binary_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint32_t app_offset, size;
    ERL_NIF_TERM bin_term;
    void *src, *dst;

    if (!enif_get_uint(env, argv[0], &app_offset)
        || !enif_get_uint(env, argv[1], &size)
        || !wasm_runtime_validate_app_addr(module_inst, app_offset, size))
        return enif_make_badarg(env);

    src = wasm_runtime_addr_app_to_native(module_inst, app_offset);
    dst = enif_make_new_binary(env, size, &bin_term);
    memcpy(dst, src, size);
    return bin_term;
}

static ERL_NIF_TERM new_module_inst_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const uint32_t stack_size = 8092, heap_size = 8092;
    char error_buf[100];
    wasm_module_inst_t mi =
        wasm_runtime_instantiate(module, stack_size, heap_size,
                                 error_buf, sizeof(error_buf));
    if (!mi)
        raise_exception(env, error_buf);
    module_inst = mi;
    return atom_ok;
}

static ERL_NIF_TERM new_exec_env_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const uint32_t stack_size = 8092;
    char error_buf[100];
    wasm_exec_env_t ee = wasm_runtime_create_exec_env(module_inst, stack_size);
    if (!ee)
        raise_exception(env, error_buf);
    exec_env = ee;
    return atom_ok;
}


static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello_nif},
    {"apply", 2, apply_nif},
    {"print_func", 1, print_func_nif},
    {"arg_binary_alloc", 1, arg_binary_alloc_nif},
    {"arg_binary_free", 1, arg_binary_free_nif},
    {"ret_binary", 2, ret_binary_nif},
    {"new_module_inst", 0, new_module_inst_nif},
    {"new_exec_env", 0, new_exec_env_nif}
};

ERL_NIF_INIT(tester_nif,nif_funcs,load,NULL,NULL,NULL)

