#include <erl_nif.h>
#include <wasm_export.h>

#include "tester.h"

static ERL_NIF_TERM atom_ok;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
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
    uint32_t n_args;
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

    if (wasm_func_get_result_count(func, module_inst) != 1)
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

    if (!tester_call_func(func, n_args, args, &result, &error)) {
        return raise_exception(env, error);
    }
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

static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello_nif},
    {"apply", 2, apply_nif},
    {"print_func", 1, print_func_nif}
};

ERL_NIF_INIT(tester_nif,nif_funcs,load,NULL,NULL,NULL)

