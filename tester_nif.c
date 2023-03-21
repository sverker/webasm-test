#include <erl_nif.h>
#include <wasm_export.h>

#include "tester.h"


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return tester_init();
}

static ERL_NIF_TERM hello_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tester_run();
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM apply_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char func_name[20];
    uint32_t n_args = 1;
    wasm_val_t args[10];
    wasm_val_t result;
    ERL_NIF_TERM head, tail;
    const char* error;
    uint32_t i;

    if (!enif_get_atom(env, argv[0], func_name, sizeof(func_name), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    i = 0;
    for (head = argv[1]; !enif_is_empty_list(env, head); head = tail, i++) {
        if (!enif_get_list_cell(env, head, &head, &tail)
            || i >= sizeof(args) / sizeof(args[0]))
            return enif_make_badarg(env);

        if (enif_get_int(env, head, &args[i].of.i32))
            args[i].kind = WASM_I32;
        else if (enif_get_int64(env, head, &args[i].of.i64))
            args[i].kind = WASM_I64;
        else if (enif_get_double(env, head, &args[i].of.f64))
            args[i].kind = WASM_F64;
        else
            return enif_make_badarg(env);
    }

    if (!tester_call_func(func_name, i, args, &result, &error)) {
        return enif_raise_exception(env, enif_make_string(env, error,
                                                          ERL_NIF_LATIN1));
    }
    switch (result.kind) {
    case WASM_I32:
        return enif_make_int(env, result.of.i32);
    case WASM_I64:
        return enif_make_int64(env, result.of.i64);
    case WASM_F64:
        return enif_make_double(env, result.of.f64);
    }
    return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello_nif},
    {"apply", 2, apply_nif}
};

ERL_NIF_INIT(tester_nif,nif_funcs,load,NULL,NULL,NULL)

