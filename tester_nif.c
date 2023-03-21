#include <erl_nif.h>

#include "tester.h"


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return tester_init();
}

static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tester_run();
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello}
};

ERL_NIF_INIT(tester_nif,nif_funcs,load,NULL,NULL,NULL)

