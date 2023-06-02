// test.c
#include <stdint.h>

#include "erl_nif_wasm.h"

/*
 * Functions that do not use the erl_nif_wasm.h interface.
 * Can be called with wasm_runtime_nif:call_raw/3.
 */

int32_t inc_static_data()
{
  static int32_t local_static = 0;

  return ++local_static;
}

int32_t add_I32(int32_t first, int32_t second)
{
  return first + second;
}


int64_t add_I64(int64_t first, int64_t second)
{
  return first + second;
}

double add_F64(double first, double second)
{
  return first + second;
}

/*
 * Functions that use the erl_nif_wasm.h interface
 * to read and create Erlang terms.
 * Can be called with wasm_runtime_nif:call/3
 */

ERL_NIF_TERM add_terms(ErlNifEnv env, ERL_NIF_TERM arg1, ERL_NIF_TERM arg2)
{
  int32_t a1, a2;

  if (!enif_wasm_get_int32(env, arg1, &a1) ||
      !enif_wasm_get_int32(env, arg2, &a2))
      return enif_wasm_make_badarg(env);

  return enif_wasm_make_int32(env, a1+a2);
}

ERL_NIF_TERM add_list_terms(ErlNifEnv env, ERL_NIF_TERM list)
{
  ERL_NIF_TERM head, tail;
  int32_t value, sum = 0;

  for (head = list; !enif_wasm_is_empty_list(env, head); head = tail) {
    if (!enif_wasm_get_list_cell(env, head, &head, &tail))
      return enif_wasm_make_badarg(env);

    if (!enif_wasm_get_int32(env, head, &value))
      return enif_wasm_make_badarg(env);

    sum += value;
  }
  return enif_wasm_make_int32(env, sum);
}

ERL_NIF_TERM binary_reverse(ErlNifEnv env, ERL_NIF_TERM binary)
{
    unsigned char buf[100];
    uint32_t size, i;

    if (!enif_wasm_get_binary_size(env, binary, &size)
        || size > sizeof(buf))
        return enif_wasm_make_badarg(env);

    if (!enif_wasm_get_binary_bytes(env, binary, 0, size, buf))
      return enif_wasm_make_badarg(env);

    for (i = 0; i < size/2; i++) {
        const unsigned char tmp = buf[i];
        buf[i] = buf[size-1-i];
        buf[size-1-i] = tmp;
    }
    return enif_wasm_make_binary(env, buf, size);
}

