// add.c
#include <stdint.h>

#include "erl_nif_wasm.h"


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


ERL_NIF_TERM add_terms(ErlNifEnv env, ERL_NIF_TERM arg1, ERL_NIF_TERM arg2)
{
  int32_t a1 = -666;
  int32_t a2 = -777;

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


