// add.c
#include <stdint.h>

#include "erl_nif_wasm.h"


static int32_t global_static = 100;
static int32_t global_extern = 200;

void inc_data()
{
  static int32_t local_static = 300;
  global_static++;
  global_extern++;
  local_static++;
  enif_printf_I("\nglobal_static = %d", global_static);
  enif_printf_I("\nglobal_extern = %d", global_extern);
  enif_printf_I("\nlocal_static = %d\n", local_static);
}

int32_t add (int32_t first, int32_t second)
{
  int32_t ret1 = enif_printf_I("add: first  = %x\n", first);
  int32_t ret2 = enif_printf_I("add: second = %x\n", second);
  enif_printf_I("add: sizeof(long) = %d\n", sizeof(long));
  enif_printf_I("add: ret1 = %x\n", ret1);
  enif_printf_I("add: ret2 = %x\n", ret2);
  return first + second;
}


int64_t addL (int64_t first, int64_t second)
{
  int64_t ret1 = enif_printf_L("addL: first  = %lx\n", first);
  int64_t ret2 = enif_printf_L("addL: second = %lx\n", second);
  enif_printf_L("addL: ret1 = %lx\n", ret1);
  enif_printf_L("addL: ret2 = %lx\n", ret2);
  return first + second;
}

double addF (double first, double second)
{
  enif_printf_F("addF: first = %f\n", first);
  enif_printf_F("addF: second = %f\n", second);
  return first + second;
}

int32_t dirty_read(int64_t native_address)
{
  int32_t *ptr = (int32_t*)native_address;
  int32_t value;

  enif_printf_L("dirty_read: native_address=%lx\n", native_address);
  value = *ptr;
  enif_printf_L("dirty_read: value=%x\n", value);
  return value;
}


char* buffy(char* buf, int32_t size)
{
  int i;
  for (i=0; i < size; i++)
    buf[i] = ~buf[i];
  return buf;
}

void segv(void)
{
  *(int*)17 = 42;
}


ERL_NIF_TERM my_nif(ErlNifEnv env, ERL_NIF_TERM arg1, ERL_NIF_TERM arg2)
{
  int32_t a1 = -666;
  int32_t a2 = -777;

  if (!enif_wasm_get_int32(env, arg1, &a1) ||
      !enif_wasm_get_int32(env, arg2, &a2))
      return enif_wasm_make_badarg(env);

  return enif_wasm_make_int32(env, a1+a2);
}

ERL_NIF_TERM add_list(ErlNifEnv env, ERL_NIF_TERM list)
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
