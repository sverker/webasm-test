// add.c
#include <stdint.h>

int32_t enif_printf_I(const char* fmt, int32_t arg1);
int64_t enif_printf_L(const char* fmt, int64_t arg1);
int32_t enif_printf_F(const char* fmt, double arg1);

static int32_t global_static = 100;
static int32_t global_extern = 200;

void inc_data()
{
  static int32_t local_static = 300;
  global_static++;
  global_extern++;
  local_static++;
  enif_printf_I("global_static = %d\n", global_static);
  enif_printf_I("global_extern = %d\n", global_extern);
  enif_printf_I("local_static = %d\n", local_static);
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
