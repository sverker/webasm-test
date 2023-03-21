// add.c

int enif_printf_I(const char* fmt, int arg1);

int add (int first, int second)
{
  int ret1 = enif_printf_I("first  = %x\n", first);
  int ret2 = enif_printf_I("second = %x\n", second);
  enif_printf_I("ret1 = %x\n", ret1);
  enif_printf_I("ret2 = %x\n", ret2);
  return first + second;
}
