// add.c

int enif_printf(const char* fmt, ...);

int add (int first, int second)
{
  //enif_printf("first = %d\n", first);
  return first + second;
}
