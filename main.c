#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>

#include "tester.h"

int main()
{
    tester_init();
    tester_run();
}

int enif_fprintf(FILE* stream, const char* fmt, ...)
{
    int ret = 0;
    va_list ap;
    va_start(ap, fmt);
    ret = vfprintf(stream,fmt, ap);
    va_end(ap);
    return ret;
}

