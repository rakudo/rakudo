#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

#include <string.h>

DLLEXPORT int NCstrlen(const char *x)
{
    return strlen(x);
}
