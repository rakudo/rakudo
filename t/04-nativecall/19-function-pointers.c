#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
typedef signed __int64 int64_t;
#else
#define DLLEXPORT extern
#include <inttypes.h>
#endif

DLLEXPORT int ForeignFunction()
{
    return 42;
}

DLLEXPORT void * ReturnFunctionPointer()
{
    return (void *)&ForeignFunction;
}
