#include <stdio.h>

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT void * ReturnSomePointer()
{
    char *x = "Got passed back the pointer I returned";
    return x;
}

DLLEXPORT int CompareSomePointer(void *ptr)
{
    int x = strcmp("Got passed back the pointer I returned", ptr) == 0;
    return x;
}
