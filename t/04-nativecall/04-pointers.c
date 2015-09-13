#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT void * ReturnSomePointer()
{
    return strdup("Got passed back the pointer I returned");
}

DLLEXPORT int CompareSomePointer(void *ptr)
{
    int x = strcmp("Got passed back the pointer I returned", ptr) == 0;
    free(ptr);
    return x;
}

DLLEXPORT void * ReturnNullPointer()
{
    return NULL;
}

DLLEXPORT void * TakeTwoPointersToInt(int *ptr1, int *ptr2)
{
    return NULL;
}

DLLEXPORT void * TakeCArrayToInt8(int array[]) {
    return NULL;
}
