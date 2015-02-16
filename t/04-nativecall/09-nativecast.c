#include <stdio.h>
#include <stdlib.h>

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

typedef struct cute_struct {
    int i;
} CUTE;

DLLEXPORT CUTE * ReturnStruct()
{
    CUTE *i = malloc(sizeof(CUTE));
    i->i = 100;
    return i;
}

DLLEXPORT int * ReturnArray()
{
    int *i = malloc(3 * sizeof(int));
    i[0] = 1;
    i[1] = 2;
    i[2] = 3;
    return i;
}

DLLEXPORT int * ReturnInt()
{
    int *i = malloc(sizeof(int));
    *i = 101;
    return i;
}

DLLEXPORT short * ReturnShort()
{
    short *i = malloc(sizeof(short));
    *i = 102;
    return i;
}

DLLEXPORT char * ReturnByte()
{
    char *i = malloc(sizeof(char));
    *i = -103;
    return i;
}

DLLEXPORT double * ReturnDouble()
{
    double *i = malloc(sizeof(double));
    *i = 99.9;
    return i;
}

DLLEXPORT float * ReturnFloat()
{
    float *i = malloc(sizeof(float));
    *i = -4.5;
    return i;
}

DLLEXPORT char * ReturnString()
{
    return "epic cuteness";
}

DLLEXPORT char * ReturnNullString()
{
    return NULL;
}
