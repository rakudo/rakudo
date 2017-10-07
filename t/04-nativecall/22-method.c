#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

typedef struct {
    long intval;
} MyStruct;

DLLEXPORT MyStruct *ReturnAStruct(long intval)
{
    MyStruct *obj = (MyStruct *) malloc(sizeof(MyStruct));
    obj->intval = intval;
    return obj;
}

DLLEXPORT long Add(MyStruct *obj, long intval)
{
	return obj->intval + intval;
}
