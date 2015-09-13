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
    double numval;
    char byteval;
    float  floatval;
    long *arr;
} MyStruct;

typedef struct { long   first, second; } IntStruct;
typedef struct { double first, second; } NumStruct;
typedef struct {
    IntStruct *a;
    NumStruct *b;
} StructStruct;

typedef struct {
    long *p;
} PointerStruct;

typedef struct {
    char *first;
    char *second;
} StringStruct;

typedef struct {
    IntStruct a;
    int i;
} StructIntStruct;

DLLEXPORT MyStruct *ReturnAStruct()
{
    MyStruct *obj = (MyStruct *) malloc(sizeof(MyStruct));
    obj->intval = 17;
    obj->numval = 4.2;
    obj->byteval = 13;
    obj->floatval = -6.28;
    obj->arr = (long *) malloc(3*sizeof(long));
    obj->arr[0] = 2;
    obj->arr[1] = 3;
    obj->arr[2] = 5;

    return obj;
}

DLLEXPORT int TakeAStruct(MyStruct *obj)
{
    if(obj->intval  != 42)            return 1;
    if(obj->numval  != -3.7)          return 2;
    if(obj->byteval != 7)             return 3;
    if(!obj->arr || obj->arr[0] != 1) return 4;
    if(!obj->arr || obj->arr[1] != 2) return 5;
    return 11;
}

DLLEXPORT StructStruct *ReturnAStructStruct() {
    StructStruct *ss = (StructStruct *) malloc(sizeof(StructStruct));
    ss->a = (IntStruct *) malloc(sizeof(IntStruct));
    ss->b = (NumStruct *) malloc(sizeof(NumStruct));
    ss->a->first  = 7;
    ss->a->second = 11;
    ss->b->first  = 3.7;
    ss->b->second = 0.1;

    return ss;
}

DLLEXPORT int TakeAStructStruct(StructStruct *obj) {
    if(!obj->a || obj->a->first  != 13)   return 1;
    if(!obj->a || obj->a->second != 17)   return 2;
    if(!obj->b || obj->b->first  != 0.9)  return 3;
    if(!obj->b || obj->b->second != 3.14) return 4;
    return 22;
}

DLLEXPORT PointerStruct *ReturnAPointerStruct() {
    PointerStruct *obj = (PointerStruct *) malloc(sizeof(PointerStruct));
    obj->p = (long *) malloc(sizeof(long));
    *(obj->p) = 19;

    return obj;
}

DLLEXPORT StringStruct *ReturnAStringStruct() {
    StringStruct *obj = (StringStruct *) malloc(sizeof(StringStruct));
    obj->first = "OMG!";
    obj->second = "Strings!";

    return obj;
}

DLLEXPORT int TakeAStringStruct(StringStruct *obj) {
    if(strcmp(obj->first,  "Lorem")) return 1;
    if(strcmp(obj->second, "ipsum")) return 2;
    return 33;
}

DLLEXPORT long _deref(long *ptr) {
    return *ptr;
}

DLLEXPORT StructIntStruct *ReturnAStructIntStruct() {
    StructIntStruct *sis = (StructIntStruct *) malloc(sizeof(StructIntStruct));
    sis->a.first = 101;
    sis->a.second = 77;
    sis->i = 42;

    return sis;
}
