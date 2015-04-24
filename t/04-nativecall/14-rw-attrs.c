#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

typedef struct {
    long   longval;
    double doubleval;
    char   charval;
    float  floatval;
} MyStruct;


DLLEXPORT void InitStruct(MyStruct *obj) {
    obj->longval   = 1;
    obj->doubleval = 2.22;
    obj->charval   = 3;
    obj->floatval  = 4.44;
}

DLLEXPORT int GetLongOfStruct(MyStruct *obj) {
    return obj->longval;
}

DLLEXPORT double GetDoubleOfStruct(MyStruct *obj) {
    return obj->doubleval;
}

DLLEXPORT char GetCharOfStruct(MyStruct *obj) {
    return obj->charval;
}

DLLEXPORT float GetFloatOfStruct(MyStruct *obj) {
    return obj->floatval;
}
