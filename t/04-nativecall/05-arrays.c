#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT double * ReturnADoubleArray()
{
    double *foo = malloc(3 * sizeof(double));
    foo[0] = 23.45;
    foo[1] = -99.87;
    foo[2] = 0.25;
    return foo;
}

DLLEXPORT double TakeADoubleArrayAndAddElements(double *arr)
{
    return arr[0] + arr[1];
}

DLLEXPORT char ** ReturnAStringArray()
{
    char **foo = malloc(2 * sizeof(char *));
    foo[0] = "La Trappe";
    foo[1] = "Leffe";
    return foo;
}

DLLEXPORT int TakeAStringArrayAndReturnTotalLength(char **arr)
{
    return (int)(strlen(arr[0]) + strlen(arr[1]));
}

typedef struct {
    long value;
} Struct;

DLLEXPORT Struct **ReturnAStructArray() {
    Struct **arr = (Struct **) malloc(3*sizeof(Struct *));
    arr[0] = malloc(sizeof(Struct));
    arr[1] = malloc(sizeof(Struct));
    arr[2] = malloc(sizeof(Struct));

    arr[0]->value = 2;
    arr[1]->value = 3;
    arr[2]->value = 5;

    return arr;
}

DLLEXPORT int TakeAStructArray(Struct **structs) {
    if(structs[0]->value == 7 && structs[1]->value == 11 && structs[2]->value == 13)
        return 14;
    return 0;
}

DLLEXPORT char *ReturnsAByteArray() {
    char *arr = malloc(3*sizeof(char));
    arr[0] = 100;
    arr[1] = 90;
    arr[2] = 80;
    return arr;
}

DLLEXPORT int TakeAByteArray(char *bytes) {
    if(bytes[0] == 31 && bytes[1] == 28 && bytes[2] == 30)
        return 18;
    return 0;
}

DLLEXPORT float *ReturnsAFloatArray() {
    float *arr = malloc(3*sizeof(float));
    arr[0] = 1.23;
    arr[1] = 4.56;
    arr[2] = 7.89;
    return arr;
}

DLLEXPORT float SumAFloatArray(float *floats) {
    return floats[0] + floats[1];
}

DLLEXPORT int TakeAStructArrayWithANull(Struct **structs) {
    return structs[1] == NULL;
}
