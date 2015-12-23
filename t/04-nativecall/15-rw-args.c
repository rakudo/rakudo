#include <stdlib.h>
#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT void SetChar(signed char *chr) {
    *chr = 97;
}

DLLEXPORT signed char PassChar(signed char *chr) {
    return *chr;
}

DLLEXPORT void SetShort(short *sht) {
    *sht = 387;
}

DLLEXPORT short PassShort(short *sht) {
    return *sht;
}

DLLEXPORT void SetLong(long *lng) {
    *lng = 777;
}

DLLEXPORT long PassLong(long *lng) {
    return *lng;
}

DLLEXPORT void SetLongLong(long long *llg) {
    *llg = 15324;
}

DLLEXPORT long long PassLongLong(long long *llg) {
    return *llg;
}

DLLEXPORT void SetFloat(float *flt) {
    *flt = 6.66;
}

DLLEXPORT float PassFloat(float *flt) {
    return *flt;
}

DLLEXPORT void SetDouble(double *dbl) {
    *dbl = 12.12;
}

DLLEXPORT double PassDouble(double *dbl) {
    return *dbl;
}

DLLEXPORT void SetUChar(unsigned char *chr) {
    *chr = 153;
}

DLLEXPORT unsigned char PassUChar(unsigned char *chr) {
    return *chr;
}

DLLEXPORT void SetUShort(unsigned short *sht) {
    *sht = 387;
}

DLLEXPORT unsigned short PassUShort(unsigned short *sht) {
    return *sht;
}

DLLEXPORT void SetULong(unsigned long *lng) {
    *lng = 777;
}

DLLEXPORT unsigned long PassULong(unsigned long *lng) {
    return *lng;
}

DLLEXPORT void SetULongLong(unsigned long long *llg) {
    *llg = 15324;
}

DLLEXPORT unsigned long long PassULongLong(unsigned long long *llg) {
    return *llg;
}

DLLEXPORT int SetPtrToPtr(int **ptr) {
    if (ptr == NULL) {
        return 0;
    }

    *ptr = (int *)42;
    return 1;
}
