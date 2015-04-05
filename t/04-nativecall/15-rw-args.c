#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT void SetChar(signed char *chr) {
    *chr = 97;
}

DLLEXPORT void SetShort(short *sht) {
    *sht = 387;
}

DLLEXPORT void SetLong(long *lng) {
    *lng = 777;
}

DLLEXPORT void SetLongLong(long long *llg) {
    *llg = 15324;
}

DLLEXPORT void SetFloat(float *flt) {
    *flt = 6.66;
}

DLLEXPORT void SetDouble(double *dbl) {
    *dbl = 12.12;
}

DLLEXPORT void SetUChar(unsigned char *chr) {
    *chr = 153;
}

DLLEXPORT void SetUShort(unsigned short *sht) {
    *sht = 387;
}

DLLEXPORT void SetULong(unsigned long *lng) {
    *lng = 777;
}

DLLEXPORT void SetULongLong(unsigned long long *llg) {
    *llg = 15324;
}
