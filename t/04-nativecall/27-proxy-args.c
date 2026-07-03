#include <stdint.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT int32_t TakePointerReturnInt(void *p) {
    return 42;
}

DLLEXPORT double TakePointerReturnDouble(void *p) {
    return 1.5;
}

DLLEXPORT const char * TakePointerReturnString(void *p) {
    return "hi";
}
