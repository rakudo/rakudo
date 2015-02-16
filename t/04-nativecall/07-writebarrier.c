#include <stdlib.h>

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

typedef struct {
    long *ptr;
} Structy;

static Structy *saved = NULL;

DLLEXPORT long _deref(long *ptr) {
    return *ptr;
}

DLLEXPORT long *make_ptr() {
    long *ptr = (long *) malloc(sizeof(long));
    *ptr = 32;
    return ptr;
}

DLLEXPORT void struct_twiddle(Structy *s) {
    s->ptr = (long *) malloc(sizeof(long));
    *(s->ptr) = 9;
}

DLLEXPORT void array_twiddle(long **arr) {
    arr[0] = (long *) malloc(sizeof(long));
    arr[1] = (long *) malloc(sizeof(long));
    arr[2] = (long *) malloc(sizeof(long));

    *arr[0] = 1;
    *arr[1] = 2;
    *arr[2] = 3;
}

DLLEXPORT void dummy(void **arr) {
    /* dummy */
}

DLLEXPORT void save_ref(Structy *s) {
    saved = s;
}

DLLEXPORT void atadistance(void) {
    saved->ptr = (long *) malloc(sizeof(long));
    *(saved->ptr) = 42;
}
