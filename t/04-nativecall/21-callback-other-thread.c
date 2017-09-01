#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

static int (*the_cb)(int);

DLLEXPORT void SetCallback(int (*cb)(int)) {
    the_cb = cb;
}

DLLEXPORT int CallCallback(int a) {
    return 3 * the_cb(a);
}
