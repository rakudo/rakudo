#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

typedef struct {
    char *str;
    long ival;
} Struct;

DLLEXPORT void TakeACallback(void (*cb)(void)) {
    cb();
}

DLLEXPORT void OptionallyTakeACallback(void (*cb)(void)) {
       if ( cb )
        cb();
}

DLLEXPORT void TakeIntCallback(void (*cb)(int)) {
    cb(17);
}

DLLEXPORT void TakeStringCallback(void (*cb)(char *)) {
    cb("lorem ipsum");
}

DLLEXPORT void TakeStructCallback(void (*cb)(Struct *)) {
    Struct *s = (Struct *) malloc(sizeof(Struct));
    s->str = "foobar";
    s->ival = -42;
    cb(s);
}

DLLEXPORT int CheckReturnsFloat(double (*cb)()) {
    double num = cb();
    if(1.23 - num > 0.001) return 1;
    return 6;
}

DLLEXPORT int CheckReturnsStr(char *(*cb)()) {
    char *str = cb();
    if(strcmp(str, "Herps and derps")) return 1;
    return 7;
}

DLLEXPORT int CheckReturnsStruct(Struct *(*cb)()) {
    Struct *s = cb();
    if(s->ival != 314) return 1;
    if(strcmp(s->str, "Tweedledum, tweedledee")) return 2;
    return 8;
}

static int pass = 0;
DLLEXPORT int CheckChangingCallback(int (*cb)()) {
    return cb() - pass++;
}
