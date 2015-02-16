#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#define long __int64
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

DLLEXPORT void CheckReturnsFloat(double (*cb)()) {
    double num = cb();
    if(num != 1.23) printf("not ");
    printf("    ok - num callback return value\n");
}

DLLEXPORT void CheckReturnsStr(char *(*cb)()) {
    char *str = cb();
    if(strcmp(str, "Herps and derps")) printf("not ");
    printf("    ok - string callback return value\n");
}

DLLEXPORT void CheckReturnsStruct(Struct *(*cb)()) {
    Struct *s = cb();
    if(s->ival != 314) printf("not ");
    printf("    ok - struct (intval) callback return value\n");
    if(strcmp(s->str, "Tweedledum, tweedledee")) printf("not ");
    printf("    ok - struct (string) callback return value\n");
}
