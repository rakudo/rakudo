#include <stdio.h>

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT void TakeInt(int x)
{
    if (x == 42)
        printf("ok 1 - got passed int 42\n");
    else
        printf("not ok 1 - got passed int 42\n");
    fflush(stdout);
}

DLLEXPORT void TakeTwoShorts(short x, short y)
{
    if (x == 10)
        printf("ok 2 - got passed short 10\n");
    else
        printf("not ok 2 - got passed short 10\n");
    if (y == 20)
        printf("ok 3 - got passed short 20\n");
    else
        printf("not ok 3 - got passed short 20\n");
    fflush(stdout);
}

DLLEXPORT void AssortedIntArgs(int x, short y, char z)
{
    if (x == 101)
        printf("ok 4 - got passed int 101\n");
    else
        printf("not ok 4 - got passed int 101\n");
    if (y == 102)
        printf("ok 5 - got passed short 102\n");
    else
        printf("not ok 5 - got passed short 102\n");
    if (z == 103)
        printf("ok 6 - got passed char 103\n");
    else
        printf("not ok 6 - got passed char 103\n");
    fflush(stdout);
}

DLLEXPORT void TakeADouble(double x)
{
    if (-6.9 - x < 0.001)
        printf("ok 7 - got passed double -6.9\n");
    else
        printf("not ok 7 - got passed double -6.9\n");
    fflush(stdout);
}

DLLEXPORT void TakeAFloat(float x)
{
    if (4.2 - x < 0.001)
        printf("ok 8 - got passed float 4.2\n");
    else
        printf("not ok 8 - got passed float 4.2\n");
    fflush(stdout);
}

DLLEXPORT void TakeAString(char *pass_msg)
{
    printf("%s\n", pass_msg);
    fflush(stdout);
}

static char *cached_str = NULL;
DLLEXPORT void SetString(char *str) {
    cached_str = str;
}

DLLEXPORT void PrintString() {
    printf("%s\n", cached_str);
    fflush(stdout);
}

DLLEXPORT void wrapped(int n) {
    printf("ok 11 - wrapped sub\n");
    fflush(stdout);
}
