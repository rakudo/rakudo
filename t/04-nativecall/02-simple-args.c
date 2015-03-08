#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT int TakeInt(int x)
{
    if (x == 42)
        return 1;
    return 0;
}

DLLEXPORT int TakeTwoShorts(short x, short y)
{
    if (x == 10 && y == 20)
        return 2;
    return 0;
}

DLLEXPORT int AssortedIntArgs(int x, short y, char z)
{
    if (x == 101 && y == 102 && z == 103)
        return 3;
    return 0;
}

DLLEXPORT int TakeADouble(double x)
{
    if (-6.9 - x < 0.001)
        return 4;
    return 0;
}

DLLEXPORT int TakeAFloat(float x)
{
    if (4.2 - x < 0.001)
        return 5;
    return 0;
}

DLLEXPORT int TakeAString(char *pass_msg)
{
    if (0 == strcmp(pass_msg, "ok 6 - passed a string"))
        return 6;
    return 0;
}

static char *cached_str = NULL;
DLLEXPORT void SetString(char *str) {
    cached_str = str;
}

DLLEXPORT int CheckString() {
    if (0 == strcmp(cached_str, "ok 7 - checked previously passed string"))
        return 7;
    return 0;
}

DLLEXPORT int wrapped(int n) {
    if (n == 42)
        return 8;
    return 0;
}
