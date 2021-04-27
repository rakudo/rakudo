#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#include <BaseTsd.h>
typedef SSIZE_T ssize_t;
typedef signed __int64 int64_t;
#else
#define DLLEXPORT extern
#include <inttypes.h>
#include <sys/types.h>
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

DLLEXPORT int TakeInt64(int64_t x)
{
    if (x == 0xFFFFFFFFFF)
        return 9;
    return 0;
}

DLLEXPORT int TakeUint8(unsigned char x)
{
    if (x == 0xFE)
        return 10;
    return 0;
}

DLLEXPORT int TakeUint16(unsigned short x)
{
    if (x == 0xFFFE)
        return 11;
    return 0;
}

DLLEXPORT int TakeUint32(unsigned int x)
{
    if (x == 0xFFFFFFFE)
        return 12;
    return 0;
}

DLLEXPORT int TakeSizeT(size_t x)
{
    if (x == 42)
        return 13;
    return 0;
}

DLLEXPORT int TakeSSizeT(ssize_t x)
{
    if (x == -42)
        return 14;
    return 0;
}
