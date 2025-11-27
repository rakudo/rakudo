#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>

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

DLLEXPORT int va1(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    int a1 = va_arg(args, int);
    va_end(args);
    return a0 == 0 && a1 == 1;
}

DLLEXPORT int va2(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    int a1 = va_arg(args, int);
    int a2 = va_arg(args, int);
    int a3 = va_arg(args, int);
    long a4 = va_arg(args, long);
    unsigned int a5 = va_arg(args, unsigned int);
    unsigned int a6 = va_arg(args, unsigned int);
    unsigned int a7 = va_arg(args, unsigned int);
    unsigned long a8 = va_arg(args, unsigned long);
    va_end(args);
    return a0 == 0 &&
        a1 == -1 &&
        a2 == -1 * 1<<10 &&
        a3 == -1 * 1<<18 &&
        a4 == -1 * 1l<<34 &&
        a5 == 1 &&
        a6 == 1<<10 &&
        a7 == 1<<18 &&
        a8 == 1ul<<34;
}

DLLEXPORT int va3(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    signed char *a1 = va_arg(args, signed char*);
    short *a2 = va_arg(args, short*);
    int *a3 = va_arg(args, int*);
    long *a4 = va_arg(args, long*);
    unsigned char *a5 = va_arg(args, unsigned char*);
    unsigned short *a6 = va_arg(args, unsigned short*);
    unsigned int *a7 = va_arg(args, unsigned int*);
    unsigned long *a8 = va_arg(args, unsigned long*);
    va_end(args);
    return a0 == 0 &&
        *a1 == -1 &&
        *a2 == -1 * 1<<10 &&
        *a3 == -1 * 1<<18 &&
        *a4 == -1 * 1l<<34 &&
        *a5 == 1 &&
        *a6 == 1<<10 &&
        *a7 == 1<<18 &&
        *a8 == 1ul<<34;
}

DLLEXPORT int va4(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    int* a1 = va_arg(args, int*);
    va_end(args);
    int ok = a0 == 0 &&
        *a1 == 1;
    *a1 = 2;
    return ok;
}

DLLEXPORT int va5(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    char* a1 = va_arg(args, char*);
    va_end(args);
    return a0 == 0 &&
        strcmp(a1, "Köln") == 0;
}

DLLEXPORT int va6(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    char* a1 = va_arg(args, char*);
    va_end(args);
    return a0 == 0 &&
         a1[0] == 0x0 && // b
         a1[1] == 0x62 &&
         a1[2] == 0x0 && // o
         a1[3] == 0x6f &&
         a1[4] == 0x0 &&
         a1[5] == 0x0;
}

struct S {
    long s0;
    double s1;
};

DLLEXPORT int va7(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    struct S* a1 = va_arg(args, struct S*);
    va_end(args);
    return a0 == 0 &&
        a1->s0 == 1 &&
        fabs(a1->s1 - 2.5) < 0.000001;
}

union U {
    long u0;
    double u1;
};

DLLEXPORT int va8(int a0, ...)
{
    va_list args;
    va_start(args, a0);
    union U* a1 = va_arg(args, union U*);
    va_end(args);
    return a0 == 0 &&
        a1->u0 == 1;
}
