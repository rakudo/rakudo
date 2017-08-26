#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT void Nothing()
{
    /* we don't even print something */
}

DLLEXPORT int Argless()
{
    return 2;
}

DLLEXPORT char ArglessChar()
{
    return 2;
}

DLLEXPORT long long ArglessLongLong()
{
    return 2;
}

int my_int = 2;
DLLEXPORT int* ArglessPointer()
{
    return &my_int;
}

DLLEXPORT int long_and_complicated_name()
{
    return 3;
}
