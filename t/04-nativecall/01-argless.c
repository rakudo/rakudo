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

DLLEXPORT int long_and_complicated_name()
{
    return 3;
}
