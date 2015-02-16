#include <stdio.h>

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT int ReturnInt()
{
    return 101;
}

DLLEXPORT short ReturnShort()
{
    return 102;
}

DLLEXPORT char ReturnByte()
{
    return -103;
}

DLLEXPORT double ReturnDouble()
{
    return 99.9;
}

DLLEXPORT float ReturnFloat()
{
    return (float)-4.5;
}

DLLEXPORT char * ReturnString()
{
    return "epic cuteness";
}

DLLEXPORT char * ReturnNullString()
{
    return NULL;
}
