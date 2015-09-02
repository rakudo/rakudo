#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
typedef signed __int64 int64_t;
#else
#define DLLEXPORT extern
#include <inttypes.h>
#endif

DLLEXPORT int ReturnInt()
{
    return 101;
}

DLLEXPORT short ReturnShort()
{
    return 102;
}

DLLEXPORT signed char ReturnByte()
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

DLLEXPORT int64_t ReturnInt64()
{
    return 0xFFFFFFFFFF;
}

DLLEXPORT unsigned char ReturnUint8()
{
    return 0xFE;
}

DLLEXPORT unsigned short ReturnUint16()
{
    return 0xFFFE;
}

DLLEXPORT unsigned int ReturnUint32()
{
    return 0xFFFFFFFE;
}
