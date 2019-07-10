#include <stdio.h>
#include <string.h>
#include <wchar.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT int GlobalInt;
int GlobalInt = 101;

DLLEXPORT short GlobalShort;
short GlobalShort = 102;

DLLEXPORT char GlobalByte;
char GlobalByte = -103;

DLLEXPORT double GlobalDouble;
double GlobalDouble = 99.9;

DLLEXPORT float GlobalFloat;
float GlobalFloat = (float)-4.5;

DLLEXPORT char * GlobalString;
char * GlobalString = "epic cuteness";

DLLEXPORT char * GlobalNullString;
char * GlobalNullString = NULL;

DLLEXPORT wchar_t * GlobalWideString;
wchar_t * GlobalWideString = L"epic cuteness";

DLLEXPORT wchar_t * GlobalNullWideString;
wchar_t * GlobalNullWideString = NULL;
