#include <stdlib.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

class DLLEXPORT Foo {
public:
    Foo();
    ~Foo();
    virtual int TakeAVoid(void);
    virtual int TakeABool(bool i);
    virtual int TakeAChar(char i);
    virtual int TakeAShort(short i);
    virtual int TakeAnInt(int i);
    virtual int TakeALong(long i);
    virtual int TakeALongLong(long long i);
    virtual int TakeAFloat(float i);
    virtual int TakeADouble(double i);
    virtual int TakeAString(char *i);
    virtual int TakeAnArray(int i[]);
    virtual int TakeAPointer(void *i);
    virtual int TakeABoolPointer(bool *i);
    virtual int TakeACharPointer(char *i);
    virtual int TakeAShortPointer(short *i);
    virtual int TakeAnIntPointer(int *i);
    virtual int TakeALongPointer(long *i);
    virtual int TakeALongLongPointer(long long *i);
    virtual int TakeAFloatPointer(float *i);
    virtual int TakeADoublePointer(double *i);
    virtual int TakeAUInt(uint i);
    virtual int TakeAUShort(ushort i);
    virtual int TakeAUChar(unsigned char i);
    virtual int TakeAInt64(long long i);
    virtual int TakeAULongLong(unsigned long long i);
    virtual int TakeAUInt64(unsigned long long i);
};

Foo::Foo()  { };
Foo::~Foo() { };

int Foo::TakeAVoid(void)                    { return  0; }
int Foo::TakeABool(bool i)                  { return  1; }
int Foo::TakeAChar(char i)                  { return  2; }
int Foo::TakeAShort(short i)                { return  3; }
int Foo::TakeAnInt(int i)                   { return  4; }
int Foo::TakeALong(long i)                  { return  5; }
int Foo::TakeALongLong(long long i)         { return  6; }
int Foo::TakeAFloat(float i)                { return  7; }
int Foo::TakeADouble(double i)              { return  8; }
int Foo::TakeAString(char *i)               { return  9; }
int Foo::TakeAnArray(int i[])               { return 10; }
int Foo::TakeAPointer(void *i)              { return 11; }
int Foo::TakeABoolPointer(bool *i)          { return 12; }
int Foo::TakeACharPointer(char *i)          { return 13; }
int Foo::TakeAShortPointer(short *i)        { return 14; }
int Foo::TakeAnIntPointer(int *i)           { return 15; }
int Foo::TakeALongPointer(long *i)          { return 16; }
int Foo::TakeALongLongPointer(long long *i) { return 17; }
int Foo::TakeAFloatPointer(float *i)        { return 18; }
int Foo::TakeADoublePointer(double *i)      { return 19; }
int Foo::TakeAUInt(uint i)                  { return 20; }
int Foo::TakeAUShort(ushort i)              { return 21; }
int Foo::TakeAUChar(unsigned char i)        { return 22; }
int Foo::TakeAInt64(long long i)            { return 23; }
int Foo::TakeAULongLong(unsigned long long i) { return 24; }
int Foo::TakeAUInt64(unsigned long long i)    { return 25; }
