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
    virtual int TakeAVoid(void)                    { return  0; }
    virtual int TakeABool(bool i)                  { return  1; }
    virtual int TakeAChar(char i)                  { return  2; }
    virtual int TakeAShort(short i)                { return  3; }
    virtual int TakeAnInt(int i)                   { return  4; }
    virtual int TakeALong(long i)                  { return  5; }
    virtual int TakeALongLong(long long i)         { return  6; }
    virtual int TakeAFloat(float i)                { return  7; }
    virtual int TakeADouble(double i)              { return  8; }
    virtual int TakeAString(char *i)               { return  9; }
    virtual int TakeAnArray(int i[])               { return 10; }
    virtual int TakeAPointer(void *i)              { return 11; }
    virtual int TakeABoolPointer(bool *i)          { return 12; }
    virtual int TakeACharPointer(char *i)          { return 13; }
    virtual int TakeAShortPointer(short *i)        { return 14; }
    virtual int TakeAnIntPointer(int *i)           { return 15; }
    virtual int TakeALongPointer(long *i)          { return 16; }
    virtual int TakeALongLongPointer(long long *i) { return 17; }
    virtual int TakeAFloatPointer(float *i)        { return 18; }
    virtual int TakeADoublePointer(double *i)      { return 19; }
};

Foo::Foo()  { };
Foo::~Foo() { };
