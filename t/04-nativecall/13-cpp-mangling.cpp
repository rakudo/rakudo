#include <stdlib.h>

#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

class Foo {
public:
    Foo();
    ~Foo();
    virtual int TakeAVoid(void) { return 0; }
    virtual int TakeABool(bool i) { return 1; }
    virtual int TakeAChar(char i) { return 2; }
    virtual int TakeAShort(short i) { return 3; }
    virtual int TakeAnInt(int i) { return 4; }
    virtual int TakeALong(long i) { return 5; }
    virtual int TakeALongLong(long long i) { return 6; }
    virtual int TakeAFloat(float i) { return 7; }
    virtual int TakeADouble(double i) { return 8; }
    virtual int TakeAString(char *i) { return 9; }
    virtual int TakeAnArray(int *i) { return 10; }
};

Foo::Foo(){};
Foo::~Foo(){};
