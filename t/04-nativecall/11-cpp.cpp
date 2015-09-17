#include <stdlib.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

class DLLEXPORT Base {
protected:
  int foo;
public:
  int method(int p) {
    return foo + p;
  }
};

struct Point {
  double cx, cy;
};

class DLLEXPORT Derived1 : public Base {
public:
    Derived1();
    ~Derived1();
    int method(int p) {
        return foo + bar + p;
    }
    int Set_foo(int p);
protected:
  int bar, baz;
  Point a_point;
  char c;
};

DLLEXPORT int SizeofDerived1() {
  return sizeof(Derived1);
}

Derived1::Derived1()
{
    foo        = 11;
    bar        = 42;
    baz        = 43;
    a_point.cx = 3.14;
    a_point.cy = 2.62;
    c          = 'A';
}

Derived1::~Derived1()
{
}

class DLLEXPORT Derived2 : public Base {
public:
    Derived2();
    ~Derived2();
    virtual int method(int p) {
        return foo + bar + p;
    }
    int Set_foo(int p);
    virtual long All_The_Things(char c, short s, int i, long l, float f, double d);
    virtual long ConstInt(const int i);
    virtual long IntPtr(int *i);
    virtual long ConstIntPtr(const int *i);
protected:
  int bar, baz;
  Point a_point;
  char c;
  int *intptr;
};

long Derived2::All_The_Things(char c, short s, int i, long l, float f, double d) {
    return c + s + i + l + f + d;
}
long Derived2::ConstInt(const int i)     { return 11; }
long Derived2::IntPtr(int *i)            { return 12; }
long Derived2::ConstIntPtr(const int *i) { return 13; }

DLLEXPORT int SizeofDerived2() {
  return sizeof(Derived2);
}

Derived2::Derived2()
{
    foo        = 11;
    bar        = 42;
    baz        = 43;
    a_point.cx = 3.14;
    a_point.cy = 2.62;
    c          = 'A';
    intptr     = (int *)malloc(sizeof(int));
    *intptr    = 666;
}

Derived2::~Derived2()
{
    free(intptr);
}
