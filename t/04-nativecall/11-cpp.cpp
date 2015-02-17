#ifdef WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

class Base {
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

class Derived1 : public Base {
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

class Derived2 : public Base {
public:
    Derived2();
    ~Derived2();
    virtual int method(int p) {
        return foo + bar + p;
    }
    int Set_foo(int p);
protected:
  int bar, baz;
  Point a_point;
  char c;
};

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
}

Derived2::~Derived2()
{
}
