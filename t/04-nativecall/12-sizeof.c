#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

#if defined(_MSC_VER) && _MSC_VER < 1800
#  define bool unsigned char
#  define true  1
#  define false 0
#  define __bool_true_false_are_defined 1
#else
#  include <stdbool.h>
#endif

#include <stddef.h>
#include <stdint.h>
#include <wchar.h>

typedef struct {
    char  foo1;
    int   foo2;
    short foo3;
    short foo4;
} Foo;

typedef struct {
    char  bar1;
    short bar2;
    char  bar3;
    int   bar4;
    short bar5;
} Bar;

typedef struct {
    char  bar1;
    short bar2;
    char  bar3;
    int   bar4;
    short bar5;
    long  baz6;
    int   bar7;
} Baz;

typedef struct {
    char  buz1;
} Buz;

struct foo1 {
    char *p;
    char c;
    long x;
};

struct foo2 {
    char *p;
    short x;
};

struct foo3 {
    char *p;
    char c;
};

struct foo4 {
    short s;
    char c;
};

struct foo5 {
    char c;
    struct foo2 s;
};

struct foo6 {
    char c;
    struct foo10 *p;
    short x;
};

struct foo7 {
    struct foo11 *p;
    short x;
    char c;
};

struct foo8 {
    struct foo2 s;
    char c;
};

DLLEXPORT size_t SizeofFoo()     { return sizeof(Foo);            }
DLLEXPORT size_t SizeofBar()     { return sizeof(Bar);            }
DLLEXPORT size_t SizeofBaz()     { return sizeof(Baz);            }
DLLEXPORT size_t SizeofBuz()     { return sizeof(Buz);            }
DLLEXPORT size_t SizeofInt()     { return sizeof(int);            }
DLLEXPORT size_t SizeofLng()     { return sizeof(long);           }
DLLEXPORT size_t SizeofPtr()     { return sizeof(void *);         }
DLLEXPORT size_t SizeofBool()    { return sizeof(bool);           }
DLLEXPORT size_t SizeofSizeT()   { return sizeof(size_t);         }
DLLEXPORT size_t SizeofFoo1()    { return sizeof(struct foo1);    }
DLLEXPORT size_t SizeofFoo2()    { return sizeof(struct foo2);    }
DLLEXPORT size_t SizeofFoo3()    { return sizeof(struct foo3);    }
DLLEXPORT size_t SizeofFoo4()    { return sizeof(struct foo4);    }
DLLEXPORT size_t SizeofFoo5()    { return sizeof(struct foo5);    }
DLLEXPORT size_t SizeofFoo6()    { return sizeof(struct foo6);    }
DLLEXPORT size_t SizeofFoo7()    { return sizeof(struct foo7);    }
DLLEXPORT size_t SizeofFoo8()    { return sizeof(struct foo8);    }
DLLEXPORT size_t SizeofWCharT()  { return sizeof(wchar_t);        }
DLLEXPORT size_t SizeofWIntT()   { return sizeof(wint_t);         }
DLLEXPORT size_t SizeofChar16T() { return sizeof(uint_least16_t); }
DLLEXPORT size_t SizeofChar32T() { return sizeof(uint_least32_t); }
