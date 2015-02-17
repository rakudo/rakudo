use v6;
use NativeCall;
use Test;

plan 14;

shell 'g++ --shared -fPIC -o 11-cpp.so t/11-cpp.cpp';
#~ shell 'clang --shared -fPIC -o 11-cpp.so t/11-cpp.cpp';
#~ shell 'nm 11-cpp.so';
#~ shell 'clang -cc1 -fdump-record-layouts t/11-cpp.cpp';

class Point is repr<CStruct> {
    has num32 $.x;
    has num32 $.y;
}

class Base is repr<CPPStruct> {
    has int32 $.foo;
    method new()  is native("./11-cpp") is nativeconv('thisgnu') { * } # const *
}

my class VirtualMethodTable is repr('CPointer') { }

class Derived1 is repr<CPPStruct> {
    has int32 $.foo;
    has int32 $.bar;
    has int32 $.baz;
    has num64 $.cx;
    has num64 $.cy;
    has uint8 $.c;
    method new()  is native("./11-cpp") is nativeconv('thisgnu') { * } # const *
}

sub SizeofDerived1() is symbol('_Z14SizeofDerived1v') returns int32 is native("./11-cpp") { * }

diag 'sizeof = ' ~ SizeofDerived1();
ok my $d1 = Derived1.new, 'can instanciate C++ class';
is $d1.foo,   11,   'can read attribute foo';
is $d1.bar,   42,   'can read attribute bar';
is $d1.baz,   43,   'can read attribute baz';
is $d1.cx,    3.14, 'can read attribute cx';
is $d1.cy,    2.62, 'can read attribute cy';
is $d1.c.chr, 'A',  'can read attribute c';

class Derived2 is repr<CPPStruct> {
    has VirtualMethodTable $.vtable;
    has int32 $.foo;
    has int32 $.bar;
    has int32 $.baz;
    has num64 $.cx;
    has num64 $.cy;
    has uint8 $.c;
    method new()  is native("./11-cpp") is nativeconv('thisgnu') { * } # const *
}

sub SizeofDerived2() is symbol('_Z14SizeofDerived2v') returns int32 is native("./11-cpp") { * }

diag 'sizeof = ' ~ SizeofDerived2();
ok my $d2 = Derived2.new, 'can instanciate C++ class with vtable';
is $d2.foo,   11,   'can read attribute foo';
is $d2.bar,   42,   'can read attribute bar';
is $d2.baz,   43,   'can read attribute baz';
is $d2.cx,    3.14, 'can read attribute cx';
is $d2.cy,    2.62, 'can read attribute cy';
is $d2.c.chr, 'A',  'can read attribute c';
