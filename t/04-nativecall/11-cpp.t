use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

try {
    compile_cpp_test_lib('11-cpp');
    CATCH {
        default {
            diag "Error while compiling C++ script:\n$_.payload()";
            print "1..0 # Skip: Cannot compile C++ script\n";
            exit 0
        }
    }
}

plan 22;

#~ shell 'dumpbin /exports 11-cpp.dll';
#~ shell 'clang --shared -fPIC -o 11-cpp.so t/04-nativecall/11-cpp.cpp';
#~ shell 'nm 11-cpp.so';
#~ shell 'clang -cc1 -fdump-record-layouts t/04-nativecall/11-cpp.cpp';

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
    method new() returns Derived1 is native("./11-cpp") is nativeconv('thisgnu') { * } # const *
}

sub SizeofDerived1() returns int32 is mangled is native("./11-cpp") { * }

is nativesizeof(Derived1), SizeofDerived1(), 'sizeof(Derived1)';
ok my $d1 = Derived1.new, 'can instantiate C++ class';
ok my Derived1 $d1b .= new, 'can instantiate the same C++ class again using « .= »';
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
    has Pointer[int32] $.intptr;
    method new() returns Derived2 is native("./11-cpp") is nativeconv('thisgnu') { * } # const *
    method All_The_Things(int8, int16, int32, long, num32, num64) returns long is native("./11-cpp") is nativeconv('thisgnu') { * }

    method ConstInt(int32 is cpp-const)          returns long is native("./11-cpp") is nativeconv('thisgnu') { * }
    method IntPtr(int32 is rw)                   returns long is native("./11-cpp") is nativeconv('thisgnu') { * }
    method ConstIntPtr(int32 is cpp-const is rw) returns long is native("./11-cpp") is nativeconv('thisgnu') { * }
}

sub SizeofDerived2() returns int32 is mangled is native("./11-cpp") { * }

is nativesizeof(Derived2), SizeofDerived2(), 'sizeof(Derived2)';
ok my $d2 = Derived2.new, 'can instantiate C++ class with vtable';
is $d2.foo,   11,   'can read attribute foo';
is $d2.bar,   42,   'can read attribute bar';
is $d2.baz,   43,   'can read attribute baz';
is $d2.cx,    3.14, 'can read attribute cx';
is $d2.cy,    2.62, 'can read attribute cy';
is $d2.c.chr, 'A',  'can read attribute c';
is $d2.intptr.deref, 666, 'can read typed pointer attribute';

is $d2.All_The_Things(1, 2, 3, 4, 5e0, 6e0), 21, 'can pass arguments to method';

is $d2.ConstInt(123),         11, 'name mangling of parameter `const int`';
my int32 $int_ptr = 123;
is $d2.IntPtr($int_ptr),      12, 'name mangling of parameter `int *`';
is $d2.ConstIntPtr($int_ptr), 13, 'name mangling of parameter `const int *`';

# vim: expandtab shiftwidth=4
