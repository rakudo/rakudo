use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

try {
    compile_cpp_test_lib('13-cpp-mangling');
    CATCH {
        default {
            diag "Error while compiling C++ script:\n$_.payload()";
            print "1..0 # Skip: Cannot compile C++ script\n";
            exit 0
        }
    }
}

plan 26;

# shell 'dumpbin /exports 13-cpp-mangling.dll';

class Foo is repr<CPPStruct> {
    has Pointer $.vtable;

    method new()          is nativeconv('thisgnu')               is native("./13-cpp-mangling") { * }
    method TakeAVoid()                             returns int32 is native("./13-cpp-mangling") { * }
    method TakeABool(Bool)                         returns int32 is native("./13-cpp-mangling") { * }
    method TakeAChar(int8)                         returns int32 is native("./13-cpp-mangling") { * }
    method TakeAShort(int16)                       returns int32 is native("./13-cpp-mangling") { * }
    method TakeAnInt(int32)                        returns int32 is native("./13-cpp-mangling") { * }
    method TakeALong(long)                         returns int32 is native("./13-cpp-mangling") { * }
    method TakeALongLong(longlong)                 returns int32 is native("./13-cpp-mangling") { * }
    method TakeAFloat(num32)                       returns int32 is native("./13-cpp-mangling") { * }
    method TakeADouble(num64)                      returns int32 is native("./13-cpp-mangling") { * }
    method TakeAString(Str)                        returns int32 is native("./13-cpp-mangling") { * }
    method TakeAnArray(CArray[int32])              returns int32 is native("./13-cpp-mangling") { * }
    method TakeAPointer(Pointer)                   returns int32 is native("./13-cpp-mangling") { * }
    method TakeABoolPointer(Pointer[Bool])         returns int32 is native("./13-cpp-mangling") { * }
    method TakeACharPointer(Pointer[int8])         returns int32 is native("./13-cpp-mangling") { * }
    method TakeAShortPointer(Pointer[int16])       returns int32 is native("./13-cpp-mangling") { * }
    method TakeAnIntPointer(Pointer[int32])        returns int32 is native("./13-cpp-mangling") { * }
    method TakeALongPointer(Pointer[long])         returns int32 is native("./13-cpp-mangling") { * }
    method TakeALongLongPointer(Pointer[longlong]) returns int32 is native("./13-cpp-mangling") { * }
    method TakeAFloatPointer(Pointer[num32])       returns int32 is native("./13-cpp-mangling") { * }
    method TakeADoublePointer(Pointer[num64])      returns int32 is native("./13-cpp-mangling") { * }
    method TakeAUInt(uint32)                       returns int32 is native("./13-cpp-mangling") { * }
    method TakeAUShort(uint16)                     returns int32 is native("./13-cpp-mangling") { * }
    method TakeAUChar(uint8)                       returns int32 is native("./13-cpp-mangling") { * }
    method TakeAInt64(int64)                       returns int32 is native("./13-cpp-mangling") { * }
    method TakeAULongLong(ulonglong)               returns int32 is native("./13-cpp-mangling") { * }
    method TakeAUInt64(uint64)                     returns int32 is native("./13-cpp-mangling") { * }


}

my $foo = Foo.new;

is $foo.TakeAVoid(),                                  0, 'void mangling';
is $foo.TakeABool(True),                              1, 'bool mangling';
is $foo.TakeAChar(1),                                 2, 'char mangling';
is $foo.TakeAShort(1),                                3, 'short mangling';
is $foo.TakeAnInt(1),                                 4, 'int mangling';
is $foo.TakeALong(1),                                 5, 'long mangling';
is $foo.TakeALongLong(1),                             6, 'long long mangling';
is $foo.TakeAFloat(5e0),                              7, 'float mangling';
is $foo.TakeADouble(6e0),                             8, 'double mangling';
is $foo.TakeAString("1"),                             9, 'string mangling';
is $foo.TakeAnArray(CArray[int32].new),              10, 'CArray mangling';
is $foo.TakeAPointer(Pointer),                       11, 'Pointer mangling';
is $foo.TakeABoolPointer(Pointer[Bool].new),         12, 'bool* mangling';
is $foo.TakeACharPointer(Pointer[int8].new),         13, 'char* mangling';
is $foo.TakeAShortPointer(Pointer[int16].new),       14, 'short* mangling';
is $foo.TakeAnIntPointer(Pointer[int32].new),        15, 'int* mangling';
is $foo.TakeALongPointer(Pointer[long].new),         16, 'long* mangling';
is $foo.TakeALongLongPointer(Pointer[longlong].new), 17, 'long long* mangling';
is $foo.TakeAFloatPointer(Pointer[num32].new),       18, 'float* mangling';
is $foo.TakeADoublePointer(Pointer[num64].new),      19, 'double* mangling';
is $foo.TakeAUInt(1),                                20, 'uint mangling';
is $foo.TakeAUShort(1),                              21, 'ushort mangling';
is $foo.TakeAUChar(1),                               22, 'uchar mangling';
is $foo.TakeAInt64(1),                               23, 'int64 mangling';
is $foo.TakeAULongLong(1),                           24, 'unsigned long long mangling';
is $foo.TakeAUInt64(1),                              25, 'uint64 mangling';

# vim: expandtab shiftwidth=4
