use v6;
use NativeCall;
use Test;

plan 11;

shell 'g++ --shared -fPIC -o 13-cpp-mangling.so t/04-nativecall/13-cpp-mangling.cpp';

class Foo is repr<CPPStruct> {
    has Pointer $.vtable; # TODO : bug - trying to allocate -1 when no attributes given

    method new() is native("./13-cpp-mangling") is nativeconv('thisgnu') { * } # const *
    method TakeAVoid() returns int32 is native("./13-cpp-mangling") { * };
#    method TakeABool(Bool) returns int32 is native("./13-cpp-mangling") { * };
    method TakeAChar(int8) returns int32 is native("./13-cpp-mangling") { * };
    method TakeAShort(int16) returns int32 is native("./13-cpp-mangling") { * };
    method TakeAnInt(int32) returns int32 is native("./13-cpp-mangling") { * };
    method TakeALong(long) returns int32 is native("./13-cpp-mangling") { * };
    method TakeALongLong(longlong) returns int32 is native("./13-cpp-mangling") { * };
    method TakeAFloat(num32) returns int32 is native("./13-cpp-mangling") { * };
    method TakeADouble(num64) returns int32 is native("./13-cpp-mangling") { * };
    method TakeAString(Str) returns int32 is native("./13-cpp-mangling") { * };
    method TakeAnArray(CArray[int32]) returns int32 is native("./13-cpp-mangling") { * };
    method TakeAPointer(Pointer) returns int32 is native("./13-cpp-mangling") { * };
}

my $foo = Foo.new;

is $foo.TakeAVoid(), 0, 'void mangling';
#is $foo.TakeABool(True), 1, 'bool mangling';
is $foo.TakeAChar(1), 2, 'char mangling';
is $foo.TakeAShort(1), 3, 'short mangling';
is $foo.TakeAnInt(1), 4, 'int mangling';
is $foo.TakeALong(1), 5, 'long mangling';
is $foo.TakeALongLong(1), 6, 'long long mangling';
is $foo.TakeAFloat(5e0), 7, 'float mangling';
is $foo.TakeADouble(6e0), 8, 'double mangling';
is $foo.TakeAString("1"), 9, 'string mangling';
is $foo.TakeAnArray(CArray[int32].new), 10, 'CArray mangling';
is $foo.TakeAPointer, 11, 'Pointer mangling';
# TODO : add more
