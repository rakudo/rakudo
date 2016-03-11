use v6;
use lib 't/04-nativecall';
use CompileTestLib;
use NativeCall;
use Test;

plan 9;

compile_test_lib('12-sizeof');

class Foo is repr<CStruct> {
    has int8  $.foo1;
    has int32 $.foo2;
    has int16 $.foo3;
    has int16 $.foo4;
}

class Bar is repr<CStruct> {
    has int8  $.bar1;
    has int16 $.bar2;
    has int8  $.bar3;
    has int32 $.bar4;
    has int16 $.bar5;
}

class Baz is repr<CStruct> {
    has int8  $.bar1;
    has int16 $.bar2;
    has int8  $.bar3;
    has int32 $.bar4;
    has int16 $.bar5;
    has long  $.bar6;
    has int32 $.bar7;
}

class Buz is repr<CStruct> {
    has int8  $.bar1;
}

sub SizeofFoo() returns int32 is native('./12-sizeof') { * }
sub SizeofBar() returns int32 is native('./12-sizeof') { * }
sub SizeofBaz() returns int32 is native('./12-sizeof') { * }
sub SizeofBuz() returns int32 is native('./12-sizeof') { * }
sub SizeofInt() returns int32 is native('./12-sizeof') { * }
sub SizeofLng() returns int32 is native('./12-sizeof') { * }
sub SizeofPtr() returns int32 is native('./12-sizeof') { * }
sub SizeofBool() returns int32 is native('./12-sizeof') { * }
sub SizeofSizeT() returns int32 is native('./12-sizeof') { * }

is nativesizeof(Foo),     SizeofFoo(), 'sizeof(Foo)';
is nativesizeof(Bar),     SizeofBar(), 'sizeof(Bar)';
is nativesizeof(Baz),     SizeofBaz(), 'sizeof(Baz)';
is nativesizeof(Buz),     SizeofBuz(), 'sizeof(Buz)';
is nativesizeof(int32),   SizeofInt(), 'sizeof(int)';
is nativesizeof(long),    SizeofLng(), 'sizeof(long)';
is nativesizeof(Pointer), SizeofPtr(), 'sizeof(Pointer)';
is nativesizeof(bool),    SizeofBool(), 'sizeof(bool)';
is nativesizeof(size_t),  SizeofSizeT(), 'sizeof(size_t)';

