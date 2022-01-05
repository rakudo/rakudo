use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 17;

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

class foo1 is repr<CStruct> {
    has Str  $.p;
    has int8 $.c;
    has long $.x;
}

class foo2 is repr<CStruct> {
    has Str   $.p;
    has int16 $.x;
}

class foo3 is repr<CStruct> {
    has Str  $.p;
    has int8 $.c;
}

class foo4 is repr<CStruct> {
    has int16 $.s;
    has int8  $.c;
}

class foo5 is repr<CStruct> {
    has int8 $.c;
    HAS foo2 $.s;
};

class foo6 is repr<CStruct> {
    has int8  $.c;
    has foo6  $.p;
    has int16 $.x;
};

class foo7 is repr<CStruct> {
    has foo7  $.p;
    has int16 $.x;
    has int8  $.c;
};

class foo8 is repr<CStruct> {
    HAS foo2 $.s;
    has int8 $.c;
};

sub SizeofFoo() returns int32 is native('./12-sizeof') { * }
sub SizeofBar() returns int32 is native('./12-sizeof') { * }
sub SizeofBaz() returns int32 is native('./12-sizeof') { * }
sub SizeofBuz() returns int32 is native('./12-sizeof') { * }
sub SizeofInt() returns int32 is native('./12-sizeof') { * }
sub SizeofLng() returns int32 is native('./12-sizeof') { * }
sub SizeofPtr() returns int32 is native('./12-sizeof') { * }
sub SizeofBool() returns int32 is native('./12-sizeof') { * }
sub SizeofSizeT() returns int32 is native('./12-sizeof') { * }
sub SizeofFoo1()  returns int32 is native('./12-sizeof') { * }
sub SizeofFoo2()  returns int32 is native('./12-sizeof') { * }
sub SizeofFoo3()  returns int32 is native('./12-sizeof') { * }
sub SizeofFoo4()  returns int32 is native('./12-sizeof') { * }
sub SizeofFoo5()  returns int32 is native('./12-sizeof') { * }
sub SizeofFoo6()  returns int32 is native('./12-sizeof') { * }
sub SizeofFoo7()  returns int32 is native('./12-sizeof') { * }
sub SizeofFoo8()  returns int32 is native('./12-sizeof') { * }

is nativesizeof(Foo),     SizeofFoo(), 'sizeof(Foo)';
is nativesizeof(Bar),     SizeofBar(), 'sizeof(Bar)';
is nativesizeof(Baz),     SizeofBaz(), 'sizeof(Baz)';
is nativesizeof(Buz),     SizeofBuz(), 'sizeof(Buz)';
is nativesizeof(int32),   SizeofInt(), 'sizeof(int)';
is nativesizeof(long),    SizeofLng(), 'sizeof(long)';
is nativesizeof(Pointer), SizeofPtr(), 'sizeof(Pointer)';
is nativesizeof(bool),    SizeofBool(), 'sizeof(bool)';
is nativesizeof(size_t),  SizeofSizeT(), 'sizeof(size_t)';
is nativesizeof(foo1),    SizeofFoo1(),  'sizeof(foo1)';
is nativesizeof(foo2),    SizeofFoo2(),  'sizeof(foo2)';
is nativesizeof(foo3),    SizeofFoo3(),  'sizeof(foo3)';
is nativesizeof(foo4),    SizeofFoo4(),  'sizeof(foo4)';
is nativesizeof(foo5),    SizeofFoo5(),  'sizeof(foo5)';
is nativesizeof(foo6),    SizeofFoo6(),  'sizeof(foo6)';
is nativesizeof(foo7),    SizeofFoo7(),  'sizeof(foo7)';
is nativesizeof(foo8),    SizeofFoo8(),  'sizeof(foo8)';

# vim: expandtab shiftwidth=4
