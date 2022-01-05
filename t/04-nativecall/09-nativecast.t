use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan(9);

compile_test_lib('09-nativecast');

sub ReturnArray() returns Pointer is native('./09-nativecast') { * }
my $carray = nativecast(CArray[uint32], ReturnArray());
is $carray[0..2], (1, 2, 3), 'casting int * to CArray[uint32] works';

sub ReturnStruct() returns Pointer is native('./09-nativecast') { * };
class CUTE is repr('CStruct') {
    has int32 $.i;
}
is nativecast(CUTE, ReturnStruct()).i, 100, 'casting to CStruct works';

sub ReturnInt() returns Pointer is native('./09-nativecast') { * }
is nativecast(int32, ReturnInt()), 101, 'casting to int32 works';

sub ReturnShort() returns Pointer  is native('./09-nativecast') { * }
is nativecast(int16, ReturnShort()), 102, 'casting to int16 works';

sub ReturnByte() returns Pointer is native('./09-nativecast') { * }
is nativecast(int8, ReturnByte()), -103, 'casting to int8 works';

sub ReturnDouble() returns Pointer is native('./09-nativecast') { * }
is-approx nativecast(num64, ReturnDouble()), 99.9e0, 'casting to num64 works';

sub ReturnFloat() returns Pointer is native('./09-nativecast') { * }
is-approx nativecast(num32, ReturnFloat()), -4.5e0, 'casting to num32 works';

sub ReturnString() returns Pointer is native('./09-nativecast') { * }
is nativecast(str, ReturnString()), "epic cuteness", 'casting to str works';

sub ReturnNullString returns Pointer is native('./09-nativecast') { * }
nok nativecast(str, ReturnNullString()).defined, 'casting null pointer to str';

# vim: expandtab shiftwidth=4
