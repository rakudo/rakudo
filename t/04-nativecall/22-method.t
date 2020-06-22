use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 2;

compile_test_lib('22-method');

class MyStruct is repr('CStruct') {
    has long   $.long;

    sub ReturnAStruct(long $intval --> MyStruct) is native('./22-method') { * }
    method new(Int :$intval) {
        ReturnAStruct($intval)
    }
    method Add(long $intval --> long) is native('./22-method') { * }
}

my $a = MyStruct.new(intval => 42);

my $res;

lives-ok { $res = $a.Add(2) }, "native sub as method";
is $res, 44, "and got the result we expected";

# vim: expandtab shiftwidth=4
