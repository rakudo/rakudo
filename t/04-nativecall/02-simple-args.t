use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 14;

compile_test_lib('02-simple-args');

# Int related
sub TakeInt(int32)                      returns int32 is native('./02-simple-args') { * }
sub TakeTwoShorts(int16, int16)         returns int32 is native('./02-simple-args') { * }
sub AssortedIntArgs(int32, int16, int8) returns int32 is native('./02-simple-args') { * }
is TakeInt(42),                    1, 'passed int 42';
is TakeTwoShorts(10, 20),          2, 'passed two shorts';
is AssortedIntArgs(101, 102, 103), 3, 'passed an int32, int16 and int8';

# Float related
sub TakeADouble(num64) returns int32 is native('./02-simple-args') { * }
sub TakeAFloat(num32)  returns int32 is native('./02-simple-args') { * }
is TakeADouble(-6.9e0), 4, 'passed a double';
is TakeAFloat(4.2e0),   5, 'passed a float';

# String related
sub TakeAString(Str) returns int32 is native('./02-simple-args') { * }
is TakeAString('ok 6 - passed a string'), 6, 'passed a string';

# Explicitly managing strings
sub SetString(Str) returns int32 is native('./02-simple-args') { * }
sub CheckString()  returns int32 is native('./02-simple-args') { * }
my $str = 'ok 7 - checked previously passed string';
explicitly-manage($str);
SetString($str);
is CheckString(), 7, 'checked previously passed string';

# Make sure wrapped subs work
sub wrapped(int32) returns int32 is native('./02-simple-args') { * }
sub wrapper(int32 $arg) { is wrapped($arg), 8, 'wrapped sub' }
wrapper(42);

# 64-bit integer
sub TakeInt64(int64) returns int32 is native('./02-simple-args') { * }
is TakeInt64(0xFFFFFFFFFF), 9, 'passed int64 0xFFFFFFFFFF';

# Unsigned integers.
sub TakeUint8(uint8) returns int32 is native('./02-simple-args') { * }
sub TakeUint16(uint16) returns int32 is native('./02-simple-args') { * }
sub TakeUint32(uint32) returns int32 is native('./02-simple-args') { * }
if $*DISTRO.name eq 'macosx' {
    #
    # For some reason, on OS X with clang, the following test fails with -O3
    # specified.  One can only assume this is some weird compiler issue (tested
    # on Apple LLVM version 6.1.0 (clang-602.0.49) (based on LLVM 3.6.0svn).
    #
    skip("Cannot test TakeUint8(0xFE) on OS X with -O3");
}
else {
    is TakeUint8(0xFE),        10, 'passed uint8 0xFE';
}
# R#2124 https://github.com/rakudo/rakudo/issues/2124
skip("Cannot test TakeUint16(0xFFFE) with clang without -O0");
#is TakeUint16(0xFFFE),     11, 'passed uint16 0xFFFE';


is TakeUint32(0xFFFFFFFE), 12, 'passed uint32 0xFFFFFFFE';

sub TakeSizeT(size_t) returns int32 is native('./02-simple-args') { * }
is TakeSizeT(42),     13, 'passed size_t 42';
sub TakeSSizeT(ssize_t --> int32) is native('./02-simple-args') { * }
is TakeSSizeT(-42),   14, 'passed ssize_t -42';

# vim: expandtab shiftwidth=4
