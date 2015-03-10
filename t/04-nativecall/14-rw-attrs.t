use lib 't/04-nativecall';
use CompileTestLib;
use lib 'lib';
use NativeCall;
use Test;

plan 16;

compile_test_lib('14-rw-attrs');

class MyStruct is repr('CStruct') {
    has long  $.long   is rw;
    has num64 $.double is rw;
    has int8  $.char   is rw;
    has num32 $.float  is rw;
}

sub InitStruct(MyStruct)                      is native('./14-rw-attrs') { * }
sub GetLongOfStruct(MyStruct)   returns long  is native('./14-rw-attrs') { * }
sub GetDoubleOfStruct(MyStruct) returns num64 is native('./14-rw-attrs') { * }
sub GetCharOfStruct(MyStruct)   returns int8  is native('./14-rw-attrs') { * }
sub GetFloatOfStruct(MyStruct)  returns num32 is native('./14-rw-attrs') { * }

my $s = MyStruct.new();

is        $s.long,   0, 'got initialized (long)';
is_approx $s.double, 0, 'got initialized (double)';
is        $s.char,   0, 'got initialized (char)';
is_approx $s.float,  0, 'got initialized (float)';

InitStruct($s);

is        $s.long,   1,    'set in C (long)';
is_approx $s.double, 2.22, 'set in C (double)';
is        $s.char,   3,    'set in C (char)';
is_approx $s.float,  4.44, 'set in C (float)';

$s.long   = 42;
$s.double = 43.3e0;
$s.char   = 44;
$s.float  = 45.5e0;

is        $s.long,   42,   'set in Perl (long)';
is_approx $s.double, 43.3, 'set in Perl (double)';
is        $s.char,   44,   'set in Perl (char)';
is_approx $s.float,  45.5, 'set in Perl (float)';

is        GetLongOfStruct($s),   42,   'C confirms (long)';
is_approx GetDoubleOfStruct($s), 43.3, 'C confirms (double)';
is        GetCharOfStruct($s),   44,   'C confirms (char)';
is_approx GetFloatOfStruct($s),  45.5, 'C confirms (float)';


# vim:ft=perl6
