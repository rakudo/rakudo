use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use NativeCall::Types;
use Test;

plan 12;

compile_test_lib('04-pointers');

sub ReturnSomePointer()         returns Pointer is native("./04-pointers") { * }
sub CompareSomePointer(Pointer) returns int32   is native("./04-pointers") { * }
sub ReturnNullPointer()         returns Pointer is native("./04-pointers") { * }

my $x     = ReturnSomePointer();
my int $a = 4321;

ok CompareSomePointer($x), 'Got passed back the pointer I returned';
ok $x,     'Non-NULL pointer is trueish';
ok $x.Int, 'Calling .Int on non-NULL pointer is trueish';
ok +$x,    'Calling prefix:<+> on non-NULL pointer is trueish';
is +$x.perl.EVAL,          +$x,               'Pointer roundtrips okay using .perl and EVAL';
is +Pointer.new,          0, 'Numerical value of Pointer.new is 0';
is +Pointer.new(0),       0, 'Pointer.new(0) has 0 numerical value';
is +Pointer.new(1234), 1234, 'Pointer.new(1234) has numerical value 1234';
is +Pointer.new($a),     $a, 'Pointer.new accepts a native int too';
ok ReturnNullPointer() === Pointer,           'A returned NULL pointer is the Pointer type object itself';

{
    eval-lives-ok q:to 'CODE', 'Signature matching with Pointer[int32] works (RT #124321)';
        use NativeCall;

        sub TakeTwoPointersToInt( Pointer[int32], Pointer[int32] )
          is native( './04-pointers' ) { * }

        my Pointer[int32] $r;
        my Pointer[int32] $c;

        TakeTwoPointersToInt( $r, $c );
        CODE
}

{
    eval-lives-ok q:to 'CODE', 'Signature matching with CArray[Int] works';
        use NativeCall;

        sub TakeCArrayToInt8( CArray[int8] )
          is native( './04-pointers' ) { * }

        my CArray[int8] $x;

        TakeCArrayToInt8( $x );
        CODE
}
