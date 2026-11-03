use lib <t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use NativeCall::Types;
use Test;

plan 31;

compile_test_lib('04-pointers');

sub ReturnSomePointer()         returns Pointer is native("./04-pointers") { * }
sub CompareSomePointer(Pointer) returns int32   is native("./04-pointers") { * }
sub ReturnNullPointer()         returns Pointer is native("./04-pointers") { * }
sub ReturnPointerToIntArray()   returns Pointer[int32] is native("./04-pointers") { * }

my $x     = ReturnSomePointer();
my int $a = 4321;

ok CompareSomePointer($x), 'Got passed back the pointer I returned';
ok $x,     'Non-NULL pointer is trueish';
ok $x.Int, 'Calling .Int on non-NULL pointer is trueish';
ok +$x,    'Calling prefix:<+> on non-NULL pointer is trueish';
is +$x.raku.EVAL,          +$x,               'Pointer roundtrips okay using .raku and EVAL';
is +Pointer.new,          0, 'Numerical value of Pointer.new is 0';
is +Pointer.new(0),       0, 'Pointer.new(0) has 0 numerical value';
is +Pointer.new(1234), 1234, 'Pointer.new(1234) has numerical value 1234';
is +Pointer.new($a),     $a, 'Pointer.new accepts a native int too';
ok ReturnNullPointer() === Pointer,           'A returned NULL pointer is the Pointer type object itself';

my $p = ReturnPointerToIntArray();
is $p.deref, 10, 'typed pointer deref method';
if $*VM.name eq 'jvm' {
    skip 'UnsupportedOperationException: This pointer is opaque', 9;
}
else {
    is $p[1], 20, 'typed pointer array dereference';
    is (++$p).deref, 20, 'typed pointer increment';
    is ($p.add: -1).deref, 10, '.add(-1)';
    is $p[0], 20, 'typed pointer incremented (1)';
    is $p[1], 30, 'typed pointer incremented (2)';
    is (--$p).deref, 10, 'typed pointer decrement';
    is $p[0], 10, 'typed pointer incremented (1)';
    is $p[1], 20, 'typed pointer incremented (2)';
    is ($p.add: 2).deref, 30, '.add(2)';
}


{
    # https://github.com/Raku/old-issue-tracker/issues/3783
    eval-lives-ok q:to 'CODE', 'Signature matching with Pointer[int32] works';
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

# https://github.com/rakudo/rakudo/issues/4483
{
    is-deeply +Pointer, 0, 'Numerifying Pointer class works';
}

# A definiteness-constrained native return type maps as its base type, rather
# than raising "Unknown type ...:D used in native call" from type_code_for.
{
    sub ReturnSomePointerD(--> Pointer:D) is symbol('ReturnSomePointer')
      is native('./04-pointers') { * }
    ok ReturnSomePointerD(), 'a Pointer:D native return type works';

    my class CPtr is repr('CPointer') {
        our sub make(--> ::?CLASS:D) is symbol('ReturnSomePointer')
          is native('./04-pointers') { * }
    }
    ok CPtr::make().defined, 'a repr(CPointer) class `--> ::?CLASS:D` native return works';
}

# Definiteness constraints on a native signature are enforced like on a
# normal routine: a violating return value or argument throws rather than
# being silently passed along.
{
    sub ReturnNullPointerD(--> Pointer:D) is symbol('ReturnNullPointer')
      is native('./04-pointers') { * }
    throws-like { ReturnNullPointerD() }, X::TypeCheck::Return,
      'a NULL return violating --> Pointer:D throws';

    my class CPtrD is repr('CPointer') {
        our sub make(--> CPtrD:D) is symbol('ReturnNullPointer')
          is native('./04-pointers') { * }
    }
    throws-like { CPtrD::make() }, X::TypeCheck::Return,
      'a NULL return violating a CPointer class :D return type throws';

    sub CompareSomePointerD(Pointer:D $ptr) returns int32
      is symbol('CompareSomePointer') is native('./04-pointers') { * }
    ok CompareSomePointerD(ReturnSomePointer()),
      'a concrete argument satisfies a Pointer:D parameter';
    throws-like { CompareSomePointerD(Pointer) },
      X::Parameter::InvalidConcreteness,
      'a type object violating a Pointer:D parameter throws';
    my Pointer $undefined;
    throws-like { CompareSomePointerD($undefined) },
      X::Parameter::InvalidConcreteness,
      'an undefined value in a container violating a Pointer:D parameter throws';

    sub TakeUndefinedPointer(Pointer:U $ptr) returns int32
      is symbol('CompareSomePointer') is native('./04-pointers') { * }
    throws-like { TakeUndefinedPointer(ReturnSomePointer()) },
      X::Parameter::InvalidConcreteness,
      'a concrete argument violating a Pointer:U parameter throws';
}

# vim: expandtab shiftwidth=4
