use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;
use nqp;

plan 13;

compile_test_lib('26-varargs');

sub va1(int32, **@varargs) returns int32 is native('./26-varargs') { * }
ok va1(0, 1), 'Can pass plain ints';

{
sub va2(int32, **@varargs) returns int32 is native('./26-varargs') { * }
sub va3(int32, **@varargs) returns int32 is native('./26-varargs') { * }
my int8 $a1 = -1;
my int16 $a2 = -1 * 2**10;
my int32 $a3 = -1 * 2**18;
my int64 $a4 = -1 * 2**34;
my uint8 $a5 = 1;
my uint16 $a6 = 2**10;
my uint32 $a7 = 2**18;
my uint64 $a8 = 2**34;
ok va2(0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8), 'Can pass typed ints';
ok va3(0, Pointer.to($a1),
          Pointer.to($a2),
          Pointer.to($a3),
          Pointer.to($a4),
          Pointer.to($a5),
          Pointer.to($a6),
          Pointer.to($a7),
          Pointer.to($a8)), 'Can pass pointers to typed ints';
}

{
sub va4(int32, **@varargs) returns int32 is native('./26-varargs') { * }
my int8 $a1 = 1;
ok va4(0, Pointer.to($a1)), 'can modify pointers (status)';
is $a1, 2, 'can modify pointers';
}

{
sub va5(int32, **@varargs) returns int32 is native('./26-varargs') { * }
my $a1 = "Köln";
ok va5(0, $a1), 'can pass strings';
ok va5(0, "Köln"), 'can pass strings inline';
}

{
sub va6(int32, **@varargs) returns int32 is native('./26-varargs') { * }
my $string = "bo";
my $a1 = CArray[uint8].new($string.encode('utf16be').list, 0);
ok va6(0, $a1), "can pass CArrays";
ok va6(0, CArray[uint8].new($string.encode('utf16be').list, 0)), "can pass CArrays inline";
}

{
class S is repr('CStruct') {
    has int64 $.s0;
    has num64 $.s1;
}
sub va7(int32, **@varargs) returns int32 is native('./26-varargs') { * }
my $a1 = S.new: :s0(1), :s1(2.5e0);
ok va7(0, $a1), "can pass CStruct";
ok va7(0, S.new(:s0(1), :s1(2.5e0))), "can pass CStruct inline";
}

{
class U is repr('CUnion') {
    has int64 $.u0;
    has num64 $.u1;
}
sub va8(int32, **@varargs) returns int32 is native('./26-varargs') { * }
my $a1 = U.new: :u0(1);
ok va8(0, $a1), "can pass CUnion";
ok va8(0, U.new(:u0(1))), "can pass CUnion inline";
}

# vim: expandtab shiftwidth=4
