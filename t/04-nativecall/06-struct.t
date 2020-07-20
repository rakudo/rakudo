use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 34;

compile_test_lib('06-struct');

class MyStruct is repr('CStruct') {
    has long   $.long;
    has num64  $.num;
    has int8   $.byte;
    has num32  $.float;
    has CArray $.arr;

    submethod TWEAK {
        $!long = 42;
        $!byte = 7;
        $!num = -3.7e0;
        $!float = 3.14e0;
        my $arr := CArray[long].new();
        $arr[0] = 1;
        $arr[1] = 2;
        $!arr := $arr;
    }

    method clear-array() {
        $!arr := CArray[long];
    }
}

# Workaround a Rakudo-bug where $!arr := CArray[long].new() won't work if $.arr
# is declared as type CArray[long].
class MyStruct2 is repr('CStruct') {
    has long         $.long;
    has num64        $.num;
    has int8         $.byte;
    has num32        $.float;
    has CArray[long] $.arr;
}

class IntStruct is repr('CStruct') {
    has long $.first;
    has long $.second;

    # Work around struct members not being containerized yet.
    submethod TWEAK {
        $!first  = 13;
        $!second = 17;
    }
}

class NumStruct is repr('CStruct') {
    has num64 $.first;
    has num64 $.second;

    # Work around struct members not being containerized yet.
    submethod TWEAK {
        $!first  = 0.9e0;
        $!second = 3.14e0;
    }
}

class StructStruct is repr('CStruct') {
    has IntStruct $.a;
    has NumStruct $.b;

    # Work around struct members not being containerized yet.
    submethod TWEAK {
        $!a := IntStruct.new;
        $!b := NumStruct.new;
    }
}

class StringStruct is repr('CStruct') {
    has Str $.first;
    has Str $.second;

    submethod TWEAK {
        $!first  := 'Lorem';
        $!second := 'ipsum';
    }
}

class PointerThing is repr('CPointer') {
    sub _deref(PointerThing $x) returns long is native('./06-struct') { * }
    method deref() { return _deref(self); }
}

class PointerStruct is repr('CStruct') {
    has PointerThing $.p;
}

class StructIntStruct is repr('CStruct') {
    HAS IntStruct $.a;
    has int32 $.i;
}

class InlinedArrayInStruct is repr('CStruct') {
    has int32 $.a is rw;
    HAS int32 @.b[3] is CArray;
    has int32 $.c is rw;
}

sub ReturnAStruct()            returns MyStruct2 is native('./06-struct') { * }
sub TakeAStruct(MyStruct $arg) returns int32     is native('./06-struct') { * }
sub TakeAStructWithNullCArray(MyStruct $arg) returns int32 is native('./06-struct') { * }

sub ReturnAStructStruct()                returns StructStruct is native('./06-struct') { * }
sub TakeAStructStruct(StructStruct $arg) returns int32        is native('./06-struct') { * }

sub ReturnAPointerStruct() returns PointerStruct is native('./06-struct') { * }

sub ReturnAStringStruct()                returns StringStruct is native('./06-struct') { * }
sub TakeAStringStruct(StringStruct $arg) returns int32        is native('./06-struct') { * }

sub ReturnAStructIntStruct() returns StructIntStruct is native('./06-struct') { * }

sub TakeAInlinedArrayInStruct(InlinedArrayInStruct $s) returns int32 is native('./06-struct') { * };
sub ReturnAInlinedArrayInStruct() returns InlinedArrayInStruct is native('./06-struct') { * };

# Perl-side tests:
my MyStruct $obj .= new;

is $obj.long,   42,     'getting long';
is-approx $obj.num,   -3.7e0,  'getting num';
is $obj.byte,   7,      'getting int8';
is-approx $obj.float,  3.14e0, 'getting num32';
is $obj.arr[1], 2,      'getting CArray and element';

# C-side tests:
my $cobj = ReturnAStruct;

is $cobj.long,   17,      'getting long from C-created struct';
is-approx $cobj.num,    4.2e0,   'getting num from C-created struct';
is $cobj.byte,   13,      'getting int8 from C-created struct';
is-approx $cobj.float,  -6.28e0, 'getting num32 from C-created struct';
is $cobj.arr[0], 2,       'C-created array member, elem 1';
is $cobj.arr[1], 3,       'C-created array member, elem 2';
is $cobj.arr[2], 5,       'C-created array member, elem 3';

my StructStruct $ss = ReturnAStructStruct();
is $ss.a.first,   7, 'field 1 from struct 1 in struct';
is $ss.a.second, 11, 'field 2 from struct 1 in struct';

is-approx $ss.b.first,  3.7e0, 'field 1 from struct 2 in struct';
is-approx $ss.b.second, 0.1e0, 'field 2 from struct 2 in struct';

my PointerStruct $x = ReturnAPointerStruct();
is $x.p.deref, 19, 'CPointer object in struct';

my StringStruct $strstr = ReturnAStringStruct();
is $strstr.first,  'OMG!',     'first string in struct';
is $strstr.second, 'Strings!', 'second string in struct';

is TakeAStruct($obj), 11, 'C-side values in struct';

$obj.clear-array();
is TakeAStructWithNullCArray($obj), 1,
    'Setting a CArray struct element to type object passes a NULL to C';

my StructStruct $ss2 .= new();

is TakeAStructStruct($ss2), 22, 'C-side values in struct struct';

my StringStruct $strstr2 .= new();

#$strstr2.first  := "Lorem";
#$strstr2.second := "ipsum";
is TakeAStringStruct($strstr2), 33, 'C-side strict values in struct';

my StructIntStruct $sis = ReturnAStructIntStruct();
is $sis.i, 42, 'and the int after is 42';
is $sis.a.first, 101, 'nested first is 101';
is $sis.a.second, 77, 'nested second is 77';

# https://github.com/Raku/old-issue-tracker/issues/4763
{
    class A is repr<CStruct> { has int8 $.a };
    class B is repr<CStruct> { has int8 $.b };
    class AB is repr<CStruct> { HAS A $.a; HAS B $.b };

    is nativesizeof(AB), 2, 'struct with inlined structs has correct size';
}

{
    throws-like 'class EmptyCStructTest is repr<CStruct> { };', Exception, message => { m/'no attributes'/ };
}

my $iais = InlinedArrayInStruct.new();
$iais.a    = 1;
$iais.b[0] = 2;
$iais.b[1] = 3;
$iais.b[2] = 4;
$iais.c    = 5;
is TakeAInlinedArrayInStruct($iais), 42, 'Can inline fixed sizes array (1)';

my InlinedArrayInStruct $iais2 = ReturnAInlinedArrayInStruct();
is $iais2.a,    111, 'Can inline fixed sizes array (2)';
is $iais2.b[0], 222, 'Can inline fixed sizes array (3)';
is $iais2.b[1], 333, 'Can inline fixed sizes array (4)';
is $iais2.b[2], 444, 'Can inline fixed sizes array (5)';
is $iais2.c,    555, 'Can inline fixed sizes array (6)';

# vim: expandtab shiftwidth=4
