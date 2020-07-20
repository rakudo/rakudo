use v6;

use lib <lib t/04-nativecall>;
use CompileTestLib;
use NativeCall;
use Test;

plan 46;

compile_test_lib('05-arrays');

{
    sub ReturnADoubleArray() returns CArray[num64] is native("./05-arrays") { * }
    my @rarr := ReturnADoubleArray();
    is-approx @rarr[0], 23.45e0, 'returning double array (1)';
    is-approx @rarr[1], -99.87e0, 'returning double array (2)';
    is-approx @rarr[2], 0.25e0, 'returning double array (3)';

    sub TakeADoubleArrayAndAddElements(CArray[num64]) returns num64 is native("./05-arrays") { * }
    my @parr := CArray[num].new();
    @parr[0] = 9.5e0;
    @parr[1] = 32.5e0;
    is-approx TakeADoubleArrayAndAddElements(@parr), 42e0, 'passing double array';
}

{
    sub ReturnAStringArray() returns CArray[Str] is native("./05-arrays") { * }
    my @rarr := ReturnAStringArray();
    is @rarr[0], 'La Trappe', 'returning string array (1)';
    is @rarr[1], 'Leffe', 'returning string array (2)';

    sub TakeAStringArrayAndReturnTotalLength(CArray[Str]) returns int32 is native("./05-arrays") { * }
    my @parr := CArray[Str].new();
    @parr[0] = "OMG";
    @parr[1] = "strings!!!";
    is TakeAStringArrayAndReturnTotalLength(@parr), 13, 'passing string array';
}

{
    my @arr := CArray[int].new();
    @arr[0] = 1;
    is @arr[0], 1, 'getting last element of managed array';
}

{
    my @arr := CArray[Pointer].new;
    @arr[1] = Pointer.new;
    my $x = @arr[0];
    pass 'getting uninitialized element in managed array';
}

{
    my @arr := CArray[int8].new(1, 2, 3, 4, 5);
    is @arr.elems, 5, "CArray.elems works (int)";
    is @arr[0], 1, ".new with values creates an array containing those values (int) - 1";
    is @arr[1], 2, ".new with values creates an array containing those values (int) - 2";
    is @arr[2], 3, ".new with values creates an array containing those values (int) - 3";
    is @arr[3], 4, ".new with values creates an array containing those values (int) - 4";
}

{
    my @arr := CArray[num].new(0.1e0, 0.2e0, 0.3e0, 0.4e0, 0.5e0);
    is @arr.elems, 5, "CArray.elems works (num)";
    is @arr[0], 0.1e0, ".new with values creates an array containing those values (num) - 1";
    is @arr[1], 0.2e0, ".new with values creates an array containing those values (num) - 2";
    is @arr[2], 0.3e0, ".new with values creates an array containing those values (num) - 3";
    is @arr[3], 0.4e0, ".new with values creates an array containing those values (num) - 4";
}

{
    class Struct is repr('CStruct') {
        has long $.val;

        method set($x) {
            $!val = $x;
        }
    }

    sub ReturnAStructArray() returns CArray[Struct] is native("./05-arrays") { * }
    my @arr := ReturnAStructArray();
    is @arr[0].val, 2, 'long in struct in element 0';
    is @arr[1].val, 3, 'long in struct in element 1';
    is @arr[2].val, 5, 'long in struct in element 2';

    sub TakeAStructArray(CArray[Struct] $obj) returns int32 is native("./05-arrays") { * }
    @arr := CArray[Struct].new;
    @arr[0] = Struct.new();
    @arr[1] = Struct.new();
    @arr[2] = Struct.new();
    @arr[0].set(7);
    @arr[1].set(11);
    @arr[2].set(13);

    is-deeply @arr[100], Struct, 'out-of-bounds access on managed array';

    is TakeAStructArray(@arr), 14, 'struct in position 0..2, C-side';

    sub TakeAStructArrayWithANull(CArray[Struct] $obj) returns int32 is native("./05-arrays") { * }
    @arr[1] = Struct;
    is TakeAStructArrayWithANull(@arr), 1,
        'Setting a type object in the array passes a NULL to the C side';
}

{
    sub ReturnsAByteArray() returns CArray[int8] is native("./05-arrays") { * }
    my @rarr := ReturnsAByteArray();
    is @rarr[0], 100, 'byte in element 0';
    is @rarr[1], 90,  'byte in element 1';
    is @rarr[2], 80,  'byte in element 2';

    sub TakeAByteArray(CArray[int8]) returns int32 is native("./05-arrays") { * }
    my @parr := CArray[int8].new;
    @parr[0] = 31;
    @parr[1] = 28;
    @parr[2] = 30;
    is TakeAByteArray(@parr), 18, 'byte in position 0..2, C-side';
}

{
    sub TakeAByteArray(Buf) returns int32 is native("./05-arrays") { * }
    my $buf = buf8.new;
    $buf[0] = 31;
    $buf[1] = 28;
    $buf[2] = 30;
    is TakeAByteArray($buf), 18, 'byte in position 0..2, C-side';
}

{
    sub ReturnsAFloatArray() returns CArray[num32] is native("./05-arrays") { * }
    my @rarr := ReturnsAFloatArray();
    is-approx @rarr[0], 1.23e0, 'float in element 0';
    is-approx @rarr[1], 4.56e0, 'float in element 1';
    is-approx @rarr[2], 7.89e0, 'float in element 2';

    sub SumAFloatArray(CArray[num32]) returns num32 is native("./05-arrays") { * }
    my @parr := CArray[num32].new;
    @parr[0] = 12.3e0;
    @parr[1] = 45.6e0;
    is-approx SumAFloatArray(@parr), 57.9e0, 'sum of float array';
}

# https://github.com/Raku/old-issue-tracker/issues/5663
{
    is CArray[uint8].new.elems, 0, 'creating CArray with no arguments works';
    is CArray[uint8].new(()).elems, 0,
        'creating CArray with () as argument does not hang';
    is-deeply CArray[uint8].new(1, 2, 3)[^3], (1, 2, 3),
        'creating CArray with several positionals works';

    my @arg = 1..3;
    is-deeply CArray[uint8].new(@arg)[^3], (1, 2, 3),
        'creating CArray with one Positional positional works';
}

# https://github.com/Raku/old-issue-tracker/issues/5859
{
    my CArray[uint8] $a .= new(200 xx 16);
    todo "RT #130267";
    is $a[0], 200, 'unsigned uint8 value';
}

# https://github.com/Raku/old-issue-tracker/issues/6421
lives-ok { CArray[Str].new[my int $ = 1] },
    'native int as index to CArray does not crash';

# https://github.com/Raku/old-issue-tracker/issues/6295
is CArray[Pointer].^shortname, 'CArray[Pointer]',
    'CArray.^shortname shows sane value';

subtest 'CArray allocation' => {
    plan 3;

    is-deeply CArray[int32].allocate(42).list, 0 xx 42,
        'Allocation works with Int typed CArray';

    is-deeply CArray[num32].allocate(42).list, 0e0 xx 42,
        'Allocation works with Num typed CArray';

    is-deeply CArray[Pointer].allocate(42).list, Pointer.new xx 42,
        'Allocation works with miscellaneously typed CArray';
}

# https://github.com/rakudo/rakudo/issues/2687
{
    class Data is repr('CStruct') {
        has CArray[num32] $.data-in;

        submethod BUILD(CArray :$data-in) {
            $!data-in := $data-in;
        }
    }

    lives-ok { Data.new(data-in => CArray[num32].new) },
      'can we build with a CArray attribute';
}

# https://github.com/rakudo/rakudo/issues/2681
{
    is-deeply .new(CArray[uint8].new(1,2,3)), .new(1,2,3),
      "can we create a {.^name} from a CArray"
      for Buf, Blob;
}

# https://github.com/rakudo/rakudo/issues/2682
{
    my @t2682 := CArray[int32].new(1,2,3);
    is-deeply @t2682[0..*-2], (1, 2), 'Indexing with WhateverStar works on CArray';
}

# vim: expandtab shiftwidth=4
