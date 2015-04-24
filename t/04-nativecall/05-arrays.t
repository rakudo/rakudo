use lib 't/04-nativecall';
use CompileTestLib;
use lib 'lib';
use NativeCall;
use Test;

plan 23;

compile_test_lib('05-arrays');

{
    sub ReturnADoubleArray() returns CArray[num] is native("./05-arrays") { * }
    my @rarr := ReturnADoubleArray();
    is_approx @rarr[0], 23.45e0, 'returning double array (1)';
    is_approx @rarr[1], -99.87e0, 'returning double array (2)';
    is_approx @rarr[2], 0.25e0, 'returning double array (3)';

    sub TakeADoubleArrayAndAddElements(CArray[num]) returns num is native("./05-arrays") { * }
    my @parr := CArray[num].new();
    @parr[0] = 9.5e0;
    @parr[1] = 32.5e0;
    is_approx TakeADoubleArrayAndAddElements(@parr), 42e0, 'passing double array';
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

    is_deeply @arr[100], Struct, 'out-of-bounds access on managed array';

    is TakeAStructArray(@arr), 14, 'struct in position 0..2, C-side';
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
    is_approx @rarr[0], 1.23e0, 'float in element 0';
    is_approx @rarr[1], 4.56e0, 'float in element 1';
    is_approx @rarr[2], 7.89e0, 'float in element 2';

    sub SumAFloatArray(CArray[num32]) returns num32 is native("./05-arrays") { * }
    my @parr := CArray[num32].new;
    @parr[0] = 12.3e0;
    @parr[1] = 45.6e0;
    is_approx SumAFloatArray(@parr), 57.9e0, 'sum of float array';
}

# vim:ft=perl6
