use Test;

plan 6;

is EVAL(q:to/CODE/), 20,
    use NativeCall;
    my class A is repr<CStruct> { HAS int32 @.a[(4, 1)[0]] is CArray; has int32 $.b }
    nativesizeof(A)
    CODE
    'an inline array shape indexing a list sizes the array';

is EVAL(q:to/CODE/), 20,
    use NativeCall;
    my class A is repr<CStruct> { HAS int32 @.a[do { 4 }] is CArray; has int32 $.b }
    nativesizeof(A)
    CODE
    'an inline array shape with a do block sizes the array';

is EVAL(q:to/CODE/), 20,
    use NativeCall;
    my class A is repr<CStruct> { HAS int32 @.a[4 if True] is CArray; has int32 $.b }
    nativesizeof(A)
    CODE
    'an inline array shape with a statement modifier sizes the array';

is EVAL(q:to/CODE/), 1,
    use NativeCall;
    my $calls;
    BEGIN $calls = 0;
    my sub elems() { $calls++; 4 }
    my class A is repr<CStruct> { HAS int32 @.a[elems()] is CArray; has int32 $.b }
    BEGIN $calls
    CODE
    'an inline array shape is evaluated once';

is EVAL(q:to/CODE/), '7 9',
    use NativeCall;
    my class A is repr<CStruct> { HAS int32 @.a[(4, 1)[0]] is CArray; has int32 $.b }
    my $a = A.new(:b(9));
    $a.a[3] = 7;
    "$a.a()[3] $a.b()"
    CODE
    'an inline array with an indexed shape holds its last element';

throws-like q:to/CODE/, Exception, message => /boom/,
    use NativeCall;
    my class A is repr<CStruct> { HAS int32 @.a[die "boom"] is CArray; has int32 $.b }
    CODE
    'an inline array shape that dies reports its own error';

# vim: expandtab shiftwidth=4
