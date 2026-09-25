use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 115;

# An array variable subscripted by a native int lexical, a native int
# operator result, or a fitting int literal calls AT-POS or ASSIGN-POS
# itself while the setting's subscript is in force.

sub direct(Mu $qast --> Bool:D) {
    (qast-contains-callmethod($qast, 'AT-POS') || qast-contains-callmethod($qast, 'ASSIGN-POS'))
    && !qast-contains-call($qast, '&postcircumfix:<[ ]>')
}
sub general(Mu $qast --> Bool:D) {
    qast-contains-call($qast, '&postcircumfix:<[ ]>')
    && !qast-contains-callmethod($qast, 'AT-POS')
    && !qast-contains-callmethod($qast, 'ASSIGN-POS')
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my @a = 1,2,3; my int $i = 1; my $v = @a[$i]', -> \v {
        qast-contains-callmethod(v, 'AT-POS') and not qast-contains-call(v, '&postcircumfix:<[ ]>')
    }, 'an array subscripted by a native int lexical calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my $v = @a[2]', -> \v { direct(v) },
        'an array subscripted by an int literal calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 1; @a[$i] = 9', -> \v {
        qast-contains-callmethod(v, 'ASSIGN-POS') and not qast-contains-call(v, '&postcircumfix:<[ ]>')
    }, 'an assignment to an array subscripted by a native int lexical calls ASSIGN-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 1; @a[$i] [=] 9', -> \v {
        qast-contains-callmethod(v, 'ASSIGN-POS') and not qast-contains-callmethod(v, 'AT-POS')
    }, 'a bracketed assignment to the subscript calls ASSIGN-POS itself';
    qast-is 'class C1 { has @!a; method m(int $i) { @!a[$i] } }', :full, -> \v { direct(v) },
        'an array attribute subscripted by a native int parameter calls AT-POS itself';
    qast-is 'role R1 { has @!a; method m(int $i) { @!a[$i] } }; class C2 does R1 { }', :full, -> \v { direct(v) },
        'an array attribute of a role calls AT-POS itself';
    qast-is 'class C3 { has int @!a; method m(int $i) { @!a[$i] = 5 } }', :full, -> \v { direct(v) },
        'a native array attribute assignment calls ASSIGN-POS itself';
    qast-is 'sub f(@x, int $i) { @x[$i] }', :full, -> \v { direct(v) },
        'an array parameter calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 1; @a[$i]++', -> \v { direct(v) },
        'a postfix increment on the subscript calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 1; @a[$i] //= 3', -> \v { direct(v) },
        'a defined-or assignment to the subscript calls AT-POS itself';
    qast-is 'my @a = <a b c>; my int $i = 1; @a[$i] .= uc', -> \v { direct(v) },
        'a method assignment on the subscript calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 1; @a[$i];', -> \v { direct(v) },
        'a sunk subscript calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int8 $i = 1; my $v = @a[$i]', -> \v { direct(v) },
        'an int8 index lexical calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my uint $i = 1; my $v = @a[$i]', -> \v { direct(v) },
        'a uint index lexical calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my $v = @a[9223372036854775807]', -> \v { direct(v) },
        'an int literal at the native int limit calls AT-POS itself';

    qast-is 'my @a = 1,2,3; my $v = @a[9223372036854775808]', -> \v { general(v) },
        'an int literal above the native int limit keeps the general call';
    qast-is 'my @a = 1,2,3; my $i = 1; my $v = @a[$i]', -> \v { general(v) },
        'a boxed index keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 0; my $v = @a[$i = 2]', -> \v { general(v) },
        'an assignment as the index keeps the general call';
    qast-is 'sub f(int $i is rw) { my @a; @a[$i] }', :full, -> \v { general(v) },
        'an rw native parameter as the index keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 1; my $v = @a[$i]:exists', -> \v { general(v) },
        'an adverb keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 1; my $v = @a[$i, 0]', -> \v { general(v) },
        'a slice keeps the general call';
    qast-is 'my $s = [1,2,3]; my int $i = 1; my $v = $s[$i]', -> \v { general(v) },
        'a scalar operand keeps the general call';
    qast-is 'class C4 { has @.a; method m(int $i) { @.a[$i] } }', :full, -> \v { general(v) },
        'a public attribute keeps the general call';
    qast-is 'my @a = 1,2,3; my $v = @a[*-1]', -> \v { general(v) },
        'a WhateverCode index keeps the general call';
    qast-is 'sub postcircumfix:<[ ]>(\S, \p) { 42 }; my @a = 1,2,3; my int $i = 1; my $v = @a[$i]', -> \v { general(v) },
        'a user subscript routine in scope keeps the call';
    qast-is 'my @a = 1,2,3; my int $i = 1; my $v = @a[$i]; sub postcircumfix:<[ ]>(\S, \p) { 42 }', -> \v { general(v) },
        'a user subscript routine declared after the use keeps the call';
    qast-is 'sub postcircumfix:<[ ]>(\S, \p) { 42 }; sub inner { my @a = 1,2,3; my int $i = 1; @a[$i] }', :full, -> \v { general(v) },
        'a user subscript routine in an outer scope keeps the call inside a nested sub';
    qast-is 'multi sub postcircumfix:<[ ]>(Str \S, \p) { 42 }; my @a = 1,2,3; my int $i = 1; my $v = @a[$i]', -> \v { general(v) },
        'a user multi candidate in scope keeps the call';
    qast-is 'use soft; my @a = 1,2,3; my int $i = 1; my $v = @a[$i]', -> \v { general(v) },
        'the soft pragma keeps the call';
    qast-is 'my @a; my int $i = 1; @a[$i] := 5', -> \v { general(v) },
        'a binding to the subscript keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 0; my $v = @a[$i + 1]', -> \v {
        direct(v) and qast-contains-op(v, 'decont') and not qast-contains-op(v, 'box_i')
    }, 'an array subscripted by a native int sum calls AT-POS itself with the result as an object';
    qast-is 'my @a = 1,2,3; my int $i = 0; my $v = @a[++$i]', -> \v { direct(v) },
        'an array subscripted by a native int prefix increment calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 0; my $v = @a[$i++]', -> \v { general(v) },
        'an array subscripted by a native int postfix increment keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 0; @a[$i + 1]++', -> \v { direct(v) },
        'a postfix increment on a subscript by a native int sum calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 0; @a[++$i] += 2', -> \v { direct(v) },
        'a metaop assignment to a subscript by a native int prefix increment calls AT-POS itself';
    qast-is 'my int @n = 1,2,3; my int $i = 0; my $v = @n[$i + 1]', -> \v {
        direct(v) and not qast-contains-op(v, 'atposref_i')
    }, 'a native int array subscripted by a native int sum calls AT-POS itself';
    qast-is 'sub g(int $i is rw) { my @a = 1,2,3; @a[$i + 1] }', :full, -> \v { direct(v) },
        'a sum of a native rw parameter is a value and calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 1; @a[$i - 1] = 7', -> \v {
        qast-contains-callmethod(v, 'ASSIGN-POS') and not qast-contains-call(v, '&postcircumfix:<[ ]>')
    }, 'an assignment through a native int difference calls ASSIGN-POS itself';
    qast-is 'my @a = 1,2,3; my int $i = 0; my $v = @a[$i * 2]', -> \v { direct(v) },
        'an array subscripted by a native int product calls AT-POS itself';
    qast-is 'class PA2 { has @!a; method m(int $i) { @!a[$i + 1] } }', :full, -> \v { direct(v) },
        'an array attribute subscripted by a native int sum calls AT-POS itself';
    qast-is 'my @a = 1,2,3; my $b = 0; my $v = @a[$b + 1]', -> \v { general(v) },
        'an array subscripted by a boxed sum keeps the general call';
    qast-is 'my @a = 1,2,3; my num $n = 1e0; my $v = @a[$n + 1e0]', -> \v { general(v) },
        'an array subscripted by a native num sum keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 0; my $v = @a[$i + 1]:exists', -> \v { general(v) },
        'an array subscripted by a native int sum with an adverb keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 0; my $v = @a[($i)]', -> \v { general(v) },
        'an array subscripted by a parenthesized native int lexical keeps the general call';
    qast-is 'my @a = 1,2,3; my $v = @a[int]', -> \v { general(v) },
        'an array subscripted by a native type object keeps the general call';
    qast-is 'sub infix:<idx>(int $a, int $b --> int) { $a + $b }; my @a = 1,2,3; my int $i = 1; my $v = @a[$i idx 1]', -> \v { general(v) },
        'a user operator with a native int return keeps the general call';
    qast-is 'sub prefix:<idx>(int $a --> int) { $a }; my @a = 1,2,3; my int $i = 1; my $v = @a[idx $i]', -> \v { general(v) },
        'a user prefix operator with a native int return keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 1; my $f = 0; my $v = @a[$i if $f]', -> \v { general(v) },
        'an index under a condition modifier keeps the general call';
    qast-is 'my @a = 1,2,3; my int $i = 1; my $v = @a[$i for ^2]', -> \v { general(v) },
        'an index under a loop modifier keeps the general call';
}
else {
    skip 'the subscript shapes are specific to the RakuAST frontend', 49;
}

{
    my @a = 1,2,3; my int $i = 1;
    is @a[$i], 2, 'a native int lexical reads the element';
    is @a[2], 3, 'an int literal reads the element';
    my int $big = 7;
    ok !@a[$big].defined, 'an index past the end reads an undefined value';
    is @a.elems, 3, 'an index past the end does not extend the array';
    ok !@a[9223372036854775807].defined, 'the native int limit as a literal index reads undefined';
    is @a.elems, 3, 'the native int limit as a literal index does not extend the array';
    my int $n = -1;
    throws-like { @a[$n] }, X::OutOfRange, 'a negative native int index reports the range error';
    my int8 $s = 1;
    is @a[$s], 2, 'an int8 index reads the element';
    my uint $u = 2;
    is @a[$u], 3, 'a uint index reads the element';
    @a[$u] = 30;
    is @a[2], 30, 'a uint index assigns the element';
}
{
    my @a = 1,2,3; my int $i = 1;
    @a[$i] = 9;
    is @a[1], 9, 'an assignment through a native int lexical stores the element';
    my int $big = 5;
    @a[$big] = 4;
    is @a.elems, 6, 'an assignment past the end extends the array';
    (@a[$i] = 20)++;
    is @a[1], 21, 'an assignment yields the element container';
    my $v = (@a[$i] = 77);
    is $v, 77, 'an assignment to the subscript yields the value';
    @a[$i] [=] 8;
    is @a[1], 8, 'a bracketed assignment stores the element';
}
{
    my @a = 1,2,3; my int $i = 1;
    @a[$i]++;
    is @a[1], 3, 'a postfix increment on the subscript updates the element';
    my @b = 1, Any, 3;
    @b[$i] //= 9;
    is @b[1], 9, 'a defined-or assignment fills the undefined element';
    my @c = <a b c>;
    @c[$i] .= uc;
    is @c[1], 'B', 'a method assignment on the subscript updates the element';
    my @d = 1,2,3; my int $j = 5;
    @d[$j]++;
    is @d[5], 1, 'a postfix increment past the end stores the stepped value';
    is @d.elems, 6, 'a postfix increment past the end grows the array by one';
}
{
    my Int @a = 1,2,3; my int $i = 1;
    throws-like { @a[$i] = "x" }, X::TypeCheck::Assignment, 'a typed array assignment through a native int reports the type error';
    is @a[$i], 2, 'the failed typed assignment leaves the element alone';
    my @b is default(42) = 1,2; my int $j = 5;
    is @b[$j], 42, 'a default array reads the default past the end through a native int';
    is @b[7], 42, 'a default array reads the default past the end through an int literal';
}
{
    sub f(@x, int $i) { @x[$i] }
    my int $i = 2;
    is f(1..5, $i), 3, 'a Range bound to an array parameter reads through a native int';
    is f((1,2,3,4).Seq, $i), 3, 'a Seq bound to an array parameter reads through a native int';
    is f((10,20,30), $i), 30, 'a List bound to an array parameter reads through a native int';
    my int @n = 7,8,9;
    is f(@n, $i), 9, 'a native int array bound to an array parameter reads through a native int';
    my int $big = 9;
    ok !f(1..5, $big).defined, 'a Range read past the end through an array parameter is undefined';
}
{
    my @lazy = 1..*; my int $i = 6;
    is @lazy[$i], 7, 'a lazy array reifies up to a native int index';
}
{
    my @s[3]; my int $i = 1;
    @s[$i] = 4;
    is @s[$i], 4, 'a shaped array assigns and reads through a native int index';
}
{
    my int $j = 3; my @w := [1,2,3,4,5];
    is @w[$j], 4, 'an array bound to a variable reads through a native int index';
}
{
    my @a is Failure; my int $i = 0;
    ok @a[$i] =:= @a, 'an array variable that is a Failure answers the subscript with itself';
}
{
    class W1 does Positional { method AT-POS(int $p is rw) { $p = 99; 1 } }
    my int $i = 1; my @w := W1.new;
    @w[$i];
    is $i, 99, 'a subscript on an array variable passes a native index by reference';
    if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
        class W2 does Positional { method ASSIGN-POS(int $p is rw, $v) { $p = 77; $v } }
        my int $j = 1; my @v := W2.new;
        @v[$j] = 5;
        is $j, 77, 'an assignment through an array variable passes a native index by reference';
        class W3 does Positional { has @!s; method AT-POS($i) { @!s[$i] }; method ASSIGN-POS($i, $v) { @!s[$i] = $v * 2 } }
        my @p := W3.new; my int $k = 1;
        @p[$k] [=] 7;
        is @p[$k], 14, 'a bracketed assignment through an array variable calls ASSIGN-POS';
    }
    else {
        skip 'the legacy frontend does not reach ASSIGN-POS for an rw native index or a bracketed assignment', 2;
    }
}
{
    class H1 {
        has @!a;
        method set(@x) { @!a := @x; self }
        method g(int $k) { @!a[$k] }
        method s(int $k, $v) { @!a[$k] = $v; @!a[$k] }
    }
    my $h = H1.new.set([10, 20, 30]); my int $i = 1;
    is $h.g($i), 20, 'an array attribute reads through a native int parameter';
    is $h.s($i, 7), 7, 'an array attribute assigns through a native int parameter';
}
{
    role R2 { has @!a; method set(@x) { @!a := @x; self }; method g(int $k) { @!a[$k] }; method s(int $k, $v) { @!a[$k] = $v; @!a[$k] } }
    class RC2 does R2 { }
    my $c = RC2.new.set([5,6,7]); my int $i = 2;
    is $c.g($i), 7, 'an array attribute from a role reads through a native int parameter';
    is $c.s($i, 8), 8, 'an array attribute from a role assigns through a native int parameter';
}
{
    class N1 { has int @!a; method set(@x) { @!a = @x; self }; method g(int $k) { @!a[$k] }; method s(int $k, int $v) { @!a[$k] = $v; @!a[$k] } }
    my $n = N1.new.set([1,2,3]); my int $i = 1;
    is $n.g($i), 2, 'a native array attribute reads through a native int parameter';
    is $n.s($i, 9), 9, 'a native array attribute assigns through a native int parameter';
}
{
    class V1 { has @!x; method fill() { nqp::bindattr(self, V1, '@!x', nqp::list(7, 8, 9)); self }; method g(int $k) { @!x[$k] } }
    my int $i = 1;
    is V1.new.fill.g($i), 8, 'an array attribute holding a raw VM array reads through a native int parameter';
}
{
    class PA1 { has @.a; method g(int $i) { @.a[$i] } }
    my int $i = 1;
    is PA1.new(a => [4,5,6]).g($i), 5, 'a public attribute reads through a native int parameter';
}
{
    my @a = 1,2,3; my int $i = 0;
    my $v = @a[$i = 2];
    is $v, 3, 'an assignment as the index reads the element it names';
}
{
    my @a = 1,2,3; my int $i = 1;
    is @a[$i + 1], 3, 'a native int sum as index reads the element';
    is @a[++$i], 3, 'a native int increment as index reads the element after stepping';
    is $i, 2, 'the increment in the index updates the variable';
    @a[$i - 1] = 7;
    is-deeply @a, [1, 7, 3], 'an assignment through a native int difference stores the element';
    @a[$i + 3] = 'far';
    is @a.elems, 6, 'an assignment through a native int sum past the end extends the array';
    throws-like { @a[$i - 5] }, X::OutOfRange, 'a negative native int difference reports the range error';
}
{
    class RW1 does Positional {
        method AT-POS(int $p is rw) { $p = 42; 'v' }
    }
    my @c := RW1.new; my int $i = 1;
    throws-like { @c[$i + 1] }, X::Parameter::RW,
        'a native int sum as index to an rw native AT-POS reports the missing container';
}
{
    my int @n = 1,2,3; my int $i = 0;
    is @n[++$i], 2, 'a native int array subscripted by a prefix increment reads the element';
    is $i, 1, 'the increment in a native array index runs once';
    my @b = 1,2,3; my int $o = 0;
    @b[++$o] = $o;
    is-deeply @b, [1, 1, 3], 'the index is evaluated before the assigned value';
    my @a = 1,2,3; my int $j = 0;
    @a[$j + 1]++;
    is @a[1], 3, 'a postfix increment on a subscript by a sum updates the element';
    @a[++$j] += 2;
    is-deeply @a, [1, 5, 3], 'a metaop assignment through a prefix increment updates the element once';
    is $j, 1, 'the increment in a metaop assignment index runs once';
    class T1 does Positional {
        method AT-POS($p) { $p.^name }
    }
    my @t := T1.new;
    is @t[$j + 1], 'Int', 'a native int sum reaches a user AT-POS as an Int object';
}
{
    my @a = 10,20,30; my int $i = 1; my $f = 0;
    is-deeply @a[$i if $f], (), 'an index under a false condition modifier selects nothing';
    is-deeply @a[1 if $f], (), 'a literal index under a false condition modifier selects nothing';
    is-deeply @a[$i for ^2], (20, 20), 'an index under a loop modifier selects per iteration';
}
{
    class RW2 does Positional {
        method AT-POS(int $p is rw) { $p = 42; 'v' }
    }
    my @c := RW2.new; my int $i = 1;
    is @c[($i)], 'v', 'a parenthesized native int lexical reaches an rw native AT-POS';
    is $i, 42, 'the rw native AT-POS wrote through the parenthesized lexical';
    sub infix:<idx>(int $a, int $b --> int) { fail "custom failure" }
    my @a = 10,20,30;
    throws-like { @a[$i idx 1] }, X::AdHoc, message => 'custom failure',
        'a native int operator that fails reports its own failure from the subscript';
    sub infix:<nil>(int $a, int $b --> int) { Nil }
    throws-like { @a[$i nil 1] }, Exception, message => /'Indexing requires a defined object'/,
        'a native int operator returning Nil reports the subscript error for a type object';
}
