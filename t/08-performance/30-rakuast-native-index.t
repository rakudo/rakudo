use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 72;

# A subscript of a plain native array by an int literal or a native
# int lexical compiles to a raw position op. A read emits the element
# reference the general call returns, so writable contexts keep
# working, and a sunk assignment of a native value binds the element
# directly. A negative index takes the general call, whose candidates
# own that error. The shapes are this frontend's.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my int @a = 1,2,3; my int $i = 1; my $v = @a[$i]', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
        and qast-contains-op(v, 'islt_i')
    }, 'a native int subscript by a native int lexical reads through the raw op behind a sign guard';

    qast-is 'my int @a = 1,2,3; my $v = @a[1]', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
        and not qast-contains-op(v, 'islt_i')
    }, 'a native int subscript by an int literal reads through the raw op with no guard';

    qast-is 'my num @n = 1e0, 2e0; my $v = @n[1]', :full, -> \v {
        qast-contains-op(v, 'atposref_n')
    }, 'a native num subscript reads through the raw op';

    qast-is 'my str @s = <x y>; my $v = @s[1]', :full, -> \v {
        qast-contains-op(v, 'atposref_s')
    }, 'a native str subscript reads through the raw op';

    qast-is 'my int @a; my int $i = 0; @a[$i] = 5; 1;', :full, -> \v {
        qast-contains-op(v, 'bindpos_i')
    }, 'a sunk native assignment binds the element through the raw op';

    qast-is 'my int @a; my int $i = 0; my $x = (@a[$i] = 5);', :full, -> \v {
        not qast-contains-op(v, 'bindpos_i')
    }, 'an assignment whose result is used keeps the general call';

    qast-is 'my @a = 1,2,3; my $v = @a[1]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript of a boxed array keeps the general call';

    qast-is 'my int @a = 1,2,3; my $i = 1; my $v = @a[$i]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a boxed index keeps the general call';

    qast-is 'my int @a = 1,2,3; my $v = @a[1]:exists', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript with an adverb keeps the general call';

    qast-is 'my int @a[3]; my $v = @a[1]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript of a shaped array keeps the general call';

    qast-is 'my int @a = 1,2,3; my $v = @a[1-2]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a folded negative literal keeps the general call';

    qast-is 'my int @a = 1,2,3; my $v = @a[10000000000000000000]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a literal too big for a native int keeps the general call';

    qast-is 'use soft; my int @a = 1,2,3; my $v = @a[1]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript under the soft pragma keeps the general call';

    qast-is 'sub f(int @a) { @a[0] }; my int @b = 1,2; f(@b)', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript of a parameter array keeps the general call';

    qast-is 'my int @a = 1,2,3; my int @b; @a := @b; my $v = @a[1]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript of a declaration a bind statement targets keeps the general call';

    qast-is 'my num @n; my int $v = 1; my int $i = 0; @n[$i] = $v; 1', :full, -> \v {
        not qast-contains-op(v, 'bindpos_n')
    }, 'an assignment of a value of another native kind keeps the general call';

    qast-is 'my int8 @t = 1,2; my $v = @t[1]', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
    }, 'a sized int array subscript reads through the raw op';

    qast-is 'my uint @u = 1,2; my $v = @u[1]', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'an unsigned array subscript keeps the general call';

    qast-is 'my int @a; for ^10 -> int $i { @a[$i] = 5 }; 1', :full, -> \v {
        qast-contains-op(v, 'bindpos_i')
    }, 'a loop body assignment by the native int loop parameter binds through the raw op';

    qast-is 'my int @a; my sub f(int $i) { my $v = @a[$i]; 1 }; f(2)', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a native int parameter reads through the raw op';

    qast-is 'my int @a; my sub f(int $i, int $v) { @a[$i] = $v; 1 }; f(0, 1)', :full, -> \v {
        qast-contains-op(v, 'bindpos_i')
    }, 'an assignment of a native int parameter binds through the raw op';

    qast-is 'my int @a; my sub f(int $i is rw) { my $v = @a[$i]; 1 }; my int $x = 1; f($x)', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript by an rw parameter keeps the general call';

    qast-is 'my int @a; my sub f(Int $i) { my $v = @a[$i]; 1 }; f(2)', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a boxed parameter keeps the general call';

    qast-is 'my num @n; my sub f(int $i, num $v) { @n[$i] = $v; 1 }; f(0, 1e0)', :full, -> \v {
        qast-contains-op(v, 'bindpos_n')
    }, 'an assignment of a native num parameter binds through the raw op';

    qast-is 'my str @s; my sub f(int $i, str $v) { @s[$i] = $v; 1 }; f(0, "x")', :full, -> \v {
        qast-contains-op(v, 'bindpos_s')
    }, 'an assignment of a native str parameter binds through the raw op';

    qast-is 'my int @a; my sub f(num $i) { my $v = @a[$i]; 1 }; f(1e0)', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a native num parameter keeps the general call';

    qast-is 'my int @a; my sub f(int $v is rw) { @a[0] = $v; 1 }; my int $x = 1; f($x)', :full, -> \v {
        not qast-contains-op(v, 'bindpos_i')
    }, 'an assignment of an rw parameter value keeps the general call';

    qast-is 'my int @a; my sub f(int $i = 1) { my $v = @a[$i]; 1 }; f()', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
    }, 'a subscript by an optional parameter with a default reads through the raw op';

    qast-is 'my int @a; my sub f(int $i is raw) { my $v = @a[$i]; 1 }; my int $x = 1; f($x)', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a raw parameter reads through the raw op';

    qast-is 'my int @a; my sub f(int $i is copy) { my $v = @a[$i]; 1 }; f(1)', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a copy parameter reads through the raw op';

    qast-is 'my int @a = 1,2,3; for ^3 -> int $i { my $v = @a[$i] }', :full, -> \v {
        qast-contains-op(v, 'atposref_i')
    }, 'a loop body read by the native int loop parameter reads through the raw op';

    qast-is 'my int @a; my int @b = 1,2; for @b <-> int $i { my $v = @a[$i] }', :full, -> \v {
        not qast-contains-op(v, 'atposref_i')
    }, 'a subscript by a loop parameter that can be bound keeps the general call';
}
else {
    skip 'the subscript shapes are specific to the RakuAST frontend', 32;
}

# Behavior stays identical.

{
    my int @a = 10, 20, 30;
    my int $i = 1;
    is @a[$i], 20, 'a raw read by a native lexical gives the element';
    is @a[0], 10, 'a raw read by a literal gives the element';
    is @a[9], 0, 'a raw read past the end gives the type default';
    @a[$i] = 99;
    is @a[1], 99, 'a raw bind stores the element';
    @a[5] = 7;
    is @a[5], 7, 'a raw bind past the end extends the array';
    is @a.elems, 6, 'the extension sets the element count';
    my int $neg = -1;
    dies-ok { my $v = @a[$neg] }, 'a negative index read dies through the general call';
    dies-ok { @a[$neg] = 1 }, 'a negative index write dies through the general call';
    my $copy = @a[1];
    $copy = 42;
    is @a[1], 99, 'assigning a copy read from the element leaves the element alone';
    sub bump($x is rw) { $x++ }
    bump(@a[0]);
    is @a[0], 11, 'a writable context writes through the element reference';
    my $x = (@a[0] = 77);
    is $x, 77, 'an assignment whose result is used yields the value';
    is @a[0], 77, 'an assignment whose result is used still stores';
}

{
    my num @n = 1e0, 2e5;
    is @n[1], 2e5, 'a raw num read gives the element';
    my int $j = 0;
    @n[$j] = 9e0;
    is @n[0], 9e0, 'a raw num bind stores the element';
}

{
    my str @s = <x y z>;
    my int $j = 1;
    @s[$j] = 'q';
    is @s[1], 'q', 'a raw str bind stores the element';
}

{
    my int @sh[3];
    @sh[0] = 1;
    is @sh[0], 1, 'a shaped array subscript computes through the general call';
}

# A folded negative literal and an oversized literal decline the raw
# op, so the general call raises the errors it owns.

{
    my int @a = 1, 2, 3;
    dies-ok { my $v = @a[1-2] }, 'a folded negative literal read dies through the general call';
    dies-ok { @a[1-2] = 99 }, 'a folded negative literal write dies through the general call';
    is @a[2], 3, 'the failing write left the last element alone';
    dies-ok { my $v = @a[10000000000000000000] },
        'a literal too big for a native int dies through the general call';
}

# A bind statement replaces the container, so the subscript dispatches
# to whatever was bound.

{
    my class P does Positional[int] {
        method AT-POS($i) { 42 }
        method of() { int }
    }
    my int @a = 1, 2, 3;
    @a := P.new;
    is @a[1], 42, 'a subscript after a bind statement dispatches to the bound container';
}

{
    my sub postcircumfix:<[ ]>(\a, \i) { 42 }
    my int @a = 1, 2, 3;
    my int $i = 0;
    is @a[$i], 42, 'a lexical subscript routine intercepts the subscript';
}

# A value of another native kind keeps the general call, whose
# assignment owns the type error.

{
    my num @n;
    my int $iv = 1;
    my int $i = 0;
    dies-ok { @n[$i] = $iv }, 'assigning an int lexical to a num element dies through the general call';
    my int @a;
    dies-ok { @a[$i] = 1.5 }, 'assigning a fractional literal to an int element dies through the general call';
}

{
    my int8 @t;
    my int $i = 0;
    @t[$i] = 300;
    is @t[0], 44, 'a sized int element truncates through the raw op as the general call does';
}

# A native int parameter reads from a native slot as a plain
# declaration does, so it serves as an index and as an assigned
# value. An rw parameter reads through a reference and takes the
# general call.

{
    my int @a;
    for ^5 -> int $i { @a[$i] = $i * 2 }
    is-deeply @a, (my int @ = 0, 2, 4, 6, 8), 'a loop with a native int parameter fills the array';
    my sub read(int $i) { @a[$i] }
    is read(3), 6, 'a subscript by a parameter gives the element';
    my sub write(int $i, int $v) { @a[$i] = $v; Nil }
    write(0, 9);
    is @a[0], 9, 'an assignment of a parameter value stores the element';
    dies-ok { write(-1, 1) }, 'a negative parameter index dies through the general call';
    my sub readraw(int $i is raw) { @a[$i] }
    is readraw(1), 2, 'a subscript by a raw parameter gives the element';
    my sub readcopy(int $i is copy) { $i = $i + 1; @a[$i] }
    is readcopy(2), 6, 'a subscript by a modified copy parameter reads the new position';
    my sub readrw(int $i is rw) { my $v = @a[$i]; $i = 0; $v }
    my int $x = 4;
    is readrw($x), 8, 'a subscript by an rw parameter computes through the general call';
    is $x, 0, 'the rw parameter still writes back to the caller';
}

{
    my num @n;
    my sub fillnum(int $i, num $v) { @n[$i] = $v; Nil }
    fillnum(0, 3.5e0);
    is @n[0], 3.5e0, 'an assignment of a num parameter value stores the element';
    my str @s;
    my sub fillstr(int $i, str $v) { @s[$i] = $v; Nil }
    fillstr(0, 'xy');
    is @s[0], 'xy', 'an assignment of a str parameter value stores the element';
}

{
    my int @a = 7, 8, 9;
    my sub readopt(int $i = 1) { @a[$i] }
    is readopt(), 8, 'a subscript by the parameter default reads that position';
    is readopt(2), 9, 'a subscript by a passed optional parameter reads that position';
}

{
    my int @a = 5, 6, 7;
    my int $i = 1;
    @a[$i] += 5;
    is @a[1], 11, 'a compound assignment writes through the element reference';
    @a[$i]++;
    is @a[1], 12, 'an increment writes through the element reference';
    my $before = @a.elems;
    my $v = @a[9];
    is @a.elems, $before, 'a read past the end does not extend the array';
}

# vim: expandtab shiftwidth=4
