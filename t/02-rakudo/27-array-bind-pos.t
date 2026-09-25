use Test;
plan 33;

# Binding into a slot that already exists keeps the bound container.
{
    my @a = 1, 2, 3;
    my $x = 42;
    @a.BIND-POS(1, $x);
    is-deeply @a, [1, 42, 3], 'binding into a reified slot replaces the element';
    $x = 43;
    is @a[1], 43, 'the slot holds the bound container itself';
}

# Binding past the end extends the array, with holes in between.
{
    my @a = 1, 2, 3;
    @a.BIND-POS(5, 9);
    is @a.elems, 6, 'binding past the end extends the array';
    is @a[5], 9, 'the element past the end is bound';
    nok @a[4]:exists, 'the slots in between are holes';
}

# An array without any elements yet gets its storage on the first bind.
{
    my @e;
    @e.BIND-POS(2, 5);
    is-deeply @e, [Any, Any, 5], 'binding into an empty array creates its storage';
    nok @e[0]:exists, 'the slots before it are holes';
}

# A lazy array reifies up to the bound position first.
{
    my @l = 1..*;
    @l.BIND-POS(3, 7);
    is-deeply @l[^5], (1, 2, 3, 7, 5), 'binding into a lazy array reifies up to the position';
    ok @l.is-lazy, 'the array stays lazy after the bind';
    my @b = 1..*;
    @b[1];
    @b.BIND-POS(2, 'x');
    is-deeply @b[^5], (1, 2, 'x', 4, 5), 'binding at the element count of a lazy array reifies that position';
    my @m = 1..*;
    @m[3];
    @m.BIND-POS(1, 'y');
    is-deeply @m[^5], (1, 'y', 3, 4, 5), 'binding into an already reified slot of a lazy array leaves the rest lazy';
    ok @m.is-lazy, 'the array stays lazy after binding into a reified slot';
    my @f = lazy 1..3;
    @f.BIND-POS(6, 'z');
    is-deeply @f, [1, 2, 3, Any, Any, Any, 'z'], 'binding past the end of a lazy array that runs out extends it';
    nok @f.is-lazy, 'the array is no longer lazy once its source ran out';
}

# A hole inside the reified part is bound like any other slot.
{
    my @h = 1;
    @h[4] = 5;
    @h.BIND-POS(2, 'x');
    is-deeply @h, [1, Any, 'x', Any, 5], 'binding into a hole fills it';
}

# A uint position takes its own candidate, on either path.
{
    my @a = 1, 2, 3;
    my uint $u = 1;
    @a.BIND-POS($u, 11);
    is @a[1], 11, 'a uint position binds into a reified slot';
    my uint $past = 5;
    @a.BIND-POS($past, 13);
    is-deeply @a, [1, 11, 3, Any, Any, 13], 'a uint position past the end extends the array';
    my @e;
    my uint $two = 2;
    @e.BIND-POS($two, 5);
    is-deeply @e, [Any, Any, 5], 'a uint position creates the storage of an empty array';
    my @l = 1..*;
    my uint $three = 3;
    @l.BIND-POS($three, 7);
    is-deeply @l[^5], (1, 2, 3, 7, 5), 'a uint position reifies a lazy array up to it';
}

# An int position is boxed for the Int candidate, on either path.
{
    my @a = 1, 2, 3;
    my int $i = 2;
    @a.BIND-POS($i, 12);
    is @a[2], 12, 'an int position binds into a reified slot';
    my int $past = 4;
    @a.BIND-POS($past, 13);
    is-deeply @a, [1, 2, 12, Any, 13], 'an int position past the end extends the array';
    my @e;
    my int $two = 2;
    @e.BIND-POS($two, 5);
    is-deeply @e, [Any, Any, 5], 'an int position creates the storage of an empty array';
    my @l = 1..*;
    my int $three = 3;
    @l.BIND-POS($three, 7);
    is-deeply @l[^5], (1, 2, 3, 7, 5), 'an int position reifies a lazy array up to it';
}

# A negative position is an error, and a uint position above the int
# range is one too.
{
    my @a = 1, 2, 3;
    throws-like { @a.BIND-POS(-1, 1) }, X::OutOfRange, got => -1,
        'a negative Int position is out of range';
    throws-like { @a.BIND-POS(-2**40, 1) }, X::OutOfRange, got => -2**40,
        'a negative Int position beyond 32 bits is out of range';
    my int $neg = -2;
    throws-like { @a.BIND-POS($neg, 1) }, X::OutOfRange, got => -2,
        'a negative int position is out of range';
    my uint $huge = 2**64 - 1;
    throws-like { @a.BIND-POS($huge, 1) }, X::OutOfRange,
        'a uint position above the int range is out of range';
    is-deeply @a, [1, 2, 3], 'a rejected position leaves the array alone';
}

# The bind returns what was bound.
{
    my @z = 1;
    my $c = 5;
    my $r := @z.BIND-POS(0, $c);
    $c = 6;
    is $r, 6, 'the bind returns the bound container';
    is @z[0], 6, 'the array element is that container';
}

# The binding subscript reaches the same method.
{
    my @t = 1, 2, 3;
    my $x = 7;
    @t[5] := $x;
    is-deeply @t, [1, 2, 3, Any, Any, 7], 'a binding subscript past the end extends the array';
    $x = 8;
    is @t[5], 8, 'the binding subscript binds the container itself';
    my int $i = 0;
    @t[$i] := 'first';
    is @t[0], 'first', 'a binding subscript through a native int binds';
}
