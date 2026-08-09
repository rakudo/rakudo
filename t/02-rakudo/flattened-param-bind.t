use Test;

plan 19;

# A sunk for statement and a given statement can inline the body,
# binding the value straight into the parameter's lowered local. That
# bind must match what calling the block would have done. The value
# gets a fresh read-only Scalar, so an itemized element stays a single
# item. A parameter with an '@', '%' or '&' sigil gets the nominal type
# check the binder would have run, and a concrete Junction value
# autothreads over its eigenstates rather than binding.

{
    my @data = 1, $("ALPHA", "BETA"), 2;
    my %index;
    for @data -> $item {
        %index{$item} = 42;
    }
    is-deeply %index.keys.sort.List, ("1", "2", "ALPHA BETA"),
        'an itemized element stays a single hash key inside the loop body';
}

{
    my @data = 1, 2;
    my @types;
    for @data -> $item {
        @types.push($item.VAR.^name);
    }
    is-deeply @types, ["Scalar", "Scalar"],
        'the loop parameter reports a Scalar container via .VAR';
}

dies-ok {
    my @data = 5,;
    for @data -> $item { $item = 99 }
}, 'assigning to the read-only loop parameter dies';

{
    my @data = 5,;
    try { for @data -> $item { $item = 99 } }
    is @data[0], 5,
        'the source element is untouched after an assignment attempt';
}

throws-like { for 1, 2 -> @row { } }, X::TypeCheck::Binding::Parameter,
    'binding a value that is not Positional to an @ loop parameter dies';

throws-like { for 1, 2 -> %row { } }, X::TypeCheck::Binding::Parameter,
    'binding a value that is not Associative to a % loop parameter dies';

throws-like { for 1, 2 -> &row { } }, X::TypeCheck::Binding::Parameter,
    'binding a value that is not Callable to an & loop parameter dies';

{
    my @rows;
    for [1, 2], [3, 4] -> @row { @rows.push(@row.join('-')) }
    is-deeply @rows, ["1-2", "3-4"],
        'an @ loop parameter binds Positional values';
}

{
    my @src = [1, 2],;
    for @src -> @row { @row.push(3) }
    is-deeply @src[0], [1, 2, 3],
        'an @ loop parameter binds the source array itself, not a copy';
}

{
    my @keys;
    for {:a(1)}, {:b(2)} -> %h { @keys.push(%h.keys[0]) }
    is-deeply @keys.sort.List, ("a", "b"),
        'a % loop parameter binds Associative values';
}

{
    my @got;
    for { 40 }, { 2 } -> &f { @got.push(f()) }
    is-deeply @got, [40, 2],
        'an & loop parameter binds Callable values and is callable in the body';
}

{
    my @seen;
    for (any([1], all([2], [3])), [9]) -> @row { @seen.push(@row[0]) }
    is-deeply @seen, [1, 2, 3, 9],
        'a Junction element autothreads over its eigenstates, depth first';
}

{
    my @seen;
    try { for (any([1], 5),) -> @row { @seen.push(@row[0]) } }
    is-deeply @seen, [1],
        'eigenstates before a failing one still run the body';
    is $!.message,
        q{Type check failed in binding to parameter '@row'; expected Positional but got Int (5)},
        'a failing eigenstate reports the binder error for the parameter';
}

{
    my %h;
    given $("A", "B") -> $x { %h{$x} = 1 }
    is-deeply %h.keys.List, ("A B",),
        'an itemized given topic stays a single hash key';
}

throws-like { given 5 -> @row { } }, X::TypeCheck::Binding::Parameter,
    'binding a value that is not Positional to an @ given parameter dies';

{
    my @seen;
    given any([1], [2]) -> @row { @seen.push(@row[0]) }
    is-deeply @seen, [1, 2],
        'a Junction given topic autothreads through an @ parameter';
}

{
    my $r = do given [1, 2] -> @row { @row.sum };
    is $r, 3,
        'a value producing given with an @ parameter returns the body value';
}

{
    my $r = do given $("A",) -> $x { $x[0] };
    is $r, "A",
        'a value producing given with a scalar parameter returns the body value';
}

# vim: expandtab shiftwidth=4
