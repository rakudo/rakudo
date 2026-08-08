use Test;

plan 8;

# A sunk for statement can inline its body, binding the pulled value
# straight into the parameter's lowered local. That bind must match what
# calling the block would have done. The value gets a fresh read-only
# Scalar, so an itemized element stays a single item. A parameter with a
# sigil other than '$' is left to the real binder, which checks its
# nominal type.

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

# vim: expandtab shiftwidth=4
