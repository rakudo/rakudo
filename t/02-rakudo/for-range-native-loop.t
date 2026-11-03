use Test;

plan 39;

# A sunk statement-level `for` loop over a CORE integer range, or over a
# CORE integer sequence with two compile time integer bounds, compiles to
# a native
# counting loop. These tests pin the observable behavior: the values each
# constructor produces, control flow, and the cases that must keep the
# general compilation.

{
    my @seen;
    for 1..5 { @seen.push($_) }
    is-deeply @seen, [1, 2, 3, 4, 5], 'an inclusive .. range produces its values';
}

{
    my @seen;
    for 1..^5 { @seen.push($_) }
    is-deeply @seen, [1, 2, 3, 4], 'a ..^ range excludes its right endpoint';
}

{
    my @seen;
    for 1^..5 { @seen.push($_) }
    is-deeply @seen, [2, 3, 4, 5], 'a ^.. range excludes its left endpoint';
}

{
    my @seen;
    for 1^..^5 { @seen.push($_) }
    is-deeply @seen, [2, 3, 4], 'a ^..^ range excludes both endpoints';
}

{
    my @seen;
    for ^4 { @seen.push($_) }
    is-deeply @seen, [0, 1, 2, 3], 'a ^N range counts from zero';
}

{
    my @seen;
    for (1..4).reverse { @seen.push($_) }
    is-deeply @seen, [4, 3, 2, 1], 'a reversed range counts down';
}

{
    my @seen;
    for -2..2 { @seen.push($_) }
    is-deeply @seen, [-2, -1, 0, 1, 2], 'a range with negative bounds produces its values';
}

{
    my @seen;
    for 5..1 { @seen.push($_) }
    is-deeply @seen, [], 'a range with start past end runs zero times';
}

{
    my @seen;
    @seen.push($_) for ^3;
    is-deeply @seen, [0, 1, 2], 'a modifier for over a range produces its values';
}

{
    my int $n = 3;
    my @seen;
    for 1..$n { @seen.push($_) }
    is-deeply @seen, [1, 2, 3], 'a native int variable works as a range bound';
}

{
    my @seen;
    for 10_000_000_000..10_000_000_002 { @seen.push($_) }
    is-deeply @seen, [10_000_000_000, 10_000_000_001, 10_000_000_002],
        'a range with bounds past 32 bits still produces its values';
}

{
    sub infix:<..>($a, $b) { (42,) }
    my @seen;
    for 3..4 { @seen.push($_) }
    is-deeply @seen, [42], 'a lexically redefined range constructor is honored';
}

{
    my @seen;
    for 1..10 { last if $_ > 3; @seen.push($_) }
    is-deeply @seen, [1, 2, 3], 'last works in a range for loop';
}

{
    my @seen;
    for 1..6 { next if $_ %% 2; @seen.push($_) }
    is-deeply @seen, [1, 3, 5], 'next works in a range for loop';
}

{
    my @seen;
    for 1...5 { @seen.push($_) }
    is-deeply @seen, [1, 2, 3, 4, 5], 'an ascending sequence produces its values';
}

{
    my @seen;
    for 5...1 { @seen.push($_) }
    is-deeply @seen, [5, 4, 3, 2, 1], 'a descending sequence produces its values';
}

{
    my @seen;
    for 3...3 { @seen.push($_) }
    is-deeply @seen, [3], 'a single element sequence produces its value';
}

{
    my @seen;
    for 1...0 { @seen.push($_) }
    is-deeply @seen, [1, 0], 'a sequence to zero descends rather than staying empty';
}

{
    my @seen;
    for (2...6).reverse { @seen.push($_) }
    is-deeply @seen, [6, 5, 4, 3, 2], 'a reversed sequence produces its values backward';
}

{
    my @seen;
    for 1...4 -> int $i { @seen.push($i) }
    is-deeply @seen, [1, 2, 3, 4], 'a sequence loop binds a native int parameter';
}

{
    my int $n = 5;
    my @seen;
    for 1...$n { @seen.push($_) }
    is-deeply @seen, [1, 2, 3, 4, 5], 'a variable bound keeps the sequence values';
}

{
    my @seen;
    for 1,3...9 { @seen.push($_) }
    is-deeply @seen, [1, 3, 5, 7, 9], 'a stepped sequence keeps its values';
}

{
    my @seen;
    for "a"..."e" { @seen.push($_) }
    is-deeply @seen, ["a", "b", "c", "d", "e"], 'a string sequence keeps its values';
}

{
    my @seen;
    for 1...10 { last if $_ > 3; @seen.push($_) }
    is-deeply @seen, [1, 2, 3], 'last works in a sequence for loop';
}

{
    my @seen;
    for 1...6 { next if $_ %% 2; @seen.push($_) }
    is-deeply @seen, [1, 3, 5], 'next works in a sequence for loop';
}

{
    sub infix:<...>($a, $b) { (42,) }
    my @seen;
    for 3...4 { @seen.push($_) }
    is-deeply @seen, [42], 'a lexically redefined sequence constructor is honored';
}

{
    my @seen;
    for (5...1).reverse { @seen.push($_) }
    is-deeply @seen, [1, 2, 3, 4, 5], 'a reversed descending sequence counts up';
}

{
    my @seen;
    for 1...* { last if $_ > 4; @seen.push($_) }
    is-deeply @seen, [1, 2, 3, 4], 'last leaves a sequence with no end bound';
}

{
    my @seen;
    for 10_000_000_000...10_000_000_002 { @seen.push($_) }
    is-deeply @seen, [10_000_000_000, 10_000_000_001, 10_000_000_002],
        'a sequence with bounds past 32 bits still produces its values';
}

{
    my @seen;
    for -3...3 { @seen.push($_) }
    is-deeply @seen, [-3, -2, -1, 0, 1, 2, 3], 'a sequence with a negative start produces its values';
}

{
    my @seen;
    for 3...-3 { @seen.push($_) }
    is-deeply @seen, [3, 2, 1, 0, -1, -2, -3], 'a sequence descending past zero produces its values';
}

{
    my @seen;
    my $redone = 0;
    for 1...3 { @seen.push($_); redo if $_ == 2 && !$redone++ }
    is-deeply @seen, [1, 2, 2, 3], 'redo repeats a sequence loop iteration once';
}

{
    my @seen;
    @seen.push($_) for 1...5;
    is-deeply @seen, [1, 2, 3, 4, 5], 'a modifier for over a sequence produces its values';
}

{
    my @seen = do for 1...3 { $_ * 10 };
    is-deeply @seen, [10, 20, 30], 'a non sunk for over a sequence keeps its result values';
}

{
    my @seen;
    LOOP: for 1...5 { @seen.push($_); last LOOP if $_ == 3 }
    is-deeply @seen, [1, 2, 3], 'last with a label leaves a sequence loop';
}

# A sequence walks its endpoints with .succ, so a bound whose value is
# an Int subtype produces elements of its own type and steps by its
# own succession. Those keep the general compilation.

{
    my @seen;
    for False...True { @seen.push($_) }
    is-deeply @seen, [False], 'a Bool sequence stops at its single Bool element';
}

{
    my enum E <a b c>;
    my @seen;
    for E::a...E::c { @seen.push($_) }
    is-deeply @seen, [E::a, E::b, E::c], 'an enum sequence produces enum elements';
}

{
    my enum G (g1 => 1, g2 => 5);
    my @seen;
    for G::g1...G::g2 { @seen.push($_) }
    is-deeply @seen, [G::g1, G::g2], 'a sparse enum sequence steps by enum succession';
}

{
    my @seen;
    for <1>...<3> { @seen.push($_.^name) }
    is-deeply @seen, ["IntStr", "Int", "Int"], 'an allomorph start keeps its own type';
}
