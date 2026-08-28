use Test;

plan 20;

# A sunk statement-level `for` loop drives the source's iterator directly
# rather than calling its `map` method, so an object that overrides `map`
# (e.g. a query-building DSL) still iterates its rows.

my class OverridesMap does Iterable {
    has @.map-calls;
    method iterator { (1, 2, 3).iterator }
    method map(|c) { @!map-calls.push(c); nextsame }
}

{
    my $source = OverridesMap.new;
    my @seen;
    for $source<> -> $x { @seen.push($x) }
    is-deeply @seen, [1, 2, 3], 'statement for with a pointy block iterates via the iterator';
    is $source.map-calls.elems, 0, 'statement for with a pointy block does not call a user map override';
}

{
    my $source = OverridesMap.new;
    my @seen;
    for $source<> { @seen.push($_) }
    is-deeply @seen, [1, 2, 3], 'statement for with a bare block iterates via the iterator';
    is $source.map-calls.elems, 0, 'statement for with a bare block does not call a user map override';
}

{
    my $source = OverridesMap.new;
    my @seen;
    for $source<> -> $x { last if $x == 2; @seen.push($x) }
    is-deeply @seen, [1], 'last works in a directly iterated for loop';
}

{
    my $source = OverridesMap.new;
    my @seen;
    TOP: for $source<> -> $x {
        for $source<> -> $y { next TOP if $y == 2; @seen.push($x ~ $y) }
    }
    is-deeply @seen, ['11', '21', '31'], 'a labeled next reaches the outer directly iterated for loop';
}

{
    my $source = OverridesMap.new;
    my @seen;
    @seen.push($_) for $source<>;
    is-deeply @seen, [1, 2, 3], 'modifier for iterates via the iterator';
    is $source.map-calls.elems, 0, 'modifier for does not call a user map override';
}

{
    my $source = OverridesMap.new;
    my @seen;
    { @seen.push($_) } for $source<>;
    is-deeply @seen, [1, 2, 3], 'block modifier for iterates via the iterator';
    is $source.map-calls.elems, 0, 'block modifier for does not call a user map override';
}

{
    my $source = OverridesMap.new;
    my @seen = do for $source<> -> $x { $x * 2 };
    is-deeply @seen, [2, 4, 6], 'an expression do-for still collects its values';
    is $source.map-calls.elems, 1, 'an expression do-for still dispatches to a user map override';
}

# A body whose signature takes any number of arguments has no fixed
# count, so the statement form does not apply and the loop goes
# through map.

{
    my $source = OverridesMap.new;
    my @seen;
    for $source<> -> *@r { @seen.push(@r) }
    is-deeply @seen, [[1], [2], [3]], 'a slurpy body receives each value as a one element array';
    is $source.map-calls.elems, 1, 'a slurpy body dispatches to a user map override';
}

{
    my $source = OverridesMap.new;
    my @seen;
    for $source<> -> $x, *@rest { @seen.push($x) }
    is-deeply @seen, [1, 2, 3], 'a positional plus slurpy body receives each value';
    is $source.map-calls.elems, 1, 'a positional plus slurpy body dispatches to a user map override';
}

{
    my $source = OverridesMap.new;
    my @seen;
    for $source<> -> $x, |c { @seen.push($x) }
    is-deeply @seen, [1, 2, 3], 'a positional plus capture body receives each value';
    is $source.map-calls.elems, 1, 'a positional plus capture body dispatches to a user map override';
}

{
    my $source = OverridesMap.new;
    my @seen;
    -> *@r { @seen.push(@r) } for $source<>;
    is-deeply @seen, [[1], [2], [3]], 'a slurpy block modifier for receives each value as a one element array';
    is $source.map-calls.elems, 1, 'a slurpy block modifier for dispatches to a user map override';
}
