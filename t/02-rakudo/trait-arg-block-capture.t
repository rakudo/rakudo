use Test;

plan 4;

# A block passed as a trait argument, as in `is memoized({ ... })`, nests in
# the routine carrying the trait. The enclosing scope must not emit the same
# block again, or its own frame prologue captures the block against itself
# and invoking the block dies with an outer frame mismatch.

{
    my @keys;
    sub trait_mod:<is>(Routine:D \r, :$keyed!) {
        r.wrap(-> |c { $keyed.(c); callsame });
    }
    sub f($a) is keyed({ @keys.push(.raku) }) { $a * 2 }
    is f(21), 42, 'a routine with a block trait argument still runs';
    like @keys[0], /21/, 'the block ran against the passed capture';
}

{
    my int $seen;
    sub trait_mod:<is>(Routine:D \r, :$count!) {
        r.wrap(-> |c { $count.(); callsame });
    }
    sub g($a) is count({ ++$seen }) { $a }
    g(1);
    g(2);
    is $seen, 2, 'the block reaches a lexical of the enclosing scope';
}

{
    my %cache;
    my int $ran;
    sub trait_mod:<is>(Routine:D \r, :$memo!) {
        r.wrap(-> |c {
            my $k = $memo.(c);
            %cache{$k}:exists ?? %cache{$k} !! (%cache{$k} = callsame)
        });
    }
    sub h($n) is memo({ .[0].Str }) { ++$ran; $n + 1 }
    h(5); h(5); h(6);
    is $ran, 2, 'a keymaker block drives memoization correctly';
}
