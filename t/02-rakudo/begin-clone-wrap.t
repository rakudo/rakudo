use Test;

plan 5;

# A code object cloned at BEGIN time, as `.wrap` does to the routine it
# wraps, kept its compile-time do. Its outer then never pointed at the
# runtime frames, so the body ran against phantom containers. Such clones
# get the same load-time replacement of their do that later clones get.

{
    my $e = 0;
    sub f() { ++$e }
    BEGIN &f.wrap(-> | { callsame });
    f();
    is $e, 1, 'the body of a BEGIN-wrapped sub reaches the runtime lexical';
}

{
    my $e = 0;
    sub f() { ++$e }
    my $c;
    BEGIN $c = &f.clone;
    $c();
    is $e, 1, 'a clone made at BEGIN reaches the runtime lexical when called';
    f();
    is $e, 2, 'the original still reaches it after the clone ran';
}

{
    my $executed = 0;
    my %cache;
    sub compute($n) { ++$executed; $n * 2 }
    BEGIN &compute.wrap(-> |c {
        my $key = c.raku;
        %cache{$key}:exists ?? %cache{$key} !! (%cache{$key} = callsame)
    });
    is compute(21) + compute(21), 84, 'memoized results are correct';
    is $executed, 1, 'memoization via a BEGIN-time wrap caches the body call';
}
