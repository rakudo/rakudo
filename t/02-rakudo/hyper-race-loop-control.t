use Test;

plan 21;

# Loop controls inside hyper/race iteration stop, skip, or repeat the
# parallel iteration instead of being silently discarded, and a label on
# such a loop neither disables parallelization nor breaks compilation.
# https://github.com/rakudo/rakudo/issues/2208

# A hyper map is ordered, so ending the iteration with `last` produces
# exactly the values before the control was thrown: earlier batches in
# full, the stopping batch truncated, later batches discarded.
is-deeply (1..1000).hyper(:batch(10), :degree(4)).map({ last if $_ == 25; $_ }).List,
    (1..24).List,
    'last in a hyper map keeps exactly the values before the control';

is-deeply (1..100).hyper(:batch(10), :degree(4)).map({ last if $_ == 99; $_ }).List,
    (1..98).List,
    'last in the final batch of a hyper map truncates that batch';

is-deeply (1..20).hyper(:batch(5), :degree(4)).map({ next if $_ %% 2; $_ }).List,
    (1, 3, 5, 7, 9, 11, 13, 15, 17, 19),
    'next in a hyper map skips the value';

{
    my atomicint $redone;
    is-deeply (1..20).hyper(:batch(5), :degree(4)).map({
        if $_ == 7 && $redone == 0 {
            $redone⚛++;
            redo;
        }
        $_
    }).List, (1..20).List, 'redo in a hyper map reruns the block for the value';
    is $redone, 1, 'the redone block ran exactly one extra time';
}

{
    my atomicint $ran;
    hyper for 1..100_000 { $ran⚛++; last if $_ == 5 }
    ok $ran < 100_000, 'last ends a sunk hyper for loop early';
}

{
    my atomicint $ran;
    race for 1..100_000 { $ran⚛++; last if $_ == 5 }
    ok $ran < 100_000, 'last ends a sunk race for loop early';
}

{
    my atomicint $ran;
    FOO: hyper for 1..100_000 { $ran⚛++; last FOO if $_ == 5 }
    ok $ran < 100_000, 'a labeled last ends a sunk hyper for loop early';
}

{
    my atomicint $ran;
    lives-ok { FOO: hyper for 1..100 { $ran⚛++; next FOO } },
        'a labeled next in a hyper for loop compiles and runs';
    is $ran, 100, 'the labeled next ran the body for every value';
}

{
    my atomicint $ran;
    hyper for 1..1000 {
        for 1..3 { last }
        $ran⚛++;
    }
    is $ran, 1000, 'last in a nested serial loop does not end the hyper loop';
}

{
    my @result = (1..10000).race(:batch(64), :degree(4)).map({ last if $_ == 100; $_ });
    ok @result.elems < 10000, 'last in a race map ends the iteration early';
    ok (1..99).Set (<=) @result.Set,
        'a race map stopped by last still emits every value before the last';
}

# A grep matcher block supports the same loop controls as a serial grep:
# next excludes the value, last keeps the matches before the control and
# ends the iteration, redo re-evaluates the value.
is-deeply (1..100).hyper(:batch(10), :degree(4)).grep({ last if $_ == 11; $_ %% 2 }).List,
    (2, 4, 6, 8, 10),
    'last in a hyper grep keeps exactly the matches before the control';

is-deeply (1..20).hyper(:batch(5), :degree(4)).grep({ next if $_ == 4; $_ %% 2 }).List,
    (2, 6, 8, 10, 12, 14, 16, 18, 20),
    'next in a hyper grep excludes the value';

{
    my atomicint $redone;
    is-deeply (1..20).hyper(:batch(5), :degree(4)).grep({
        if $_ == 4 && $redone == 0 {
            $redone⚛++;
            redo;
        }
        $_ %% 2
    }).List, (2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
        'redo in a hyper grep re-evaluates the value';
    is $redone, 1, 'the redone matcher ran exactly one extra time';
}

ok (1..100_000).race(:batch(64), :degree(4)).grep({ last if $_ == 100; True }).elems < 100_000,
    'last in a race grep ends the iteration early';

# The machinery invoked without any loop control is unaffected.
is-deeply (1..10000).hyper(:batch(64), :degree(4)).map(* + 1).List,
    (2..10001).List,
    'a hyper map without loop controls produces all values in order';

is (1..10000).race(:batch(64), :degree(4)).map(* + 1).elems, 10000,
    'a race map without loop controls produces all values';

throws-like { hyper for 1..100 { die "boom" if $_ == 5 } },
    X::HyperRace::Died,
    'an exception in a hyper for loop still reaches the caller';

# vim: expandtab shiftwidth=4
