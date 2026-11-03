use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;
use ThunkedRoutines;

plan 16;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is-deeply (1..3).map({ ThunkedRoutines::nested-statements($_) }).List, (10, 20, 30),
    'a sub declared in parenthesized statements on the left of xx in a precompiled module closes over each call of its routine';
is-deeply (1..3).map({ ThunkedRoutines::under-try($_) }).List, (10, 20, 30),
    'a sub declared on the left of xx under try in a precompiled module closes over each call of its routine';
is-deeply (1..3).map({ ThunkedRoutines::under-once($_) }).List, (10, 20, 30),
    'a sub declared under once in a precompiled module closes over each call of its routine';
is ThunkedRoutines::topic-under-try(), 43,
    'a try right of andthen in a precompiled module sees the topic andthen gives';
is-deeply (1..3).map({ ThunkedRoutines::first-nested($_) }).List, (1, 2, 3),
    'a sub declared under a FIRST in the statement of a FIRST in a precompiled module closes over each call of its routine';
todo 'closes over the first call on the legacy frontend' unless $rakuast;
is-deeply (1..3).map({ ThunkedRoutines::begin-routine($_) }).List, (1, 2, 3),
    'a sub declared under BEGIN in a routine of a precompiled module closes over each call of the routine';
# These hold whichever block declares the code, and guard what it keeps.
is-deeply X.map(*[1]).List, (43, 43),
    'a sub declared on the left of xx in a constant of a precompiled module is called';
if $rakuast {
    is (try W.(1)), 2,
        'a sub declared in a Whatever curry as a constant of a precompiled module is called';
}
else {
    skip 'dies on the legacy frontend', 1;
}
is K, 7,
    'a sub declared under try in a constant of a precompiled module is called';
is-deeply ThunkedRoutines::trait-default(), (5, 5),
    'a sub declared on the left of xx in a trait argument of a precompiled module is called';
is-deeply (1..3).map({ ThunkedRoutines::left-of-xx($_) }).List, (1, 2, 3),
    'a sub declared on the left of xx in a precompiled module closes over each call of its routine';
is ThunkedRoutines::trait-routine(), 42,
    'a sub declared in a role argument of a class in a precompiled module is called';
is ThunkedRoutines::will-method(), 'p-m',
    'a will trait on a method in a precompiled module runs its block';
is ThunkedRoutines::where-routine(5), 5,
    'a sub declared in the where of a variable in a precompiled module is called';
is-deeply (1..3).map({ ThunkedRoutines::multi-xx($_) }).List, <i1 i2 i3>,
    'a multi declared on the left of xx 0 in a precompiled module closes over each call of its routine';
ok ThunkedRoutines::astr-begin(),
    'a regex as the where of a subset in a precompiled module is smartmatched at BEGIN time';
