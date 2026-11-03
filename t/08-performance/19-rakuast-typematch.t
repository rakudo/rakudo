use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 22;

# A smartmatch against a compile-time-known type object reduces to a type
# check guarded by a runtime Junction test on the topic. A matcher with a
# user ACCEPTS candidate, a matcher only known at runtime, and a link of a
# longer comparison chain all keep the full smartmatch.

sub qast-op-named (Mu $qast, Str:D $op, Str:D $name --> Bool:D) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq $op && $qast.name eq $name {
        return True;
    }
    elsif qast-descendable $qast {
        for $qast.list {
            qast-op-named $_, $op, $name and return True;
        }
    }
    False
}

# These observe the emitted QAST.
qast-is 'my $x = 5; my $r = $x ~~ Int', -> \v {
        qast-contains-op(v, 'istype')
    and not qast-op-named(v, 'chain', '&infix:<~~>')
    and not qast-op-named(v, 'chainstatic', '&infix:<~~>')
    and not qast-op-named(v, 'call', '&infix:<~~>')
    and not qast-op-named(v, 'callstatic', '&infix:<~~>')
}, 'a smartmatch against a type object compiles to a type check';

qast-is 'my $x = 5; my $r = $x !~~ Int', -> \v {
    qast-contains-op(v, 'istype') and qast-contains-op(v, 'not_i')
}, 'a negated smartmatch against a type object compiles to a negated type check';

qast-is 'my $x = 5; my $r = $x ~~ Int', -> \v {
    qast-contains-callmethod(v, 'BOOLIFY-ACCEPTS')
}, 'the type check carries a Junction fallback for the topic';

qast-is 'my $x = 5; my $r = $x ~~ Junction', -> \v {
        qast-contains-op(v, 'istype')
    and not qast-contains-callmethod(v, 'BOOLIFY-ACCEPTS')
}, 'a match against Junction itself needs no fallback';

qast-is 'my $r = 42 ~~ Int', -> \v {
        not qast-contains-op(v, 'istype')
    and not qast-op-named(v, 'chain', '&infix:<~~>')
    and not qast-op-named(v, 'chainstatic', '&infix:<~~>')
    and not qast-op-named(v, 'call', '&infix:<~~>')
    and not qast-op-named(v, 'callstatic', '&infix:<~~>')
}, 'a smartmatch of two compile-time values folds to a constant';

qast-is 'subset TmEven of Int where * %% 2; my $x = 5; my $r = $x ~~ TmEven', -> \v {
    qast-contains-op(v, 'istype')
}, 'a smartmatch against a subset compiles to a type check';

qast-is 'class TmW { method ACCEPTS($x) { True } }; my $x = 5; my $r = $x ~~ TmW', -> \v {
    not qast-contains-op(v, 'istype')
}, 'a matcher with its own ACCEPTS keeps the full smartmatch';

qast-is 'my $x = 5; my $y = Int; my $r = $x ~~ $y', -> \v {
    not qast-contains-op(v, 'istype')
}, 'a matcher only known at runtime keeps the full smartmatch';

# These observe that the reduced match still answers like the full one.
my $x = 5;
my Int $u;
subset RtEven of Int where * %% 2;
class RtW { method ACCEPTS($x) { True } }

is ($x ~~ Int), True, 'a variable topic matches its type';
is ($x !~~ Str), True, 'a negated match against a foreign type holds';
is ($x ~~ Int:D), True, 'a definite type matches a concrete topic';
is ($u ~~ Int:D), False, 'a definite type rejects an undefined topic';
is ("5" ~~ Int(Str)), True, 'a coercion type accepts a coercible topic';
is ($x ~~ RtEven), False, 'a subset runs its refinement';
is (4 ~~ RtEven), True, 'a subset accepts a refined value';
is ($x ~~ RtW), True, 'a user ACCEPTS candidate still runs';
is (any(1, "x") ~~ Int), True, 'an any Junction topic autothreads';
is (all(1, "x") ~~ Int), False, 'an all Junction topic autothreads';
is (any(1, "x") !~~ Int), False, 'a negated Junction match negates the whole match';
is (any(3, 4) ~~ RtEven), True, 'a Junction topic autothreads over a subset';
{
    my $calls = 0;
    sub topic { $calls++; 5 }
    is "{topic() ~~ Int} $calls", 'True 1', 'the topic is evaluated once';
}
# A name lookup may claim a compile-time value that differs from what
# evaluating it produces, so it must match at runtime rather than fold.
sub tm-marker() is export { }
is (EXPORT::ALL:: ~~ Stash), True, 'a package stash topic matches its type at runtime';

# vim: expandtab shiftwidth=4
