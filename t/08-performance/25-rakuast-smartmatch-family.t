use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 39;

# The helper's walk does not descend a ParamTypeCheck, and a local name
# identifies the unfolded junction more precisely than any op, so these
# walk every child list themselves.

sub qast-deep-contains-op (Mu $qast, Str:D $name --> Bool:D) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq $name {
        return True;
    }
    for (try $qast.list) // () {
        return True if nqp::istype($_, QAST::Node) && qast-deep-contains-op($_, $name);
    }
    False
}

sub qast-contains-unfold-local (Mu $qast --> Bool:D) {
    if nqp::istype($qast, QAST::Var) && $qast.name.starts-with('junction_unfold') {
        return True;
    }
    for (try $qast.list) // () {
        return True if nqp::istype($_, QAST::Node) && qast-contains-unfold-local($_);
    }
    False
}

# The legacy frontend reduces through its own QAST optimizer, so the
# shapes here are this frontend's.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my Int $x; my $r = $x ~~ Int', :full, -> \v {
        not qast-contains-op(v, 'istype')
    }, 'a typematch guaranteed by the declared type folds away its check';

    qast-is 'my $x = 5; my $r = $x ~~ 42', :full, -> \v {
        not qast-contains-call(v, '&infix:<~~>')
    }, 'a literal matcher compiles without the smartmatch dispatch';

    qast-is 'my $x = 5; my $r = $x ~~ :is-prime', :full, -> \v {
        not qast-contains-call(v, '&infix:<~~>')
    }, 'a Pair matcher compiles without the smartmatch dispatch';

    qast-is 'my $x = 2; if $x == 1|2|3 { say 1 }', :full, -> \v {
        qast-contains-unfold-local(v)
    }, 'a junction comparison in boolean position unfolds to a short-circuit chain';

    qast-is 'my $x = 2; my $r = $x == 1|2', :full, -> \v {
        not qast-contains-unfold-local(v)
    }, 'a junction comparison whose value is used keeps the junction';

    qast-is 'sub f($x where Int|Str) { 1 }; f(1)', :full, -> \v {
        qast-deep-contains-op(v, 'istype')
    }, 'a junction-of-types where constraint checks the types inline';
}
else {
    skip 'the reduced shapes are specific to the RakuAST frontend', 6;
}

# Behavior stays identical.

{
    my Int $x = 5;
    is-deeply ($x ~~ Int, $x ~~ Str, $x !~~ Int), (True, False, False),
        'a declared-type topic still answers each typematch correctly';
    sub typed(Int $a) { $a ~~ Int }
    ok typed(3), 'a typed parameter topic folds to the same answer';
}

{
    my $x = 42;
    is-deeply ($x ~~ 42, $x ~~ 43, $x !~~ 43, $x ~~ "42"), (True, False, True, True),
        'literal matches compare the way their ACCEPTS would';
    is "abc" ~~ 5, False, 'a topic that fails numeric coercion matches no number and does not throw';
    my Int $u;
    todo 'the legacy optimizer answers False for a negated literal match on an undefined topic'
        unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';
    is-deeply ($u ~~ 42, $u !~~ 42), (False, True),
        'an undefined topic fails a literal match without warning';
    my $j = any(1, 42);
    ok $j ~~ 42, 'a concrete Junction topic still autothreads a literal match';
    is 5 ~~ 5.0, True, 'both sides known folds through the literal ACCEPTS';
}

{
    is-deeply (4 ~~ :is-prime, 7 ~~ :is-prime, 4 ~~ :!is-prime), (False, True, True),
        'Pair matchers ask the method the key names';
    my %h = e => 42;
    is-deeply (%h ~~ :e, %h ~~ :!missing), (True, False),
        'an Associative topic takes the full Pair match';
    my $p = (is-prime => True);
    ok 7 ~~ $p, 'a Pair in a variable keeps the full match and still matches';
}

{
    my $x = 2;
    my @r;
    @r.push('any') if $x == 1|2|3;
    @r.push('none') if $x == 4|5;
    @r.push('all') if $x == 2&2;
    @r.push('not-all') unless $x == 2&3;
    is @r.join(','), 'any,all,not-all', 'unfolded junction conditions branch correctly';
    my $a = 1;
    my $b = 2;
    @r = ();
    @r.push('vars') if $x == $a|$b;
    is @r.join, 'vars', 'a junction built from variables unfolds and still matches';
    my $r = ($x == 1|2);
    isa-ok $r, Junction, 'a junction comparison used as a value stays a Junction';
    my $j = 1|2;
    ok ?($x == $j), 'a junction in a variable is not unfolded and still autothreads';
}

{
    sub f($x where Int|Str) { 'ok' }
    is f(1), 'ok', 'an inline any-of-types constraint accepts a matching argument';
    is f('a'), 'ok', 'the second type of the constraint accepts too';
    dies-ok { f(1.5e0) }, 'a non-matching argument still rejects';
    sub g($x where Int & Numeric) { 'ok' }
    is g(1), 'ok', 'an all-of-types constraint accepts when every type matches';
    dies-ok { g('a') }, 'an all-of-types constraint rejects a partial match';
    subset SmallInt of Int where * < 10;
    sub s($x where SmallInt|Str) { 'ok' }
    is-deeply (s(3), s('z')), ('ok', 'ok'), 'a subset eigenstate accepts through its refinement';
    dies-ok { s(50) }, 'a subset eigenstate rejects through its refinement';
    multi w($x where Int|Str) { 'is' }
    multi w($x) { 'other' }
    is-deeply (w(1), w(3.5e0)), ('is', 'other'), 'multi dispatch over an inline constraint is unchanged';
}

{
    # `~~` is a chaining infix, so `0 ~~ 0 ~~ 0` is `(0 ~~ 0) && (0 ~~ 0)`. The
    # legacy QAST optimizer miscompiles a chained smartmatch that appears in an
    # argument list, inverting the result; it comes out right under
    # --optimize=off and for a single `~~`, and the RakuAST frontend gets it
    # right through the chain-link withdrawal above. Only exercise the chain
    # protocol where it holds.
    if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
        is-deeply (0 ~~ 0 ~~ 0, 0 ~~ 0 ~~ 1, 0 ~~ 0 !~~ 1), (True, False, True),
            'chained smartmatches keep the chain protocol over reduced links';
    }
    else {
        skip 'the legacy optimizer miscompiles a chained ~~ in an argument list', 1;
    }
    my Int $x = 1;
    is ($x ~~ Int ~~ Bool), False,
        'a fold withdrawn from a chain link passes the middle operand along';
    is (0 ~~ 0 == 0), True, 'a reduced link mixed with a comparison chains correctly';
}

# A smartmatch against a junction of type objects, in a position that
# only tests truth, checks the topic against each type instead of
# threading the matcher and collapsing the boolean junction.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my $x = 5; if $x ~~ Int|Str { say 1 }', :full, -> \v {
        qast-deep-contains-op(v, 'istype')
            and not qast-contains-call(v, '&infix:<~~>')
    }, 'a boolean-position junction-of-types match checks istype with no smartmatch';

    qast-is 'my $x = 5; my $r = $x ~~ Int|Str', :full, -> \v {
        qast-contains-call(v, '&infix:<~~>')
            or qast-contains-callmethod(v, 'ACCEPTS')
    }, 'a junction-of-types match whose value is used keeps the full match';
}
else {
    skip 'the junction-of-types shapes are specific to the RakuAST frontend', 2;
}

{
    my $x = 5;
    my @r;
    @r.push('any') if $x ~~ Int|Str;
    @r.push('none') if $x ~~ Str|Num;
    @r.push('all') if $x ~~ Cool & Numeric;
    @r.push('not-all') if $x ~~ Str & Numeric;
    @r.push('neg') if $x !~~ Str|Num;
    is @r.join(','), 'any,all,neg', 'junction-of-types conditions branch like the full match';
}

{
    my $j = any(1, 2.5e0);
    my @r;
    @r.push('jt') if $j ~~ Int|Num;
    @r.push('no') if $j ~~ Str|Bool;
    is @r.join, 'jt', 'a concrete Junction topic still threads through the matcher';
}

{
    my @r;
    given 5 { when Int|Str { @r.push('is') }; default { @r.push('no') } }
    given 2.5e0 { when Int|Str { @r.push('is') }; default { @r.push('no') } }
    $_ = 's';
    @r.push('mod') when Int|Str;
    is @r.join(','), 'is,no,mod', 'when with a junction-of-types matcher picks the right arms';
}

{
    subset TinyInt of Int where * < 10;
    my @r;
    @r.push('sub') if 4 ~~ TinyInt|Str;
    @r.push('big') if 50 ~~ TinyInt|Str;
    is @r.join, 'sub', 'a subset eigenstate runs its refinement in the type check';
}

{
    my class WithAccepts { method ACCEPTS($x) { True } }
    my $x = 5;
    ok ?($x ~~ WithAccepts|Str), 'a user-ACCEPTS eigenstate keeps the full match';
    my constant IntOrStr = Int|Str;
    ok ?($x ~~ IntOrStr), 'a constant junction matcher reduces and matches';
}
