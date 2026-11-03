use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 34;

# A conditional whose condition's truth is known at compile time keeps
# only the code that could run, and a chaining comparison on constant
# operands folds to its answer so such conditions become decidable.

sub qast-contains-sval (Mu $qast, Str:D $value --> Bool:D) {
    if nqp::istype($qast, QAST::SVal) && $qast.value eq $value {
        return True;
    }
    for (try $qast.list) // () {
        return True if nqp::istype($_, QAST::Node) && qast-contains-sval($_, $value);
    }
    False
}

# The legacy frontend has no dead-branch elimination, so the shapes
# here are this frontend's.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'if 2 > 4 { say "deadstr" } else { say "alivestr" }', :full, -> \v {
        qast-contains-sval(v, 'alivestr') and not qast-contains-sval(v, 'deadstr')
    }, 'a false constant comparison keeps only the else branch';

    qast-is 'my constant DEBUG = False; if DEBUG { say "deadstr" }', :full, -> \v {
        not qast-contains-sval(v, 'deadstr')
    }, 'a constant-gated branch is dropped whole';

    qast-is 'if 1 < 2 { say "alivestr" }', :full, -> \v {
        qast-contains-sval(v, 'alivestr')
            and not qast-contains-call(v, '&infix:«<»')
            and not qast-contains-op(v, 'islt_i')
    }, 'a true constant condition leaves the branch with no test at all';

    qast-is 'say "deadstr" if 2 > 4', :full, -> \v {
        not qast-contains-sval(v, 'deadstr')
    }, 'a false condition modifier drops its statement';

    # Not :full, so the walk sees only this statement's QAST and the absence
    # of an if op means the modifier itself is gone, not just the folded
    # comparison inside it.
    qast-is 'say "alivestr" if 2 > 1', -> \v {
        qast-contains-sval(v, 'alivestr')
            and not qast-contains-op(v, 'if')
    }, 'a true condition modifier is eliminated whole';

    qast-is 'my $x = 3; if $x > 4 { say "maybestr" }', :full, -> \v {
        qast-contains-sval(v, 'maybestr')
    }, 'a runtime condition keeps its conditional';
}
else {
    skip 'the dead-branch shapes are specific to the RakuAST frontend', 6;
}

# Behavior stays identical.

{
    my @r;
    if 2 > 4 { @r.push('dead') } else { @r.push('alive') }
    is @r.join, 'alive', 'the surviving else branch runs';
}

{
    my constant DEBUG = False;
    my @r;
    if DEBUG { @r.push('dead') }
    @r.push('after');
    is @r.join, 'after', 'code after a dropped constant-gated branch runs';
}

{
    is-deeply (do if 0 { 5 }), Empty, 'a false conditional with no else is Empty';
    is (do if 1 { 5 } else { 6 }), 5, 'a true conditional yields its then value';
    is (do if 0 { 5 } else { 6 }), 6, 'a false conditional yields its else value';
    is (do unless 0 { 7 }), 7, 'a false unless condition yields its body value';
    is-deeply (do unless 1 { 7 }), Empty, 'a true unless condition is Empty';
}

{
    my @r;
    @r.push('if') if 1;
    @r.push('dead') if 0;
    @r.push('unless') unless 0;
    is @r.join(','), 'if,unless', 'condition modifiers with known truth keep the right statements';
}

{
    my $x = 5 if 0;
    ok !$x.defined, 'a dropped-condition declaration is still declared and undefined';
}

{
    my @r;
    if 1 { @r.push('a') } elsif 1 { @r.push('b') } else { @r.push('c') }
    if 0 { @r.push('x') } elsif 1 { @r.push('q') }
    is @r.join(','), 'a,q', 'elsif chains pick the same arms as at runtime';
}

{
    if 0 { } elsif (my $k = 5) { }
    is $k, 5, 'a declaration in a kept elsif condition still binds';
}

{
    my $seen;
    with 42 { $seen = $_ }
    is $seen, 42, 'a with part is untouched and still topicalizes';
}

{
    is-deeply (2 > 4, 1 < 2 < 3, 3 < 2 < 10), (False, True, False),
        'chained comparisons on constants fold to the runtime answers';
    my $x = 5;
    is (1 < 2 < $x), True, 'a folded link inside a live chain still chains';
}

{
    my int $n = 0;
    if 1 { my $a = 1; $n = $n + $a }
    is $n, 1, 'a collapsed branch that then flattens still runs its code';
}

{
    my class Boomy { method Bool() { die "boom" } }
    my \b = Boomy.new;
    dies-ok { if b { } }, 'a constant whose Bool throws keeps the throw at runtime';
}

# Statements after one that unconditionally leaves the surrounding code
# never run, and are dropped unless they declare something or register
# a phaser.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'sub f() { return 5; say "deadstr" }; say f()', :full, -> \v {
        not qast-contains-sval(v, 'deadstr')
    }, 'statements after a return are dropped';

    qast-is 'sub f() { return 5; my $k = 1; say "deadstr" }; say f()', :full, -> \v {
        not qast-contains-sval(v, 'deadstr')
    }, 'a declaring unreachable statement stays while its droppable neighbors go';

    qast-is 'sub f() { die "x"; say "livestr" }; try f()', :full, -> \v {
        qast-contains-sval(v, 'livestr')
    }, 'statements after a die stay, since a handler can resume into them';

    qast-is 'sub f() { return 5; say "deadstr"; CATCH { default { say "handlerstr" } } }; say f()', :full, -> \v {
        qast-contains-sval(v, 'handlerstr') and not qast-contains-sval(v, 'deadstr')
    }, 'a CATCH after a return stays while its droppable neighbors go';
}
else {
    skip 'the unreachable-code shapes are specific to the RakuAST frontend', 4;
}

{
    sub f() { return 5; say "dead" }
    is f(), 5, 'a routine still returns its value with trailing code dropped';
}

{
    sub f() { return 1; sub g() { 9 } }
    is f(), 1, 'a routine declared after a return is kept, since its name hoists';
    sub caller-test() { return 2; my $x = 3 }
    is caller-test(), 2, 'an unreachable declaration is kept without running its initializer';
}

{
    my $l;
    sub f() { return 7; LEAVE $l = 'leave-ran' }
    is f(), 7, 'a phaser after a return still registers';
    is $l, 'leave-ran', 'and fires when the routine is left';
}

{
    my @r;
    sub f() { die "x"; @r.push('resumed') }
    { f(); CATCH { default { .resume } } }
    @r.push('end');
    is @r.join(','), 'resumed,end', 'resuming after a die still reaches the following statements';
}

{
    sub f() { return 5 if 1 > 2; 'fell-through' }
    is f(), 'fell-through', 'a conditional return does not drop what follows';
}

{
    lives-ok { EVAL 'return 1; CATCH { default { } }' },
        'a CATCH after a mainline return still catches the control exception';
}
