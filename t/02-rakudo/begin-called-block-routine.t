use Test;
use nqp;

plan 53;

# The bind belongs to the RakuAST frontend, so the legacy frontend runs
# none of this.
unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    skip-rest 'the BEGIN-time compilation bind is specific to the RakuAST frontend';
    exit;
}

# A routine called at BEGIN time is compiled ahead of the unit, in a
# compilation of its own whose outer is a snapshot of the compile-time
# scope. A unit run as a script binds such a routine to the block of its
# own compilation where the routine is declared, so a routine declared
# in a block closes over the frame of the block at runtime and not over
# that snapshot. The cases that read a lexical read one of an enclosing
# bare block, which is what tells the two compilations apart. A lexical
# at unit scope reads the same either way and would assert nothing.

{
    my $x = 5;
    sub c() { $x }
    BEGIN c();
    $x = 7;
    is c(), 7, 'a sub in a block reads the runtime value of a block lexical';
}

{
    my $x = 5;
    multi c() { $x }
    BEGIN c();
    $x = 7;
    is c(), 7, 'a multi in a block reads the runtime value of a block lexical';
}

{
    my $x = 5;
    my $cl = sub c() { $x }
    BEGIN c();
    $x = 7;
    is $cl(), 7, 'the closure a sub declaration evaluates to reads the runtime value';
}

{
    my $x = 5;
    sub c() { $x }
    sub d() { c() }
    BEGIN d();
    $x = 7;
    is d(), 7, 'a sub reached through another sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    sub c() { -> { $x } }
    BEGIN c()();
    $x = 7;
    is c()(), 7, 'a closure taken inside a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    sub c() { $x }
    my &saved = BEGIN { c(); &c };
    $x = 7;
    is saved(), 7, 'the routine object a BEGIN block hands out reads the runtime value';
    is c(), 7, 'the block lexical for that routine reads the runtime value';
}

{
    my @seen;
    for 1, 2 {
        my $x = $_;
        sub c() { $x }
        BEGIN c();
        @seen.push(c());
    }
    is-deeply @seen, [1, 2], 'each entry of a loop body clones the sub over its own lexical';
}

{
    my @closures;
    for 1, 2 {
        my $x = $_;
        my &cl = sub c() { $x }
        BEGIN c();
        @closures.push(&cl);
    }
    is-deeply @closures.map({ .() }).list, (1, 2), 'closures kept from each loop entry read their own lexical';
}

{
    use soft;
    sub s() { 1 }
    sub c() { s() }
    BEGIN c();
    &s.wrap(-> { 42 });
    is c(), 42, 'a runtime wrap of a sub is seen by a caller that ran at BEGIN time';
}

{
    my $x = 5;
    sub c() { $x }
    BEGIN c();
    &c.wrap(-> { 'w' ~ callsame });
    $x = 7;
    is c(), 'w7', 'a runtime wrap of a sub called at BEGIN time wraps its runtime clone';
}

{
    use soft;
    sub s() { 1 }
    sub c() { s() }
    BEGIN { &c.wrap(-> { 'w' ~ callsame }); c() }
    &s.wrap(-> { 42 });
    is c(), 'w42', 'a sub wrapped and called at BEGIN time still sees a runtime wrap of its callee';
}

{
    sub c() { state $n = 0; ++$n }
    BEGIN c();
    is c(), 1, 'a state variable in a block sub starts fresh at runtime';
}

sub unit-state() { state $n = 0; ++$n }
BEGIN unit-state();
is unit-state(), 1, 'a state variable in a unit sub starts fresh at runtime';

sub unit-wrapped() { 1 }
sub unit-caller() { unit-wrapped() }
BEGIN unit-caller();
&unit-wrapped.wrap(-> { 42 });
is unit-caller(), 42, 'a runtime wrap of a unit sub is seen by a unit caller that ran at BEGIN time';

{
    my $x = 5;
    sub with-default($y = { $x }) { $y }
    BEGIN with-default();
    $x = 7;
    is with-default().(), 7,
        'a closure a parameter default builds reads the runtime value';
}

{
    my $x = 5;
    sub default-expr($n = $x) { $n }
    BEGIN default-expr();
    $x = 7;
    is default-expr(), 7,
        'an expression a parameter default evaluates reads the runtime value';
}

{
    my $x = 5;
    sub with-try() { try $x }
    BEGIN with-try();
    $x = 7;
    is with-try(), 7,
        'a statement prefix thunk in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-enter() { ENTER { $seen = $x }; 1 }
    BEGIN with-enter();
    $x = 7;
    with-enter();
    is $seen, 7, 'an ENTER phaser with a block body reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-leave() { LEAVE { $seen = $x }; 1 }
    BEGIN with-leave();
    $x = 7;
    with-leave();
    is $seen, 7, 'a LEAVE phaser with a block body reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-keep() { KEEP { $seen = $x }; 1 }
    BEGIN with-keep();
    $x = 7;
    with-keep();
    is $seen, 7, 'a KEEP phaser with a block body reads the runtime value';
}

our $temporized = 1;
{
    my $x = 5;
    my $seen;
    sub with-temp() { temp $temporized = $x; $seen = $temporized; 1 }
    BEGIN with-temp();
    $x = 7;
    with-temp();
    is $seen, 7, 'a temp in a sub called at BEGIN time assigns the runtime value';
}

{
    my $x = 5;
    my regex matcher { $x }
    sub with-regex() { so 'q' ~~ &matcher }
    BEGIN with-regex();
    $x = 'q';
    is with-regex(), True,
        'a regex a sub called at BEGIN time matches interpolates the runtime value';
}

{
    my $x = 5;
    sub outer-routine() { sub inner-routine() { $x }; inner-routine() }
    BEGIN outer-routine();
    $x = 7;
    is outer-routine(), 7,
        'a sub nested in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    proto explicit-proto(|) {*}
    multi explicit-proto() { $x }
    BEGIN explicit-proto();
    $x = 7;
    is explicit-proto(), 7,
        'a candidate of an explicit proto reads the runtime value';
}

{
    my $x = 5;
    sub read-from-thread() { $x }
    BEGIN read-from-thread();
    $x = 7;
    is await(start read-from-thread()), 7,
        'a sub called at BEGIN time reads the runtime value on another thread';
}

{
    sub hands-out-closure() { -> $y { { $y } } }
    my &saved = BEGIN hands-out-closure();
    is saved(9), 9,
        'a closure handed out by a sub called at BEGIN time still runs once the sub is bound again';
}

# A loop clones its body block, and the clone copies that block's NEXT,
# LAST, QUIT and CLOSE phasers, so such a phaser binds before the clone.

{
    my $x = 5;
    my $seen;
    sub with-next() { for 1..2 { NEXT $seen = $x }; 1 }
    BEGIN with-next();
    $x = 7;
    with-next();
    is $seen, 7, 'a NEXT phaser of a loop in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-last() { for 1..2 { LAST $seen = $x }; 1 }
    BEGIN with-last();
    $x = 7;
    with-last();
    is $seen, 7, 'a LAST phaser of a loop in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    sub with-start() { start { $x } }
    BEGIN with-start();
    $x = 7;
    is await(with-start()), 7,
        'a start block in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    sub with-react() {
        my @seen;
        react { whenever supply { emit $x } -> $v { @seen.push($v); done } }
        @seen[0]
    }
    BEGIN with-react();
    $x = 7;
    is with-react(), 7,
        'a whenever block in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-while-next() { my $i = 0; while $i++ < 2 { NEXT $seen = $x }; 1 }
    BEGIN with-while-next();
    $x = 7;
    with-while-next();
    is $seen, 7,
        'a NEXT phaser of a while loop in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-quit() {
        react { whenever supply { die 'stop' } { QUIT { $seen = $x; done; True } } }
        1
    }
    BEGIN { try with-quit() }
    $x = 7;
    try with-quit();
    is $seen, 7, 'a QUIT phaser in a sub called at BEGIN time reads the runtime value';
}

# A statement prefix with a block body hands that block out as its code
# object, so each prefix reaches the block through the same closure.

{
    my $x = 5;
    sub with-gather() { gather { take $x } }
    BEGIN with-gather().list;
    $x = 7;
    is with-gather().list[0], 7,
        'a gather block in a sub called at BEGIN time takes the runtime value';
}

{
    my $x = 5;
    sub with-do() { do { $x } }
    BEGIN with-do();
    $x = 7;
    is with-do(), 7, 'a do block in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    sub with-try-block() { try { $x } }
    BEGIN with-try-block();
    $x = 7;
    is with-try-block(), 7,
        'a try block in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    sub with-once() { once { $x } }
    BEGIN with-once();
    $x = 7;
    is with-once(), 7, 'a once block in a sub called at BEGIN time reads the runtime value';
}

# A loop body that runs immediate has no clone of its own, so entering
# the block is the only site its phasers bind at.

{
    my $x = 5;
    my $seen;
    sub with-until-next() { my $i = 0; until $i++ >= 2 { NEXT $seen = $x }; 1 }
    BEGIN with-until-next();
    $x = 7;
    with-until-next();
    is $seen, 7,
        'a NEXT phaser of an until loop in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-repeat-next() { my $i = 0; repeat { NEXT $seen = $x } while $i++ < 1; 1 }
    BEGIN with-repeat-next();
    $x = 7;
    with-repeat-next();
    is $seen, 7,
        'a NEXT phaser of a repeat loop in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-loop-last() { loop (my $i = 0; $i < 2; $i++) { LAST $seen = $x }; 1 }
    BEGIN with-loop-last();
    $x = 7;
    with-loop-last();
    is $seen, 7,
        'a LAST phaser of a loop statement in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-first-in-loop() { for 1..2 { FIRST $seen = $x }; 1 }
    BEGIN with-first-in-loop();
    $x = 7;
    with-first-in-loop();
    is $seen, 7,
        'a FIRST phaser of a loop in a sub called at BEGIN time reads the runtime value';
}

our $let-restored = 1;
{
    my $x = 5;
    my $seen;
    sub with-let() { let $let-restored = $x; $seen = $let-restored; fail 'undo' }
    BEGIN { try with-let() }
    $x = 7;
    try with-let();
    is $seen, 7, 'a let in a sub called at BEGIN time assigns the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-close() {
        my $tap = (supply { CLOSE { $seen = $x }; emit 1 }).tap;
        $tap.close;
        1
    }
    BEGIN with-close();
    $x = 7;
    $seen = Nil;
    with-close();
    is $seen, 7, 'a CLOSE phaser of a supply in a sub called at BEGIN time reads the runtime value';
}

# PRE and POST walk the same rebind list as the other phasers, and
# UNDO rides the LEAVE order.

{
    my $x = 5;
    my $seen;
    sub with-pre() { PRE { $seen = $x; True }; 1 }
    BEGIN with-pre();
    $x = 7;
    with-pre();
    is $seen, 7, 'a PRE phaser in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-post() { POST { $seen = $x; True }; 1 }
    BEGIN with-post();
    $x = 7;
    with-post();
    is $seen, 7, 'a POST phaser in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my $seen;
    sub with-undo() { UNDO $seen = $x; fail 'undo' }
    BEGIN { try with-undo() }
    $x = 7;
    try with-undo();
    is $seen, 7, 'an UNDO phaser in a sub called at BEGIN time reads the runtime value';
}

{
    my $x = 5;
    my class WithMethod { method m() { $x } }
    BEGIN WithMethod.m;
    $x = 7;
    is WithMethod.m, 7,
        'a method of a class in a block called at BEGIN time reads the runtime value';
}

# A constant's value runs in a compilation of its own that holds the
# thunk around the value and nothing else, so a statement prefix of a
# bare statement, which that thunk takes a closure of, is declared by
# the thunk rather than by an enclosing scope.

{
    constant $tried = try 42;
    is $tried, 42, 'a constant whose value is a try of a bare statement';
}

{
    constant @gathered = gather take 1;
    is-deeply @gathered, (1,), 'a constant whose value is a gather of a bare statement';
}

{
    constant $started = start 42;
    is $started.result, 42, 'a constant whose value is a start of a bare statement';
}

{
    constant $onced = once 42;
    is $onced, 42, 'a constant whose value is a once of a bare statement';
}

# A thunk with an enclosing scope leaves that declaration to the scope.
# Declared inside the thunk as well, the thunk's frame would capture a
# block whose outer is the scope.

{
    my $modified = 'a';
    s/(.)/x/ for $modified;
    is $modified, 'x', 'a substitution under a for modifier runs from its enclosing scope';
}

# A runtime EVAL is itself a dynamic compilation.

is EVAL(q[
    my $x = 5;
    sub evaled() { $x }
    BEGIN evaled();
    $x = 7;
    evaled();
]), 7, 'a sub called at BEGIN time inside a runtime EVAL reads the runtime value';
