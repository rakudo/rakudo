use Test;
use nqp;

plan 32;

# The bind belongs to the RakuAST frontend, so the legacy frontend runs
# none of this.
unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    skip-rest 'the BEGIN-time compilation bind is specific to the RakuAST frontend';
    exit;
}

# A routine called at BEGIN time is compiled ahead of the unit, in a
# compilation of its own whose outer is a snapshot of the compile-time
# scope. A unit run as a script binds such a routine to the block of its
# own compilation where the routine is declared, so a routine declared in
# a block closes over the frame of the block at runtime and not over that
# snapshot. Each case reads a lexical of an enclosing bare block, which
# is what tells the two compilations apart. A lexical at unit scope
# reads the same either way and would assert nothing.

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
        'a closure a BEGIN-called sub handed out still runs once the sub is bound again';
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
