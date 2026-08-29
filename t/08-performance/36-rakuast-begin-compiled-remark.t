use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 114;

# A routine invoked at BEGIN time compiles ahead of the unit's optimize
# walk and caches its QAST. That compilation runs its own optimize walk
# and lowering first. The unit emission re-forms the cached block after
# the unit's own walk, so a precompiled unit runs such routines with
# the same lowerings as routines compiled in the ordinary order, while
# a script runs the frames of the early compilation. The behavioral
# tests hold on both frontends. The QAST shape tests are specific to
# the RakuAST frontend.

# Dynamic compilation is per code object, so each method the BEGIN
# block invokes compiles ahead of the optimize walk.
my class Counter {
    has int $!i;
    method bump() { ++$!i }
    method value() { $!i }
    method named-count(*%_) { %_.elems }
    method own-name() { &?ROUTINE.name }
}
BEGIN {
    my $c := Counter.new;
    $c.bump;
    $c.named-count(:x);
    $c.own-name;
}

{
    my $c := Counter.new;
    $c.bump;
    $c.bump;
    is $c.value, 2,
        'a method invoked at BEGIN time still increments correctly at runtime';
    is $c.bump, 3,
        'the increment result value is correct after a BEGIN time use';
}

is Counter.new.named-count(:a, :b), 2,
    'the implicit named slurpy still collects arguments after a BEGIN time use';

is Counter.new.own-name, 'own-name',
    'a routine variable read still works after a BEGIN time use';

# The early compilation carries the optimize walk's marks. A chained
# comparison is one the walk marks for a static callee lookup, which
# the dynamic compilation must resolve by value instead.
my class Within {
    method check($x) { 1 <= $x <= 3 }
}
BEGIN Within.new.check(2);
is Within.new.check(2), True,
    'a chained comparison in a method used at BEGIN time still holds at runtime';
is Within.new.check(5), False,
    'a chained comparison in a method used at BEGIN time still fails at runtime';

# The walk ahead of the unit resolves names in scopes the parse is
# still adding to, so a declaration made after the BEGIN time use must
# still reach the unit's own emission. The BEGIN statement's own walk
# consults the unit scope for the call to the sub.
sub twice($x) { $x * 2 }
BEGIN twice(1);
my $declared-after = 5;
is twice($declared-after), 10,
    'a lexical declared after a BEGIN time call of a sub is declared in the unit';

# A routine compiled ahead of the unit consults the unit scope for a
# sub, a constant, and an enum value it uses.
sub helper($x) { $x + 1 }
my constant K = 100;
enum Colour <Red Green>;
sub uses-unit-symbols($x) { helper($x) + K + Green }
BEGIN uses-unit-symbols(1);
my $declared-after-too = 5;
is uses-unit-symbols($declared-after-too), 107,
    'a lexical declared after a BEGIN time call of a routine using unit symbols is declared in the unit';

# A role body compiles at the end of the role declaration, before a
# sub declared later in the unit exists, and a call to that sub from a
# role method resolves once the unit is complete.
my role Forward {
    method call-later() { declared-later(1) }
}
my class UsesForward does Forward { }
sub declared-later($x) { $x + 1 }
is UsesForward.new.call-later, 2,
    'a role method calling a sub declared later in the unit resolves the call';

# The early compilation flattens an inner body block into its frame.
my class Branch {
    method m($b) { my $s = 0; if $b { my $t = $b + 1; $s = $t }; $s }
}
BEGIN Branch.new.m(1);
is Branch.new.m(0), 0,
    'an untaken flattened inner branch leaves the outer variable alone after a BEGIN time use';
is Branch.new.m(41), 42,
    'a taken flattened inner branch writes the outer variable after a BEGIN time use';

# A stub role declared inside a role body, or inside a routine used at
# BEGIN time, is formed with the enclosing block ahead of the unit and
# has no fixup nodes to put back.
my role Outer {
    my role Inner { ... }
    method m() { 1 }
}
my class UsesOuter does Outer { }
is UsesOuter.new.m, 1,
    'a stub role inside a role body compiles alongside the role';
sub declares-stub() { my role Stubbed { ... }; 1 }
BEGIN declares-stub();
is declares-stub(), 1,
    'a stub role inside a routine used at BEGIN time compiles alongside it';

# A role body's lexicals are reached by name from its methods, which
# are cloned per concretization with the body frame as their outer, so
# the body's lowering keeps every captured one as a lexical.
my role Tally {
    my $count = 0;
    my int $native = 0;
    my $branch = 0;
    if True { my $t = 5; $branch = $t }
    method bump() { ++$count }
    method nbump() { ++$native }
    method branch() { $branch }
    method nameds() { %_.elems }
}
my class Tallied does Tally { }
BEGIN { Tallied.new.bump; Tallied.new.nbump }
Tallied.new.bump;
is Tallied.new.bump, 3,
    'a role body lexical a method increments keeps its value after a BEGIN time use';
is Tallied.new.nbump, 2,
    'a native role body lexical a method increments keeps its value after a BEGIN time use';
is Tallied.new.branch, 5,
    'a role body lexical assigned in a body level block reads back from a method';
is Tally.new.bump, 1,
    'punning the role runs the body afresh with its own lexicals';
is Tallied.new.nameds(:x, :y, :z), 3,
    'a role method reading its implicit slurpy still collects named arguments';

# The elided slurpy binding still accepts and discards stray nameds.
# A call that fails its constraint reruns through the full binder,
# which binds the parameters it reaches into the frame by name.
my class Stray {
    method m() { 42 }
    method n($x where * > 0) { $x }
}
BEGIN { Stray.new.m; Stray.new.n(1) }
is Stray.new.m(:ignored, :also), 42,
    'a BEGIN used method with an unused slurpy still discards stray nameds';
is Stray.new.n(5, :stray), 5,
    'a BEGIN used method with a constrained param still binds with stray nameds';
throws-like { Stray.new.n(-1) }, X::TypeCheck::Binding::Parameter,
    'a call failing its constraint after a BEGIN use reports through the full binder';

# A call the frame certainly reaches is reported at the BEGIN time
# compilation when its routine is declared only later, whether it sits
# under a statement modifier or a short circuit operator.
throws-like { EVAL 'BEGIN { later-mod() if True }; sub later-mod() { 1 }' }, X::Undeclared::Symbols,
    'a call under a statement modifier in a BEGIN block to a sub declared later is reported at compile time';
throws-like { EVAL 'BEGIN { True && later-and() }; sub later-and() { 1 }' }, X::Undeclared::Symbols,
    'a call under a short circuit in a BEGIN block to a sub declared later is reported at compile time';

# The implicit marks decide what a re-formed frame elides, so an
# implicit the routine reads must keep its full setup there.
my class UsesErr {
    method m() { try die 'boom'; $!.message }
}
BEGIN UsesErr.new.m;
is UsesErr.new.m, 'boom',
    'an error variable read still works after a BEGIN time use';

my class UsesMatch {
    method m(Str $s) { $s ~~ /\d+/; ~$/ }
}
BEGIN UsesMatch.new.m("a1");
is UsesMatch.new.m("x42"), '42',
    'a match variable read still works after a BEGIN time use';

my class UsesBlock {
    method m() { &?BLOCK.arity }
}
BEGIN UsesBlock.new.m;
is UsesBlock.new.m, 1,
    'a block variable read still works after a BEGIN time use';

# A role body forms ahead of the unit, and its methods form with it.
my role Countable {
    has int $!n;
    method tick() { ++$!n }
}
my class Ticker does Countable { }
BEGIN {
    my $t := Ticker.new;
    $t.tick;
}
{
    my $t := Ticker.new;
    $t.tick;
    is $t.tick, 2,
        'a role method invoked at BEGIN time still increments correctly at runtime';
}

# A closure formed inside a BEGIN compiled routine still captures its
# frame after the re-formation, and a closure minted during the BEGIN
# call itself keeps working when invoked at runtime.
my class Closures {
    method make-adder($base) {
        -> $n { $base + $n }
    }
}
our $begin-adder;
BEGIN {
    $begin-adder = Closures.make-adder(5);
}
{
    my $add := Closures.make-adder(10);
    is $add(5), 15,
        'a closure made by a BEGIN compiled method captures its arguments at runtime';
    my $again := Closures.make-adder(20);
    is $again(5), 25,
        'each closure keeps its own captured environment';
    is $add(5), 15,
        'an earlier closure is unaffected by later ones';
    is $begin-adder(3), 8,
        'a closure minted during the BEGIN call still runs correctly at runtime';
}

# The BEGIN made closure captures from inside an inner body block, whose
# frame boundary the serialized shape keeps.
my class CaptureInner {
    method m($b) { my $f; if $b { my $y = $b; $f = -> { $y + 1 } }; $f }
}
our $begin-inner;
BEGIN {
    $begin-inner = CaptureInner.m(41);
}
is $begin-inner(), 42,
    'a BEGIN made closure over an inner body block frame runs correctly at runtime';

# A sub called at BEGIN time to produce a constant keeps working.
my sub double(int $x) { $x * 2 }
my constant doubled = double(21);
is doubled, 42,
    'a sub run at BEGIN time computed the right constant';
is double(5), 10,
    'the same sub still runs correctly at runtime';

# A generic parameter makes the signature emission mint an instantiated
# parameter list local, whose declaring bind must re-emit when the block
# re-forms.
my sub typed(::T $x) { T.^name }
BEGIN typed(42);
is typed("hi"), 'Str',
    'a generic parameter instantiates per call after a BEGIN time use';
is typed(1.5), 'Rat',
    'a second call instantiates the generic independently';

# An inner bare block flattens into the method frame in the early
# compilation as it does in ordinary compilation, and the re-formation
# keeps that shape.
my class Inner {
    method m() { my $r; { my int $x = 1; $r = $x + 2; }; $r }
}
BEGIN Inner.new.m;
is Inner.new.m, 3,
    'a method used at BEGIN with an inner bare block runs correctly at runtime';

# Punning the role at runtime specializes the serialized role body.
{
    my $p := Countable.new;
    $p.tick;
    is $p.tick, 2,
        'punning a role used at BEGIN specializes its body correctly at runtime';
}

# The dispatcher force compiles candidates it needs at BEGIN time.
proto sized(|) {*}
multi sized(Int $x) { 'int' }
multi sized(Str $x) { 'str' }
BEGIN sized(1);
is sized(2), 'int',
    'a multi candidate used at BEGIN still dispatches to it at runtime';
is sized("x"), 'str',
    'the other candidate still dispatches correctly at runtime';

# The temp restore loop goes into the phaser block once, however many
# times the owning routine forms.
our $g = 1;
sub with-temp() { temp $g = 2; $g }
BEGIN with-temp();
is with-temp(), 2,
    'a temp in a sub used at BEGIN sets the new value during the call';
is $g, 1,
    'the temporized variable is restored after the call';

# A LEAVE phaser rides the exit handler setup and the value stash
# symbol the re-formed frame carries.
our $left = 0;
sub with-leave() { LEAVE $left++; 'r' }
BEGIN with-leave();
with-leave();
is $left, 1,
    'a LEAVE phaser in a sub used at BEGIN fires once per runtime call';

# On both frontends a FIRST phaser in a sub invoked at BEGIN fires
# during that call, so a runtime call finds the trigger container
# already set. This pins that the runtime frame consults the container
# the BEGIN compilation minted, not a fresh one from a later formation.
our $first-runs = 0;
sub with-first() { FIRST $first-runs++; 'x' }
BEGIN { with-first(); with-first() }
with-first();
with-first();
is $first-runs, 0,
    'the runtime frame consults the FIRST trigger container the BEGIN compilation minted';

# A placeholder signature takes the reset on its own signature object.
sub ph { $^a + 1 }
BEGIN ph(1);
is ph(41), 42,
    'a placeholder sub used at BEGIN still runs correctly at runtime';

# A BEGIN use inside a runtime EVAL compiles in a nested context, which
# starts with its own optimize state.
is EVAL(q[my class E { has int $!i; method m() { ++$!i } }; my $e := E.new; BEGIN E.new.m; $e.m; $e.m]), 2,
    'a BEGIN use inside a runtime EVAL compiles and runs correctly';

# The graft's registrations only matter across a store and load
# boundary, so a precompiled module exercises what the in-process tests
# cannot: the serialized unit must carry the re-formed frames with
# their phaser blocks attached and their closures resolvable.
{
    my $dir = $*TMPDIR.add("rakuast-begin-remark-{$*PID}");
    $dir.mkdir;
    $dir.add('PrecompRemark.rakumod').spurt(q:to/END/);
        unit module PrecompRemark;
        our $g = 1;
        our sub t($v) { temp $g = $v; "in=$g" }
        our sub check-temp() { my $in = t(5); "$in after=$g" }
        our sub adder($base) { -> $n { $base + $n } }
        our $begin-closure;
        our sub check-begin-closure() { $begin-closure(3) }
        our sub inner-closure($b) {
            my $f;
            if $b { my $y = $b; $f = -> { $y + 1 } }
            $f
        }
        our $begin-inner;
        our sub check-inner() { $begin-inner() }
        our sub flat($b) { my $r = 0; if $b { my $t = $b + 1; $r = $t }; $r }
        our sub bump-late() { my int $i = 1; $i++; $i }
        role RL {
            my $count = 0;
            my int $native = 0;
            my $branch = 0;
            if True { my $t = 5; $branch = $t }
            method bump() { my int $i = 1; $i++; $i }
            method count() { ++$count }
            method ncount() { ++$native }
            method branch() { $branch }
            method nameds() { %_.elems }
            method idx(int $i) { my int @a = 1,2,3; @a[$i] }
            method via-sub() { soft-target(5) }
        }
        class RC does RL { }
        role Gen[::T] { has T $.x; method t() { T.^name } }
        class GenInt does Gen[Int] { }
        our sub soft-target(int $a) { $a + 1 }
        our sub soft-caller(int $i) { soft-target($i) }
        our sub soft-index(int $i) { my int @a = 1,2,3; @a[$i] }
        our sub guarded($b) { if $b { declared-below(1) } else { 0 } }
        our sub g-unless($b) { my $r = 0; unless $b { $r = declared-below(1) }; $r }
        our sub g-while($n) { my $i = 0; while $i < $n { $i = declared-below($i) }; $i }
        our sub nested-role() { my role NR { my $c = 0; method m() { ++$c } }; my class NC does NR { }; NC.new.m }
        our sub nested-role-attr() { my role NA { has $.a = 42; method m() { $!a + 1 } }; my class NCA does NA { }; NCA.new.m }
        our $guarded-at-begin;
        our sub var-op() { my $s = 0; for 1..3 { $s = $s + $_ }; $s }
        grammar PG { token TOP { ( "ab" | "a" | ( "b" ) ) } }
        BEGIN PG.parse("ab");
        class PC {
            has int $!i;
            method bump() { ++$!i }
            method named(*%_) { %_.elems }
        }
        BEGIN {
            t(2);
            PC.new.bump;
            PC.new.named(:x);
            $begin-closure = adder(5);
            $begin-inner = inner-closure(41);
            flat(1);
            bump-late();
            RC.new.count;
            soft-caller(1);
            soft-index(0);
            $guarded-at-begin = guarded(0);
            g-unless(1);
            g-while(0);
            nested-role();
            nested-role-attr();
            var-op();
        }
        our sub declared-below($x) { $x + 1 }
        sub postfix:<++>($a is rw) { $a = 100 }
        use soft;
        my &infix:<..> = sub ($a, $b) { (100,) };
        END
    my $repo = CompUnit::Repository::FileSystem.new(:prefix($dir.Str));
    CompUnit::RepositoryRegistry.use-repository($repo);
    require ::('PrecompRemark');
    is &::('PrecompRemark::check-temp')(), 'in=5 after=1',
        'a precompiled temp routine used at BEGIN restores through its own frame';
    is ::('PrecompRemark::PC').new.named(:a, :b), 2,
        'a precompiled named slurpy method used at BEGIN still collects at runtime';
    is &::('PrecompRemark::adder')(10)(5), 15,
        'a precompiled closure maker used at BEGIN captures correctly at runtime';
    is &::('PrecompRemark::check-begin-closure')(), 8,
        'a closure the precompiled BEGIN block minted runs correctly at runtime';
    is &::('PrecompRemark::check-inner')(), 42,
        'a closure minted inside an inner block at BEGIN keeps its frame chain across precompilation';
    is &::('PrecompRemark::flat')(41), 42,
        'a precompiled routine with a flattened inner branch used at BEGIN computes at runtime';
    is &::('PrecompRemark::bump-late')(), 100,
        'an operator declared after a BEGIN time use of a precompiled routine still shadows the core one in it';
    is ::('PrecompRemark::RC').new.bump, 100,
        'an operator declared after a precompiled role still shadows the core one in a role method';
    is ::('PrecompRemark::RC').new.ncount, 1,
        'a precompiled role body keeps a native lexical its method increments';
    is ::('PrecompRemark::RC').new.branch, 5,
        'a precompiled role body lexical assigned in a body level block reads back from a method';
    is ::('PrecompRemark::RC').new.nameds(:a, :b), 2,
        'a precompiled role method reading its implicit slurpy still collects named arguments';
    is ::('PrecompRemark::GenInt').new(x => 1).t, 'Int',
        'a precompiled parametric role instantiates its type parameter';
    is ::('PrecompRemark::GenInt').new(x => 1).x, 1,
        'a precompiled parametric role keeps the accessor of its generic attribute';
    is $::('PrecompRemark::guarded-at-begin'), 0,
        'a precompiled BEGIN time call leaves an untaken call to a sub declared later intact';
    is &::('PrecompRemark::guarded')(1), 2,
        'a precompiled routine used at BEGIN reaches a sub declared later through a branch at runtime';
    is &::('PrecompRemark::g-unless')(0), 2,
        'a precompiled routine used at BEGIN reaches a sub declared later through an unless body at runtime';
    is &::('PrecompRemark::g-while')(3), 3,
        'a precompiled routine used at BEGIN reaches a sub declared later through a while body at runtime';
    is &::('PrecompRemark::nested-role')(), 2,
        'a role declared in a precompiled routine used at BEGIN keeps its body lexical across calls';
    is &::('PrecompRemark::nested-role-attr')(), 43,
        'a role declared in a precompiled routine used at BEGIN reads its attribute in a method';
    is &::('PrecompRemark::var-op')(), 100,
        'an operator bound to a variable after a BEGIN time use shadows the core one in a precompiled routine';
    &::('PrecompRemark::soft-target').wrap(-> | { 'wrapped' });
    is &::('PrecompRemark::soft-caller')(5), 'wrapped',
        'the soft pragma parsed after a BEGIN time use keeps a precompiled routine wrappable';
    is ::('PrecompRemark::RC').new.via-sub, 'wrapped',
        'the soft pragma parsed after a precompiled role keeps a sub its method calls wrappable';
    my ($sub-index, $role-index);
    {
        my $index-wrap = &postcircumfix:<[ ]>.wrap(-> | { 'wrapped' });
        LEAVE $index-wrap.restore;
        $sub-index  = &::('PrecompRemark::soft-index')(1);
        $role-index = ::('PrecompRemark::RC').new.idx(1);
    }
    is $sub-index, 'wrapped',
        'the soft pragma parsed after a BEGIN time use keeps a native subscript in a precompiled routine wrappable';
    is $role-index, 'wrapped',
        'the soft pragma parsed after a precompiled role keeps a native subscript in its method wrappable';
    is ::('PrecompRemark::PG').parse("ab").Str, 'ab',
        'a precompiled grammar parsed at BEGIN still picks the longest alternative';
    is ::('PrecompRemark::PG').parse("a")[0].Str, 'a',
        'a precompiled grammar parsed at BEGIN still captures its alternation';
    is ::('PrecompRemark::PG').parse("b")[0][0].Str, 'b',
        'a capturing group nested in another group survives precompilation of a BEGIN used grammar';
    sub nuke(IO::Path $d) {
        for $d.dir { $_.d ?? nuke($_) !! $_.unlink }
        $d.rmdir;
    }
    nuke($dir);
}

# Compiling a regex body stores its alternation NFAs on the code object,
# and a second formation must not add a second set.
my grammar Alts { token TOP { "xx" | "yy" | "zz" } }
BEGIN Alts.parse("yy");
{
    my \alt-nfas = nqp::getattr(Alts.^lookup('TOP'), Regex, '%!alt_nfas');
    is (nqp::isnull(alt-nfas) ?? 0 !! nqp::elems(alt-nfas)), 1,
        'a token with alternations parsed at BEGIN stores one alternation NFA set';
}

# A capturing group compiles as its own regex code object through a
# formation of its own, so its alternations take the same treatment.
# The BEGIN parse runs the dynamically compiled regex itself, so its
# capture pins that compilation directly.
my grammar CapAlts { token TOP { ("aa" | "bb") "-" } }
my $begin-cap;
BEGIN $begin-cap = ~CapAlts.parse("aa-")[0];
is $begin-cap, 'aa',
    'the BEGIN time parse itself captures the matched alternative';
is CapAlts.parse("bb-")[0].Str, 'bb',
    'a captured alternation in a token parsed at BEGIN still captures at runtime';
is CapAlts.parse("aa-")[0].Str, 'aa',
    'the other alternative of the captured group still matches at runtime';

# A quantified group takes the walk through the quant node into the
# group, so its alternation still drives longest token matching.
my grammar QuantAlts { token TOP { [ "a" | "ab" ]+ } }
BEGIN QuantAlts.parse("a");
{
    my \alt-nfas = nqp::getattr(QuantAlts.^lookup('TOP'), Regex, '%!alt_nfas');
    is (nqp::isnull(alt-nfas) ?? 0 !! nqp::elems(alt-nfas)), 1,
        'a quantified alternation parsed at BEGIN stores one alternation NFA set';
}
is QuantAlts.parse("ab").Str, 'ab',
    'a quantified alternation parsed at BEGIN still picks the longest alternative';

# QAST is a DAG: a Want's alternatives share subtrees with its primary,
# so a walk must remember visited nodes or it re-walks shared regions
# once per path to them.
sub qast-find-block(Mu $qast, str $name, %seen = {}) {
    return Mu unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return Mu if %seen{$id};
    %seen{$id} = True;
    if nqp::istype($qast, QAST::Block) && $qast.name eq $name {
        return $qast;
    }
    for $qast.list {
        my \found = qast-find-block($_, $name, %seen);
        return found if nqp::istype(found, QAST::Block);
    }
    Mu
}

sub qast-deep-has-op(Mu $qast, str $op, %seen = {} --> Bool:D) {
    return False unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return False if %seen{$id};
    %seen{$id} = True;
    if nqp::istype($qast, QAST::Op) {
        return True if $qast.op eq $op;
    }
    for $qast.list {
        qast-deep-has-op($_, $op, %seen) and return True;
    }
    False
}

sub qast-deep-has-call(Mu $qast, str $name, %seen = {} --> Bool:D) {
    return False unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return False if %seen{$id};
    %seen{$id} = True;
    return True if nqp::istype($qast, QAST::Op) && $qast.name eq $name;
    for $qast.list {
        return True if qast-deep-has-call($_, $name, %seen);
    }
    False
}

# The binding a kept slurpy or routine variable gets is a plain Var use
# beside the declaration, so an elision leaves only the declaration.
# The walk stays within the frame the root block owns, so nested block
# boundaries end the descent.
sub qast-uses-lexical(Mu $qast, str $name, Mu :$root = $qast, :%seen = {} --> Bool:D) {
    return False unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return False if %seen{$id};
    %seen{$id} = True;
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'lexical' && $qast.name eq $name && !$qast.decl;
    }
    return False if nqp::istype($qast, QAST::Block)
        && !nqp::eqaddr(nqp::decont($qast), nqp::decont($root));
    for $qast.list {
        qast-uses-lexical($_, $name, :$root, :%seen) and return True;
    }
    False
}

# Counts every appearance of a block by name, re-walking a subtree
# each time a parent lists it, so one subtree spliced twice counts its
# blocks twice. The guard is a path set rather than a seen set, which
# keeps the walk cycle safe without deduplicating the recount.
sub qast-block-occurrences(Mu $qast, str $name, :%on-path = {} --> Int:D) {
    return 0 unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return 0 if %on-path{$id};
    my int $count = nqp::istype($qast, QAST::Block) && $qast.name eq $name ?? 1 !! 0;
    %on-path{$id} = True;
    for $qast.list {
        $count = $count + qast-block-occurrences($_, $name, :%on-path);
    }
    %on-path{$id} = False;
    $count
}

# Matches a declaration instead of a use, with the same root block
# boundary rule as qast-uses-lexical.
sub qast-declares-lexical(Mu $qast, str $name, Mu :$root = $qast, :%seen = {} --> Bool:D) {
    return False unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return False if %seen{$id};
    %seen{$id} = True;
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'lexical' && $qast.name eq $name && $qast.decl;
    }
    return False if nqp::istype($qast, QAST::Block)
        && !nqp::eqaddr(nqp::decont($qast), nqp::decont($root));
    for $qast.list {
        qast-declares-lexical($_, $name, :$root, :%seen) and return True;
    }
    False
}

# Matches a declaration in its plain var form. The contvar and param
# setups fall outside it, so it distinguishes an elided container
# implicit from its full setup. A routine variable's full setup also
# declares a var, so its shape assertions pair this with a check for
# the ops the setup emits.
sub qast-declares-bare-lexical(Mu $qast, str $name, Mu :$root = $qast, :%seen = {} --> Bool:D) {
    return False unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return False if %seen{$id};
    %seen{$id} = True;
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'lexical' && $qast.name eq $name
            && $qast.decl eq 'var';
    }
    return False if nqp::istype($qast, QAST::Block)
        && !nqp::eqaddr(nqp::decont($qast), nqp::decont($root));
    for $qast.list {
        qast-declares-bare-lexical($_, $name, :$root, :%seen) and return True;
    }
    False
}

sub qast-counts-nested-blocks(Mu $qast, Mu :$root = $qast, :%seen = {} --> Int:D) {
    return 0 unless nqp::istype($qast, QAST::Node);
    my str $id = ~nqp::objectid($qast);
    return 0 if %seen{$id};
    %seen{$id} = True;
    my int $count = nqp::istype($qast, QAST::Block)
        && !nqp::eqaddr(nqp::decont($qast), nqp::decont($root)) ?? 1 !! 0;
    for $qast.list {
        $count = $count + qast-counts-nested-blocks($_, :$root, :%seen);
    }
    $count
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    # Control: with no BEGIN use the attribute increment lowers to add_i.
    qast-is 'my class K1 { has int $!i; method m() { ++$!i } }; K1.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'add_i')
    }, 'control: a native attribute increment lowers to add_i without a BEGIN use';

    # The same method compiled dynamically by a BEGIN use gets the same
    # lowering in the frame the unit emits.
    qast-is 'my class K2 { has int $!i; method m() { ++$!i } }; BEGIN K2.new.m; K2.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'add_i')
    }, 'a native attribute increment lowers to add_i after a BEGIN time use';

    # The re-formed frame drops the per-call setup of its unused
    # implicits but keeps their lexical names, since a context the
    # early compilation serialized rebinds by name.
    qast-is 'my class K3 { has int $!i; method m() { ++$!i } }; BEGIN K3.new.m; K3.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block)
            && !qast-uses-lexical(block, '%_')
            && !qast-deep-has-op(block, 'getcodeobj')
            && qast-declares-bare-lexical(block, '%_')
            && qast-declares-bare-lexical(block, '&?ROUTINE')
            && qast-declares-bare-lexical(block, '$_')
            && qast-declares-bare-lexical(block, '$/')
            && qast-declares-bare-lexical(block, '$!')
            && qast-declares-bare-lexical(block, '$¢')
    }, 'the re-formed frame elides unused implicit setup while keeping the lexical names';

    # Pairs with the assertion above: without a BEGIN use the unused
    # implicits lose both setup and name. The slurpy target is the
    # exception, keeping its bare slot in every frame for the full
    # binder, so it stays off this list.
    qast-is 'my class K4 { has int $!i; method m() { ++$!i } }; K4.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block)
            && !qast-uses-lexical(block, '%_')
            && !qast-deep-has-op(block, 'getcodeobj')
            && !qast-declares-lexical(block, '&?ROUTINE')
            && !qast-declares-lexical(block, '$_')
            && !qast-declares-lexical(block, '$/')
            && !qast-declares-lexical(block, '$!')
            && !qast-declares-lexical(block, '$¢')
    }, 'control: the unused implicits other than the slurpy target are not declared without a BEGIN use';

    qast-is 'sub f() { my int $x = 1; $x + 2 }; BEGIN f(); f', :full, -> \v {
        my \block = qast-find-block(v, 'f');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'add_i')
    }, 'a sub used at BEGIN time still lowers its addition to add_i';

    qast-is 'sub g() { my int $x = 1; $x + 2 }; g', :full, -> \v {
        my \block = qast-find-block(v, 'g');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'add_i')
    }, 'control: the same sub lowers identically without a BEGIN use';

    # A package declared inside the routine splices its generated
    # accessor and POPULATE QAST into the package body's cached block,
    # which the re-formation consults again, so the splice must land
    # exactly once.
    qast-is 'sub k() { my class KP { has $.a = 42 }; KP.new.a }; BEGIN k(); k()', :full, -> \v {
        qast-block-occurrences(v, 'POPULATE') == 1 && qast-block-occurrences(v, 'a') == 1
    }, 'a class declared in a sub used at BEGIN emits its generated methods once';

    qast-is 'sub k2() { my class KQ { has $.a = 42 }; KQ.new.a }; k2()', :full, -> \v {
        qast-block-occurrences(v, 'POPULATE') == 1 && qast-block-occurrences(v, 'a') == 1
    }, 'control: the same class emits its generated methods once without a BEGIN use';

    # A role body forms its methods with it ahead of the unit, and the
    # unit re-forms them once the marks are settled.
    qast-is 'my role R1 { has int $!i; method m() { ++$!i } }; my class C1 does R1 { }; C1.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'add_i')
    }, 'a native attribute increment in a role method lowers to add_i';

    qast-is 'my role R2 { my $c = 0; my $u = 5; method m() { ++$c } }; my class C2 does R2 { }; C2.new.m', :full, -> \v {
        my \body = qast-find-block(v, 'R2');
        my \m = qast-find-block(v, 'm');
        nqp::istype(body, QAST::Block) && nqp::istype(m, QAST::Block)
            && qast-declares-lexical(body, '$c') && qast-uses-lexical(m, '$c')
            && !qast-uses-lexical(body, '$u')
    }, 'a role body keeps a lexical its methods capture and lowers one they do not';

    # The early compilation lowers the routine before it caches the
    # block, so the inner body block flattens there as it does in the
    # control, and the re-formation keeps that shape.
    qast-is 'my class F1 { method m() { my $s = 0; if $s { my $t = 1; $s = $t }; $s } }; BEGIN F1.new.m; F1.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block) && qast-counts-nested-blocks(block) == 0
    }, 'an inner body block flattens after a BEGIN time use';

    qast-is 'my class F2 { method m() { my $s = 0; if $s { my $t = 1; $s = $t }; $s } }; F2.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block) && qast-counts-nested-blocks(block) == 0
    }, 'control: the same inner body block flattens without a BEGIN use';

    # The shapes a method in a class takes, in a method of a role.
    qast-is 'my role RW { method w() { my int $i = 0; while $i < 3 { $i++ }; $i } }; my class CW does RW { }; CW.new.w', :full, -> \v {
        my \block = qast-find-block(v, 'w');
        nqp::istype(block, QAST::Block) && qast-counts-nested-blocks(block) == 0 && qast-deep-has-op(block, 'islt_i')
    }, 'a while loop in a role method flattens its body and lowers its native condition';
    qast-is 'my role RC { method c($x) { 1 <= $x <= 3 } }; my class CC does RC { }; CC.new.c(2)', :full, -> \v {
        my \block = qast-find-block(v, 'c');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'chainstatic')
    }, 'a chained comparison in a role method takes the static chain lookup';
    qast-is 'my role RS { method s() { unit-helper(1) } }; sub unit-helper($x) { $x }; my class CS does RS { }; CS.new.s', :full, -> \v {
        my \block = qast-find-block(v, 's');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'callstatic')
    }, 'a call from a role method to a sub of the unit takes the static call lookup';
    qast-is 'my role RF { method f() { my int $t = 0; for 1..3 { $t = $t + $_ }; $t } }; my class CF does RF { }; CF.new.f', :full, -> \v {
        my \block = qast-find-block(v, 'f');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'while')
    }, 'a range loop in a role method lowers to a native counting loop';
    qast-is 'sub w3() { my int $i = 0; while $i < 3 { $i++ }; $i }; BEGIN w3(); w3()', :full, -> \v {
        my \block = qast-find-block(v, 'w3');
        nqp::istype(block, QAST::Block) && qast-counts-nested-blocks(block) == 0
    }, 'a while loop body in a sub used at BEGIN time flattens';

    # The soft pragma parsed after the early walk withdraws each mark
    # that binds a routine by identity from the frame the unit emits.
    qast-is 'sub s1() { my int $i = 0; ++$i }; BEGIN s1(); use soft; s1()', :full, -> \v {
        my \block = qast-find-block(v, 's1');
        nqp::istype(block, QAST::Block) && !qast-deep-has-op(block, 'add_i')
    }, 'the soft pragma parsed after a BEGIN time use withdraws the native increment lowering';
    qast-is 'sub t1($x) { $x }; sub c1() { t1(1) }; BEGIN c1(); use soft; c1()', :full, -> \v {
        my \block = qast-find-block(v, 'c1');
        nqp::istype(block, QAST::Block) && !qast-deep-has-op(block, 'callstatic') && qast-deep-has-op(block, 'call')
    }, 'the soft pragma parsed after a BEGIN time use withdraws the static call lookup';
    qast-is 'sub t2($x) { $x }; sub c2() { t2(1) }; BEGIN c2(); c2()', :full, -> \v {
        my \block = qast-find-block(v, 'c2');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'callstatic')
    }, 'control: a call to a sub of the unit after a BEGIN time use takes the static call lookup';
    qast-is 'sub r1() { my $s = 0; for 1..3 { $s = $s + $_ }; $s }; BEGIN r1(); use soft; r1()', :full, -> \v {
        my \block = qast-find-block(v, 'r1');
        nqp::istype(block, QAST::Block) && !qast-deep-has-op(block, 'while')
    }, 'the soft pragma parsed after a BEGIN time use withdraws the range loop lowering';
    qast-is 'sub r2() { my $s = 0; for 1..3 { $s = $s + $_ }; $s }; BEGIN r2(); r2()', :full, -> \v {
        my \block = qast-find-block(v, 'r2');
        nqp::istype(block, QAST::Block) && qast-deep-has-op(block, 'while')
    }, 'control: a range loop after a BEGIN time use lowers to a native counting loop';
    qast-is 'sub m1() { my $s = 0; $s = $s + $_ for 1..3; $s }; BEGIN m1(); use soft; m1()', :full, -> \v {
        my \block = qast-find-block(v, 'm1');
        nqp::istype(block, QAST::Block) && !qast-deep-has-op(block, 'while')
    }, 'the soft pragma parsed after a BEGIN time use withdraws the range loop modifier lowering';
    qast-is 'my role SR { method c() { my int $i = 0; ++$i } }; my class SC does SR { }; use soft; SC.new.c', :full, -> \v {
        my \block = qast-find-block(v, 'c');
        nqp::istype(block, QAST::Block) && !qast-deep-has-op(block, 'add_i')
    }, 'the soft pragma parsed after a role withdraws the native increment lowering in its method';

    # A role body's re-formation splices the generated accessor QAST
    # back in exactly once.
    qast-is 'my role RA { has $.a = 42 }; my class CA does RA { }; CA.new.a', :full, -> \v {
        qast-block-occurrences(v, 'a') == 1
    }, 'a role with a public attribute emits its generated accessor once';
    qast-is 'sub k3() { my role RR { method m() { 1 } }; my class KC does RR { }; KC.new.m }; BEGIN k3(); k3()', :full, -> \v {
        qast-block-occurrences(v, 'RR') == 1 && qast-block-occurrences(v, 'm') == 1
    }, 'a role declared in a sub used at BEGIN emits its body and method once';

    # A throw from the walk ahead of the unit, caught by the code that
    # invoked the routine, must leave the unit's own walk intact.
    my $early-throw = q:to/END/;
        my constant &probe-dies = sub ($x) { $x } but role {
            method soft() { state $n = 0; die 'probe' unless $n++; False }
        };
        sub calls-probe() { probe-dies(1) }
        my $caught;
        BEGIN { try calls-probe(); $caught = $!.message }
        my $folded = 2 ** 10;
        "$caught $folded"
        END
    is EVAL($early-throw), 'probe 1024',
        'a throw from the walk ahead of the unit reaches the BEGIN block that invoked the routine';
    qast-is $early-throw, :full, -> \v { !qast-deep-has-call(v, '&infix:<**>') },
        'the unit walk still folds a constant after a caught throw from the walk ahead of the unit';

    # An aborted walk ahead of the unit leaves none of the scopes or
    # packages it entered behind on the resolver a retry compiles with.
    my $retry-after-throw = q:to/END/;
        my constant &probe-dies-once = sub ($x) { $x } but role {
            method soft() { state $n = 0; die 'probe' unless $n++; False }
        };
        sub names-package() {
            my class Inner { method m() { probe-dies-once(1) } }
            Inner.new.m;
            $?PACKAGE.^name
        }
        BEGIN { try names-package() }
        names-package()
        END
    is EVAL($retry-after-throw), 'GLOBAL',
        'a routine retried after a caught throw from the walk ahead of the unit compiles in its own package';

    # A call in a flattened body to a sub declared later is left to the
    # runtime lookup at BEGIN time.
    lives-ok { EVAL 'BEGIN { my $b = False; if $b { later-if() } }; sub later-if() { 1 }' },
        'a call in a flattened if body to a sub declared later compiles at BEGIN time';
    lives-ok { EVAL 'BEGIN { my $b = False; unless !$b { later-unless() } }; sub later-unless() { 1 }' },
        'a call in a flattened unless body to a sub declared later compiles at BEGIN time';
    lives-ok { EVAL 'BEGIN { my $b = False; while $b { later-while() } }; sub later-while() { 1 }' },
        'a call in a flattened while body to a sub declared later compiles at BEGIN time';

    # The rewrites deferred ahead of the unit reach the re-formed frame.
    qast-is 'sub p1() { 2 ** 10 }; BEGIN p1(); p1()', :full, -> \v {
        my \block = qast-find-block(v, 'p1');
        nqp::istype(block, QAST::Block) && !qast-deep-has-call(block, '&infix:<**>')
    }, 'a constant expression in a sub used at BEGIN time folds in the re-formed frame';
    qast-is 'sub p2() { return 1; my $x = 2; $x + 1 }; BEGIN p2(); p2()', :full, -> \v {
        my \block = qast-find-block(v, 'p2');
        nqp::istype(block, QAST::Block) && !qast-deep-has-call(block, '&infix:<+>')
    }, 'statements after a return in a sub used at BEGIN time are dropped from the re-formed frame';
    qast-is 'sub p3() { my role PR { method m() { 2 ** 10 } }; my class PC does PR { }; PC.new.m }; BEGIN p3(); p3()', :full, -> \v {
        !qast-deep-has-call(v, '&infix:<**>')
    }, 'a constant expression in a role method declared in a sub used at BEGIN time folds';
}
else {
    skip-rest 'QAST shapes specific to the RakuAST frontend';
}
