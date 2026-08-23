use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 39;

# A routine invoked at BEGIN time compiles ahead of the unit's optimize
# walk and caches its QAST. The unit emission re-forms the cached block
# once the walk's marks are known, so such routines run at runtime with
# the same lowerings as routines compiled in the ordinary order. The
# behavioral tests hold on both frontends. The QAST shape tests are
# specific to the RakuAST frontend.

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

# A role body appends lexical fixup nodes to its formed block, so it
# declines the re-formation. Its methods still take it.
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

# An inner bare block would flatten into the method frame in ordinary
# compilation, and the lowering holds that off for a frame the early
# compilation serialized.
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
        }
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

    # The frame shape stays as the BEGIN compilation serialized it: the
    # named slurpy and the routine variable remain declared.
    qast-is 'my class K3 { has int $!i; method m() { ++$!i } }; BEGIN K3.new.m; K3.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block)
            && qast-uses-lexical(block, '%_')
            && qast-deep-has-op(block, 'getcodeobj')
    }, 'the re-formed frame keeps its slurpy binding and routine variable population';

    # Pairs with the presence assertion above, so a leaked shape gate
    # would show up here.
    qast-is 'my class K4 { has int $!i; method m() { ++$!i } }; K4.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block)
            && !qast-uses-lexical(block, '%_')
            && !qast-deep-has-op(block, 'getcodeobj')
    }, 'control: the unused slurpy binding and routine variable still elide without a BEGIN use';

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

    # The frame boundary of an inner body block is part of the shape the
    # early compilation serialized, so it survives the re-formation
    # while the control flattens it away.
    qast-is 'my class F1 { method m() { my $s = 0; if $s { my $t = 1; $s = $t }; $s } }; BEGIN F1.new.m; F1.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block) && qast-counts-nested-blocks(block) > 0
    }, 'an inner body block keeps its frame after a BEGIN time use';

    qast-is 'my class F2 { method m() { my $s = 0; if $s { my $t = 1; $s = $t }; $s } }; F2.new.m', :full, -> \v {
        my \block = qast-find-block(v, 'm');
        nqp::istype(block, QAST::Block) && qast-counts-nested-blocks(block) == 0
    }, 'control: the same inner body block flattens without a BEGIN use';
}
else {
    skip-rest 'QAST shapes specific to the RakuAST frontend';
}
