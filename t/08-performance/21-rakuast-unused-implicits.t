use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 53;

# A routine declares fresh $_ and $¢ containers it usually never uses,
# and a block binds its topic from the enclosing one. When nothing in
# the scope, and nothing reaching lexicals by name at run time, uses
# such an implicit, its declaration is not emitted, which saves a
# container clone per call.

sub qast-var-decl (Mu $qast, Str:D $name, Str:D $decl --> Bool:D) {
    if nqp::istype($qast, QAST::Var) && $qast.name eq $name && $qast.decl eq $decl {
        return True;
    }
    if qast-descendable $qast {
        for $qast.list {
            qast-var-decl $_, $name, $decl and return True;
        }
    }
    False
}

qast-is 'sub f($a) { $a + $a }; say f(1)', :full, -> \v {
    not qast-var-decl(v, '$_', 'contvar')
}, 'a sub that never touches the topic declares no fresh $_';

# The kept-shape and cursor assertions are this frontend's: the legacy
# optimizer lowers a used topic to a local rather than keeping the
# contvar, and gates $¢ on the scope making no calls at all, which an
# operator call already defeats.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'sub f($a) { $_ = $a; $_ + 1 }; say f(1)', :full, -> \v {
        qast-var-decl(v, '$_', 'contvar')
    }, 'a sub that assigns the topic keeps its fresh $_';

    qast-is 'sub f($a) { $a ~~ Int }; say f(1)', :full, -> \v {
        qast-var-decl(v, '$_', 'contvar')
    }, 'a smartmatch reaches the topic by name, so the fresh $_ stays';

    qast-is 'sub f($a) { $a + $a }; say f(1)', :full, -> \v {
        not qast-var-decl(v, '$¢', 'contvar')
    }, 'a sub with no regex declares no fresh $¢';
}
else {
    skip 'shapes specific to the RakuAST frontend', 3;
}

# Behavior stays identical.

{
    sub topical() { $_ = 5; $_ + 1 }
    is topical(), 6, 'assigning and reading the topic in a sub behaves unchanged';
}

{
    $_ = 42;
    sub fresh() { $_ }
    ok !fresh().defined, 'a sub sees its own fresh topic, not the enclosing one';
}

{
    $_ = 1;
    given 2 { is $_, 2, 'given topicalizes' }
    is $_, 1, 'the enclosing topic survives a given';
}

{
    my @r;
    for 1..3 { @r.push($_) }
    is @r.join(','), '1,2,3', 'a for loop binds its topic per iteration';
}

{
    sub matcher($x) { $x ~~ Int ?? 'int' !! 'other' }
    is matcher(1), 'int', 'smartmatch in a sub with a kept topic works';
}

{
    my $seen;
    sub capturer() { my $c = { $seen = $_ }; $c(7) }
    capturer();
    is $seen, 7, 'a closure taking the topic as its argument still receives it';
}

# A method's implicit %_ goes the same way when unused, while the
# slurpy parameter itself stays, so stray nameds are still accepted
# and discarded.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class C { method m() { 42 } }; say C.m', :full, -> \v {
        not qast-var-decl(v, '%_', 'contvar')
    }, 'a method that never touches %_ declares no hash for it';

    qast-is 'my class C { method m() { %_.elems } }; say C.m', :full, -> \v {
        qast-var-decl(v, '%_', 'contvar')
    }, 'a method using the %_ placeholder keeps its hash';
}
else {
    skip 'the %_ shapes are specific to the RakuAST frontend', 2;
}

{
    my class C { method m() { 'ok' } }
    is C.m(:stray, :other(2)), 'ok',
        'a method with an unused %_ still accepts and discards stray nameds';
}

{
    my class C {
        has @.handles;
        method new(*@handles) { self.bless(:@handles, |%_) }
    }
    is C.new(1, 2).handles.join(','), '1,2',
        'flattening %_ into another call still works';
}

# A method binds &?ROUTINE to its own code object only when something
# uses it, which saves a getcodeobj bind per call.

sub qast-op-present (Mu $qast, Str:D $op --> Bool:D) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq $op {
        return True;
    }
    if qast-descendable $qast {
        for $qast.list {
            qast-op-present $_, $op and return True;
        }
    }
    False
}

qast-is 'my class C { method m() { 42 } }; say C.m', :full, -> \v {
    not qast-op-present(v, 'getcodeobj')
}, 'a method that never touches &?ROUTINE does not bind it';

qast-is 'my class C { method m() { &?ROUTINE.name } }; say C.m', :full, -> \v {
    qast-op-present(v, 'getcodeobj')
}, 'a method using &?ROUTINE keeps its binding';

{
    my class C { method m() { &?ROUTINE.name } }
    is C.m, 'm', '&?ROUTINE in a method names the method';
}

{
    sub named-r() { &?ROUTINE.name }
    is named-r(), 'named-r', '&?ROUTINE in a sub names the sub';
}

{
    my class C { method m() { my $c = { &?ROUTINE.name }; $c() } }
    is C.new.m, 'm', '&?ROUTINE reached from a nested block names the enclosing method';
}

# samewith is on the analyzer's poison list, so the routine binding
# stays for it.
{
    my class C { multi method m(Int $x) { $x ?? samewith(0) !! 'base' } }
    is C.new.m(5), 'base', 'samewith in a method redispatches through its kept binding';
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    {
        my class C { method m() { EVAL q[&?ROUTINE.name] } }
        is C.new.m, 'm', 'EVAL reaching &?ROUTINE by name keeps the binding';
    }
}
else {
    skip 'legacy never installs &?ROUTINE for an EVAL-only use', 1;
}

# A generated accessor's body is fully synthetic, so it declares no
# special variables at all, saving their container clones per call.

sub find-block (Mu $qast, Str:D $name) {
    if nqp::istype($qast, QAST::Block) && $qast.name eq $name {
        return $qast;
    }
    if qast-descendable $qast {
        for $qast.list {
            my $found := find-block($_, $name);
            return $found if nqp::isconcrete($found);
        }
    }
    Mu
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class C { has $.acc }; say C.new(acc => 1).acc', :full, -> \v {
        my $block = find-block(v, 'acc');
        nqp::isconcrete($block)
        and not qast-var-decl($block, '$/', 'contvar')
        and not qast-var-decl($block, '$!', 'contvar')
        and not qast-var-decl($block, '$_', 'contvar')
        and not qast-var-decl($block, '$¢', 'contvar')
    }, 'a generated accessor declares no special variable containers';
}
else {
    skip 'the accessor shape is specific to the RakuAST frontend', 1;
}

{
    my class C { has $.acc = 5 }
    is C.new.acc(:stray), 5, 'an accessor still absorbs stray named arguments';
}

{
    my class C { has int $.n is rw }
    my $o = C.new;
    $o.n = 7;
    is $o.n, 7, 'a native rw accessor still writes and reads through its reference';
}

{
    my class C { has $.s is rw }
    my $o = C.new;
    $o.s = 'set';
    is $o.s, 'set', 'a scalar rw accessor still writes and reads its container';
}

{
    my role R { has $.r = 'role-attr' }
    my class C does R { }
    is C.new.r, 'role-attr', 'an accessor composed from a role still reads its attribute';
}

# A generated accessor binds its invocant as a raw local parameter and
# discards stray nameds through a local slurpy, with no binder run.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class C { has $.acc2 }; say C.new(acc2 => 1).acc2', :full, -> \v {
        my $block = find-block(v, 'acc2');
        nqp::isconcrete($block)
        and qast-var-decl($block, 'self', 'param')
        and not qast-var-decl($block, 'self', 'var')
    }, 'a generated accessor takes its invocant as a raw parameter';
}
else {
    skip 'the accessor block shape is specific to the RakuAST frontend', 1;
}

# The package's accessors share one signature with a definite invocant.
# The legacy frontend fills its shared parameter template once, so the
# invocant name there is whichever package composed first, and only
# this frontend names the right package.

{
    my class Shared { has $.one; has $.two }
    my class SharedSub is Shared { }
    if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
        is Shared.^find_method('one').signature.raku, ':(Shared:D $:: *%_)',
            'a generated accessor signature names its own package with a definite invocant';
        is Shared.^find_method('one').cando(\(SharedSub.new)).elems, 1,
            'a subclass instance fits the shared accessor signature';
    }
    else {
        skip 'the legacy invocant name leaks from the first composed package', 2;
    }
    ok Shared.^find_method('one').signature =:= Shared.^find_method('two').signature,
        'accessors of one package share one signature';
}

{
    my class TO { has $.v = 3 }
    dies-ok { TO.v }, 'an accessor invoked on the type object still dies';
    my $m = TO.^find_method('v');
    is $m(TO.new), 3, 'an accessor found through the MOP still invokes';
    dies-ok { $m(TO.new, 42) }, 'an accessor rejects an extra positional argument';
}

{
    my class Deep { has $.d = 5 }
    my class Deeper is Deep { method d() { callsame() + 1 } }
    is Deeper.new.d, 6, 'callsame from an overriding method reaches the accessor';
}

{
    my class Wrapped { has $.w = 2 }
    my $m = Wrapped.^find_method('w');
    my $h = $m.wrap(sub ($self) { 10 + callsame() });
    is Wrapped.new.w, 12, 'a wrapped accessor runs its wrapper around the raw block';
    $m.unwrap($h);
    is Wrapped.new.w, 2, 'an unwrapped accessor reads directly again';
}

{
    my class Mixed { has $.m = 9 }
    my $obj = Mixed.new but role Extra { method extra() { 'x' } };
    is $obj.m, 9, 'an accessor still reads through a mixin instance';
}

# The shared signature and its definite invocant type survive the
# precompilation store.

{
    my $dir = $*TMPDIR.add("rakuast-accessor-sig-$*PID");
    $dir.mkdir;
    $dir.add('AccessorSigTest.rakumod').spurt(q:to/MODULE/);
        unit module AccessorSigTest;
        class Point is export { has $.px; has $.py }
        MODULE
    my $probe = 'use AccessorSigTest; my $m = Point.^find_method("px"); print $m.signature.raku ~ "|" ~ ($m.signature =:= Point.^find_method("py").signature) ~ "|" ~ Point.new(px => 3).px';
    my $expected = ':(AccessorSigTest::Point:D $:: *%_)|True|3';
    for 'compiles', 'loads from the precompilation store' -> $stage {
        my $proc = run $*EXECUTABLE, '-I', $dir.absolute, '-e', $probe, :out, :err;
        my $out = $proc.out.slurp(:close);
        $proc.err.slurp(:close);
        if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
            is $out, $expected, "the shared accessor signature $stage";
        }
        else {
            ok $out.ends-with('|True|3'), "the shared accessor signature $stage";
        }
    }
    sub nuke(IO::Path $p) {
        if $p.d { nuke($_) for $p.dir; $p.rmdir }
        else { $p.unlink }
    }
    nuke($dir);
}

# A frame that never uses $/ or $! and makes no calls drops both
# fresh containers, the rule the legacy optimizer applies. Both stay
# dynamic in every language revision, so any call keeps them: a
# callee can reach either by name.

qast-is 'use nqp; my class T { method m() { nqp::add_i(1,2) } }; T.new.m', :full, -> \v {
    qast-contains-op(v, 'add_i')
    and not qast-var-decl(v, '$/', 'contvar')
    and not qast-var-decl(v, '$!', 'contvar')
}, 'a method of raw ops declares no fresh match or error variable';

qast-is 'use nqp; my class T { method m($x) { if $x { $x.Str }; nqp::add_i(1,2) } }; T.new.m(1)', :full, -> \v {
    qast-contains-op(v, 'add_i')
    and qast-var-decl(v, '$/', 'contvar')
    and qast-var-decl(v, '$!', 'contvar')
}, 'a call inside a nested block keeps the method fresh containers';

qast-is 'use nqp; my class T { method m($v) { nqp::add_i(1,2); "x: $v" } }; T.new.m(1)', :full, -> \v {
    qast-contains-op(v, 'add_i')
    and qast-var-decl(v, '$/', 'contvar')
    and qast-var-decl(v, '$!', 'contvar')
}, 'string interpolation counts as a call, keeping both containers';

qast-is 'my class T { method m() { try die "x"; 1 } }; T.new.m', :full, -> \v {
    qast-var-decl(v, '$/', 'contvar')
    and qast-var-decl(v, '$!', 'contvar')
}, 'a method that tries keeps both fresh containers';

qast-is 'my class T { method m($x) { $x.Str; 42 } }; T.new.m(1)', :full, -> \v {
    qast-var-decl(v, '$/', 'contvar')
    and qast-var-decl(v, '$!', 'contvar')
}, 'a method that makes a call keeps both fresh containers';

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class T { has int $!i; method m() { $!i++; Nil } }; T.new.m', :full, -> \v {
        not qast-var-decl(v, '$/', 'contvar')
        and not qast-var-decl(v, '$!', 'contvar')
    }, 'a native step compiles to a raw op, so it counts as no call';

    qast-is 'my class T { method m($x) { my $y = $x; $y := 42; 1 } }; T.new.m(1)', :full, -> \v {
        qast-contains-op(v, 'bind')
        and not qast-var-decl(v, '$/', 'contvar')
        and not qast-var-decl(v, '$!', 'contvar')
    }, 'assignments and binds count as no call';
}
else {
    skip 'the call counting shapes are specific to the RakuAST frontend', 2;
}

{
    my sub write-caller-match() { use nqp; my $t := nqp::getlexcaller(Q[$/]); $t = 'clobbered'; 1 }
    "aXa" ~~ /(X)/;
    my class A { method m($x) { if $x { write-caller-match() }; 42 } }
    A.new.m(1);
    is ~$0, 'X', 'a nested callee writing its caller match variable cannot reach the mainline one';
    my sub read-caller-match() { use nqp; nqp::getlexcaller(Q[$/]) }
    my class B { method m($x) { my $r; if $x { $r = read-caller-match() }; $r } }
    nok B.new.m(1).defined, 'a nested callee reading its caller match variable sees a fresh one';
    my class M { method m($s) { $s.match(/(c)/); ~$0 } }
    is M.new.m("abc"), 'c', 'a method calling match reads the result through its own match variable';
}

{
    my class T { method m($s) { $s ~~ /b(.)/; ~$0 } }
    is T.new.m("abc"), 'c', 'a method that matches keeps its own match variable';
    my class E { method m() { try die "boom"; $!.message } }
    is E.new.m, 'boom', 'a method that tries keeps its own error variable';
    my class C { method m() { my $x = "a1" ~~ /\d/; ~$x } }
    is C.new.m, '1', 'a match in a method still binds its result';
}
