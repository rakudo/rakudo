use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 30;

# A sigilless term declaration lowers to a frame local unless a nested
# frame uses it or something can reach it by name. The QAST shapes
# checked are those of the RakuAST frontend.

sub qast-has-lowered(Mu $qast, str $prefix --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'local' && $qast.name.starts-with($prefix);
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-lowered($_, $prefix) and return True;
        }
    }
    False
}

# Whether a lowered local with the given name prefix is declared by a
# bind of null, the form a body flattened into its user's frame emits.
sub qast-clears-lowered(Mu $qast, str $prefix --> Bool:D) {
    if nqp::istype($qast, QAST::Op) && $qast.op eq 'bind' {
        my Mu $target := $qast.list[0];
        my Mu $source := $qast.list[1];
        return True if nqp::istype($target, QAST::Var) && $target.scope eq 'local'
            && $target.decl && $target.name.starts-with($prefix)
            && nqp::istype($source, QAST::Op) && $source.op eq 'null';
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-clears-lowered($_, $prefix) and return True;
        }
    }
    False
}

sub qast-has-lexical-decl(Mu $qast, str $name, str $decl --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'lexical' && $qast.name eq $name
            && $qast.decl eq $decl;
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-lexical-decl($_, $name, $decl) and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'sub f() { my \x := 42; x + 1 }; f()', :full, -> \v {
        qast-has-lowered(v, '__lowered_x')
        and qast-has-lexical-decl(v, 'x', 'static')
    }, 'a bound term lowers to a frame local and keeps a static lexical';

    qast-is 'sub f() { my \x = 42; x + 1 }; f()', :full, -> \v {
        qast-has-lowered(v, '__lowered_x')
    }, 'a term initialized with = lowers to a frame local';

    qast-is 'sub f() { my Int \x = 42; x + 1 }; f()', :full, -> \v {
        qast-has-lowered(v, '__lowered_x')
    }, 'a typed term lowers to a frame local';

    qast-is 'use nqp; sub f($a) { nqp::stmts((my \x := $a + 1), nqp::if(x, x, 0)) }; f(1)', :full, -> \v {
        qast-has-lowered(v, '__lowered_x')
    }, 'a term declared inside an nqp op argument lowers to a frame local';

    qast-is 'sub f() { my $s = 0; for ^4 { my \x := $_ * 2; $s += x }; $s }; f()', :full, -> \v {
        qast-has-lowered(v, '__lowered_x')
    }, 'a term declared in a loop body that stays a frame lowers to a frame local';

    qast-is 'sub f() { my $s = 0; for ^4 -> $i { my \x := $i * 2; $s += x }; $s }; f()', :full, -> \v {
        qast-clears-lowered(v, '__lowered_x')
    }, 'a term declared in a flattened for body is cleared on every entry';

    qast-is 'sub f() { my $s = 0; my $i = 0; while $i++ < 4 { my \x := $i * 2; $s += x }; $s }; f()', :full, -> \v {
        qast-clears-lowered(v, '__lowered_x')
    }, 'a term declared in a flattened while body is cleared on every entry';

    qast-is 'sub f() { my \x := 42; x + 1 }; f()', :full, -> \v {
        not qast-clears-lowered(v, '__lowered_x')
    }, 'a term declared in a routine body is not cleared';

    qast-is 'sub f() { my \x = 42; my $c = { x + 1 }; $c() }; f()', :full, -> \v {
        qast-has-lexical-decl(v, 'x', 'var')
        and not qast-has-lowered(v, '__lowered_x')
    }, 'a term a nested frame uses keeps the lexical';

    qast-is 'use MONKEY-SEE-NO-EVAL; sub f() { my \x = 42; EVAL "x" }; f()', :full, -> \v {
        qast-has-lexical-decl(v, 'x', 'var')
        and not qast-has-lowered(v, '__lowered_x')
    }, 'a term in a frame with an EVAL keeps the lexical';

    qast-is 'sub f() { my \x = 42; MY::<x> }; f()', :full, -> \v {
        qast-has-lexical-decl(v, 'x', 'var')
        and not qast-has-lowered(v, '__lowered_x')
    }, 'a term in a frame with a pseudo-package lookup keeps the lexical';

    qast-is 'sub f() { Nil orelse my \x = 5; x }; f()', :full, -> \v {
        qast-has-lexical-decl(v, 'x', 'var')
        and not qast-has-lowered(v, '__lowered_x')
    }, 'a term that is itself a thunked operand keeps the lexical';

    qast-is 'sub f($c) { $c andthen (my \x = 3); x }; f(1)', :full, -> \v {
        qast-has-lexical-decl(v, 'x', 'var')
        and not qast-has-lowered(v, '__lowered_x')
    }, 'a term declared under a thunk of its scope keeps the lexical';

    qast-is 'sub f() { state \x = 5; x }; f()', :full, -> \v {
        not qast-has-lowered(v, '__lowered_x')
    }, 'a state term is not lowered';
}
else {
    skip 'the lowering shapes are specific to the RakuAST frontend', 14;
}

# Runtime behavior of terms, lowered or kept lexical.

{
    my sub a() { my \x := 42; x + 1 }
    is a(), 43, 'a bound term reads its value';
    my sub b() { my \x = 1; my \y = x + 1; my \z = y + x; x ~ y ~ z }
    is b(), '123', 'a term initializer reads the terms declared before it';
    my sub c() { my \x = 41; my $c = { x + 1 }; $c() }
    is c(), 42, 'a term a closure captures reads through the lexical';
    my sub d() { my @c; for ^3 { my \x := $_ * 2; @c.push: { x } }; @c.map({ $_() }).join(",") }
    is d(), '0,2,4', 'closures in a loop body each capture their own term';
    my sub e() { my $s = 0; for ^4 -> $i { my \x := $i * 2; $s += x }; $s }
    is e(), 12, 'a term in a loop body is bound anew on every iteration';
    my sub f() {
        my @r;
        for 1..3 -> $i {
            ($i == 1) && (my \x := 10);
            @r.push: nqp::isnull(x) ?? "unbound" !! x;
        }
        @r.join(",")
    }
    is f(), '10,unbound,unbound', 'a term whose bind a condition skips does not read the previous iteration';
    my sub g() { my \x = my $y = 3; x = 7; $y }
    is g(), 7, 'a term bound to a container assigns through to it';
    my sub h() { my Int \x = "a"; x }
    throws-like { h() }, X::TypeCheck::Binding, 'a typed term checks what it is bound to';
    my sub i() { my \x := { x.^name }; x.() }
    is i(), 'Block', 'a term can refer to itself from a closure in its initializer';
    my sub j() { my \x = 42; MY::<x> }
    is j(), 42, 'a MY:: lookup reads a term, not a sentinel';
    my sub k() { my \x = 42; ::("x") }
    is k(), 42, 'an indirect lookup reads a term, not a sentinel';
    my sub m() { Nil orelse my \x = 5; x }
    is m(), 5, 'a term that is itself a thunked operand binds and reads its value';
    my sub n() { my @r; (my \x = $_; @r.push: x) for 1..3; @r.join(",") }
    is n(), '1,2,3', 'a term declared under a loop modifier binds on every iteration';
    my sub o() {
        my @r;
        { my \x = 1; @r.push: x }
        { my \x = 2; @r.push: x }
        for 1..2 -> $i { my \x = $i * 10; @r.push: x }
        @r.join(",")
    }
    is o(), '1,2,10,20', 'terms of one name in sibling blocks keep their own values';
    my sub p() { my \x = 1; my $r = do { my \x = 2; x }; "$r," ~ x }
    is p(), '2,1', 'a term shadowing another of the same name leaves the outer one alone';
    use MONKEY-SEE-NO-EVAL;
    my sub l() { my \x = 42; EVAL "x + 1" }
    is l(), 43, 'an EVAL reads a term, not a sentinel';
}

# vim: expandtab shiftwidth=4
