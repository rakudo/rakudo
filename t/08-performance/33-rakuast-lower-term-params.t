use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 19;

# A sigilless term parameter lowers to a frame local: its binding
# writes a register and every use reads one, with the by-name
# lexical kept static for introspection. A use from a nested frame
# needs the lexical, as do bindable and native terms, so those keep
# it. The shapes are this frontend's.

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

sub qast-has-lexical-decl(Mu $qast, str $name --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'lexical' && $qast.name eq $name && $qast.decl;
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-lexical-decl($_, $name) and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class T { method m(\t) { t.defined } }; T.new.m(1)', :full, -> \v {
        qast-has-lowered(v, '__lowered_t')
    }, 'a term parameter lowers to a frame local';

    qast-is 'my class T { method m(\t is raw) { t.defined } }; T.new.m(1)', :full, -> \v {
        qast-has-lowered(v, '__lowered_t')
    }, 'a raw term parameter lowers, since it cannot be rebound';

    qast-is 'my class T { method m(\t) { my $c = { t.defined }; $c() } }; T.new.m(1)', :full, -> \v {
        qast-has-lexical-decl(v, 't')
        and not qast-has-lowered(v, '__lowered_t')
    }, 'a term parameter a nested frame uses keeps the lexical';

    qast-is 'my class T { method m(|c) { c.elems } }; T.new.m(1)', :full, -> \v {
        qast-has-lowered(v, '__lowered_c')
    }, 'a capture parameter lowers to a frame local';

    qast-is 'my class T { method m(+vals) { vals.elems } }; T.new.m(1, 2)', :full, -> \v {
        qast-has-lowered(v, '__lowered_vals')
    }, 'a slurpy term parameter lowers to a frame local';

    qast-is 'my class T { method m(int \t) { t.defined } }; T.new.m(1)', :full, -> \v {
        qast-has-lexical-decl(v, 't')
        and not qast-has-lowered(v, '__lowered_t')
    }, 'a native term parameter keeps the lexical';
}
else {
    skip 'the lowering shapes are specific to the RakuAST frontend', 6;
}

# Behavior stays identical.

{
    my class T { method m(\a, \b) { a + b } }
    is T.new.m(40, 2), 42, 'lowered term parameters pass their values';
    my class U { method m(\t) { my $c = { t * 2 }; $c() } }
    is U.new.m(21), 42, 'a term parameter a closure captures reads through the lexical';
    my class V { method m(|c) { c.elems } }
    is V.new.m(1, 2, 3), 3, 'a capture parameter collects its arguments';
    my class W { method m(+vals) { vals.sum } }
    is W.new.m(20, 22), 42, 'a slurpy term parameter collects its arguments';
    my class X1 { method m(\t is raw) { t } }
    is X1.new.m(42), 42, 'a raw term parameter passes its value';
    my sub f(\t) { t ~ "!" }
    is f("hi"), 'hi!', 'a term parameter in a sub passes its value';
    my class Y1 { method m(Int \t) { t + 1 } }
    is Y1.new.m(41), 42, 'a typed term parameter passes its value';
    my class N1 { method m(int \t) { t + 1 } }
    is N1.new.m(41), 42, 'a native term parameter passes its value';
    my sub g() { my (\a, \b) := (40, 2); a + b }
    is g(), 42, 'a signature declaration binds its term targets';
    my class I1 { method m(\t) { ::("t") } }
    is I1.new.m(42), 42, 'an indirect lookup reads a term parameter, not a sentinel';
    my class I2 { method m(\t) { MY::<t> } }
    is I2.new.m(42), 42, 'a MY:: lookup reads a term parameter, not a sentinel';
}

{
    my role R { method double() { self.x * 2 } }
    my class C does R { method x() { 21 } }
    is C.new.double, 42, 'a role method reads self through its generic invocant';
    my role S2[::T] { method t() { T.^name } }
    my class D does S2[Int] {}
    is D.new.t, 'Int', 'a parametric role method still resolves its type parameter';
}

# vim: expandtab shiftwidth=4
