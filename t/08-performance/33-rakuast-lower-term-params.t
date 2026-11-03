use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 23;

# A sigilless term parameter lowers to a frame local: its binding
# writes a register and every use reads one, with the by-name
# lexical kept static for introspection. A use from a nested frame
# needs the lexical, as do bindable and native terms, so those keep
# it. A role method's implicit invocant is generic but never
# coercive, so no runtime probe of its parameter object is emitted.
# The shapes are this frontend's.

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

# The parameter transforms hang ops under the QAST::Var param node
# itself, which qast-descendable never enters, so probing for them
# needs a walk of every node's children.
sub qast-deep-has-op(Mu $qast, str $op --> Bool:D) {
    if nqp::istype($qast, QAST::Op) {
        return True if $qast.op eq $op;
    }
    for $qast.list {
        qast-deep-has-op($_, $op) and return True;
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

    qast-is 'use nqp; my role R { method m() { nqp::add_i(1,2) } }; my class C does R {}; say C.new.m', :full, -> \v {
        qast-deep-has-op(v, 'add_i')
        and not qast-deep-has-op(v, 'bitand_i')
    }, 'a role method probes no parameter flags for its implicit invocant';

    qast-is 'my role R2[::T] { method m(T \x) { x.defined } }; my class C2 does R2[Int] {}; C2.new.m(1)', :full, -> \v {
        qast-deep-has-op(v, 'bitand_i')
    }, 'a generic non-invocant parameter still probes its parameter flags';
}
else {
    skip 'the lowering shapes are specific to the RakuAST frontend', 8;
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
    my class S3 { method m(Str(Int) $x:) { $x } }
    is-deeply S3.^find_method('m')(42), '42', 'an explicit coercive invocant still coerces';
    my class S4 { method m(\me:) { me * 2 } }
    is S4.^find_method('m')(21), 42, 'an explicit term invocant passes its value';
}

# vim: expandtab shiftwidth=4
