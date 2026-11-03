use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 33;

# The implicit invocant lowers to a frame local: the invocant binding
# writes a register and every self or attribute access reads one, with
# the by-name lexical kept static for introspection. A use from a
# nested frame needs the lexical, as does an attribute reached through
# its aliasing declaration, whose access compiles to a by-name read of
# self. The shapes are this frontend's.

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

sub qast-has-op(Mu $qast, str $op --> Bool:D) {
    if nqp::istype($qast, QAST::Op) {
        return True if $qast.op eq $op;
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-op($_, $op) and return True;
        }
    }
    False
}

# A method can be compiled more than once, as a unit emission and as a
# dynamic compilation for BEGIN use, so a per-block check runs against
# every block of the given name rather than the whole unit.
sub qast-block-passes(Mu $qast, str $name, &check --> Bool:D) {
    if nqp::istype($qast, QAST::Block) && $qast.name eq $name {
        return True if check($qast);
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-block-passes($_, $name, &check) and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class T { method m() { self.defined } }; T.new.m', :full, -> \v {
        qast-has-lowered(v, '__lowered_self')
    }, 'the invocant lowers to a frame local';

    qast-is 'my class T { has $.x; method m() { $!x } }; T.new.m', :full, -> \v {
        qast-has-lowered(v, '__lowered_self')
    }, 'an attribute access reads the invocant through its local';

    qast-is 'my class T { has $.x; method m($!x) { self.defined } }; T.new.m(1)', :full, -> \v {
        qast-has-lowered(v, '__lowered_self')
    }, 'a method with an attributive parameter lowers its invocant';

    qast-is 'my class T { submethod s() { self.defined } }; T.new.s', :full, -> \v {
        qast-has-lowered(v, '__lowered_self')
    }, 'a submethod lowers its invocant';

    qast-is 'my class T { method m($self:) { $self.defined } }; T.new.m', :full, -> \v {
        qast-has-lowered(v, '__lowered_self')
    }, 'a method with an explicit invocant still lowers self';

    qast-is 'my class T { has $.x; method m() { (1, 2).map({ self.x }) } }; T.new.m', :full, -> \v {
        qast-has-lexical-decl(v, 'self')
        and not qast-has-lowered(v, '__lowered_self')
    }, 'an invocant a nested frame uses keeps the lexical';

    qast-is 'my class T { method m() { EVAL q|self| } }; T.new.m', :full, -> \v {
        qast-has-lexical-decl(v, 'self')
        and not qast-has-lowered(v, '__lowered_self')
    }, 'an EVAL in the method keeps the invocant a lexical';

    qast-is 'my class T { has $x; method m() { $x } }; T.new.m', :full, -> \v {
        qast-has-lexical-decl(v, 'self')
        and not qast-has-lowered(v, '__lowered_self')
    }, 'an attribute reached through its alias keeps the invocant a lexical';

    qast-is 'my class T { has ($a, $b); method m() { $a } }; T.new.m', :full, -> \v {
        qast-has-lexical-decl(v, 'self')
        and not qast-has-lowered(v, '__lowered_self')
    }, 'an attribute from a has list keeps the invocant a lexical';

    qast-is 'my grammar G { token TOP { \w+ } }; G.parse("a")', :full, -> \v {
        qast-has-lexical-decl(v, 'self')
        and not qast-has-lowered(v, '__lowered_self')
    }, 'a regex declaration keeps its invocant a lexical';

    qast-is 'my class T { method m() { self } }; T.new.m', :full, -> \v {
        qast-block-passes(v, 'm', -> \b { qast-has-lowered(b, '__lowered_self') })
        and not qast-block-passes(v, 'm', -> \b {
            qast-has-op(b, 'p6decontrv') or qast-has-op(b, 'p6decontrv_6c')
        })
    }, 'a method returning its lowered invocant still elides the return decont';
}
else {
    skip 'the lowering shapes are specific to the RakuAST frontend', 11;
}

# Behavior stays identical.

{
    my class C { has $.x; method m() { $!x + 1 } }
    is C.new(x => 41).m, 42, 'an attribute access in a method yields its value';

    my class D { has $.x; method m($!x) { self } }
    is D.new.m(42).x, 42, 'an attributive parameter binds into the invocant';

    my class E { has num $.re; method !SET-SELF($!re) { self }
        method make(\v) { nqp::create(self)!SET-SELF(v) } }
    is E.make(2e0).re, 2e0, 'a private method with an attributive parameter constructs';

    my class F { has $.x; method m() { (1..3).map({ self.x * $_ }).sum } }
    is F.new(x => 2).m, 12, 'a closure in a method reads the invocant';

    my class G2 { method m() { self } }
    ok G2.new.m ~~ G2, 'a method returns its invocant deconted';

    my class P1 { method m() { "P" } }
    my class C1 is P1 { method m() { "C" ~ callsame } }
    is C1.m, 'CP', 'deferral in a method still runs';

    my class H1 { has $.x; method m() { EVAL q|self.x| } }
    is H1.new(x => 7).m, 7, 'an EVAL in a method reads the invocant, not a sentinel';

    my class I1 { method m() { MY::<self>.^name } }
    is I1.new.m, 'I1', 'a MY:: lookup reads the invocant, not a sentinel';

    my class I2 { method m() { ::("self").^name } }
    is I2.new.m, 'I2', 'an indirect lookup reads the invocant, not a sentinel';

    my class J1 { has $x; method set(\v) { $x = v; self }; method m() { $x } }
    is J1.new.set(9).m, 9, 'an aliased attribute reads and writes through self';

    my class J2 { has ($a, $b); submethod BUILD(:$!a, :$!b) { }
        method m() { $a + $b } }
    is J2.new(a => 20, b => 22).m, 42, 'attributes from a has list read through self';

    my class K1 { has $.x; method m($v = self.x) { $v } }
    is K1.new(x => 7).m, 7, 'a parameter default reads the live invocant';

    my class L1 { has $.x; method m($v where { $_ < self.x }) { $v } }
    is L1.new(x => 10).m(5), 5, 'a where clause reads the live invocant';
    dies-ok { L1.new(x => 3).m(5) }, 'a where clause rejects against the live invocant';

    my role R1 { method m() { self.n * 2 } }
    my class M1 does R1 { method n() { 21 } }
    is M1.new.m, 42, 'a role method reads self through its generic invocant';

    my role R2 { has $.x; method m() { $!x + self.x } }
    my class D2 does R2 {}
    is D2.new(x => 21).m, 42, 'a role method reads an attribute through its generic invocant';

    my class B1 { has num $.re; method !SET-SELF($!re) { self }
        method make(\v) { nqp::create(self)!SET-SELF(v) } }
    my constant K = B1.make(3e0);
    is K.re, 3e0, 'a method compiled for BEGIN time constructs through self';

    my class M2 {
        has $.x;
        proto method m(|) {*}
        multi method m() { $!x }
        multi method m($v) { self.x + $v }
    }
    is M2.new(x => 40).m, 40, 'a multi candidate reads an attribute through self';
    is M2.new(x => 40).m(2), 42, 'another multi candidate reads self through a method call';

    my class P2 { has $.x; method m() { $!x } }
    my class C2 is P2 { method m() { nextsame } }
    is C2.new(x => 42).m, 42, 'nextsame reaches a candidate that reads an attribute';

    my $held = 5;
    my class S5 { method m(\selfish) { selfish } }
    is S5.m($held).VAR.^name, 'Int', 'a term parameter named like self still returns deconted';
    my sub selfsub(\selfmade) { selfmade }
    is selfsub($held).VAR.^name, 'Int', 'a sub term parameter named like self still returns deconted';
}

# vim: expandtab shiftwidth=4
