use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 21;

# An attribute read reports its declared type, and self reports its
# enclosing package's type, so the optimize pass can decide dispatches
# over them. The QAST shapes the assertions pin down are this
# frontend's. An assignment to an attribute keeps its run time check
# whatever the declared type says, and an argument that can never bind
# is rejected at compile time, as the legacy frontend rejects it.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'class N { has int $!i; method m() { $!i + 1 } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-call(v, '&infix:<+>')
    }, 'arithmetic on a native int attribute inlines to the raw op';

    qast-is 'class F { has num $!n; method m() { $!n + 1e0 } }', :full, -> \v {
        qast-contains-op(v, 'add_n')
        and not qast-contains-call(v, '&infix:<+>')
    }, 'arithmetic on a native num attribute inlines to the raw op';

    qast-is 'role R[::T] { has T $!r; method m() { $!r + 1 } }', :full, -> \v {
        not qast-contains-op(v, 'add_i')
    }, 'arithmetic on a generic role attribute keeps the operator call';

    qast-is 'class B { has Int $!x; method m() { $!x + 1 } }', :full, -> \v {
        qast-contains-call(v, '&infix:<+>')
    }, 'arithmetic on a boxed attribute keeps the operator call';

    qast-is 'class D { has Int:D $!x = 1; method m() { $!x + 1 } }', :full, -> \v {
        qast-contains-call(v, '&infix:<+>')
        and not qast-contains-op(v, 'add_i')
    }, 'arithmetic on a definite typed attribute keeps the operator call';

    qast-is 'my subset Pos of Int where * > 0; class P { has Pos $!p = 1; method m() { $!p + 1 } }', :full, -> \v {
        qast-contains-call(v, '&infix:<+>')
        and not qast-contains-op(v, 'add_i')
    }, 'arithmetic on a subset typed attribute keeps the operator call';

    qast-is 'class K { has Int() $!c; method m() { $!c + 1 } }', :full, -> \v {
        qast-contains-call(v, '&infix:<+>')
        and not qast-contains-op(v, 'add_i')
    }, 'arithmetic on a coercion typed attribute keeps the operator call';
}
else {
    skip 'the typed shapes are specific to the RakuAST frontend', 7;
}

# Behavior stays identical.

{
    my class N { has int $.i; method step() { $!i = $!i + 3 } }
    my $n = N.new;
    $n.step;
    $n.step;
    is $n.i, 6, 'inlined native attribute arithmetic accumulates correctly';
}

{
    my class M { has int $!n; method bump() { $!n = $!n * 2 - $!n + 1 }; method v() { $!n } }
    my $m = M.new;
    $m.bump for ^3;
    is $m.v, 3, 'mixed inlined native attribute operations compute correctly';
}

{
    my role G[::T] { has T $.g; method get() { $!g + 1 } }
    my class GC does G[Int] { }
    is GC.new(g => 7).get, 8, 'a parameterized role attribute computes through the kept call';
}

{
    my class S {
        multi method pick(S $s) { 'self' }
        multi method pick(Str $s) { 'str' }
        method go() { self.pick(self) }
    }
    is S.new.go, 'self', 'a typed self picks the invocant candidate';
    my class Sub is S { }
    is Sub.new.go, 'self', 'a subclass invocant still picks the invocant candidate';
}

{
    my class T { method probe() { self ~~ T ?? 'yes' !! 'no' } }
    is T.new.probe, 'yes', 'a typed self still smartmatches its own class';
}

{
    my class D { has Int:D $.d = 3; method m() { $!d * 2 } }
    is D.new.m, 6, 'a definite typed attribute computes through the kept call';
}

{
    my subset Pos of Int where * > 0;
    my class P { has Pos $.p = 5; method m() { $!p + 1 } }
    is P.new.m, 6, 'a subset typed attribute computes through the kept call';
}

{
    my class K { has Int() $.c; method m() { $!c + 1 } }
    is K.new(c => '4').m, 5, 'a coercion typed attribute computes through the kept call';
}

# The declared type informs the optimizer, not the compile time
# legality of an assignment: a literal that cannot fit stays a run
# time failure, as it does under the legacy frontend.

{
    my $code = q:to/CODE/;
    my class LT { has Int $!x; method m() { $!x = 1.5 } }
    LT.new
    CODE
    my $lt;
    lives-ok { $lt = EVAL $code },
        'assigning a fractional literal to an Int attribute still compiles';
    dies-ok { $lt.m },
        'assigning a fractional literal to an Int attribute fails at run time';
}

dies-ok { EVAL q[sub g-str(Str $s) {}; class CS { method go() { g-str(self) } }] },
    'a self argument that can never bind is rejected at compile time';

dies-ok { EVAL q[sub h-str(Str $s) {}; class CA { has Int $!x; method go() { h-str($!x) } }] },
    'an attribute argument that can never bind is rejected at compile time';

{
    my class BG {
        my $m = BEGIN anon method () { $!x + 1 };
        has Int $.x = 4;
        method call-it() { $m(self) }
    }
    is BG.new.call-it, 5,
        'an attribute reached from BEGIN compiled code in the still open class computes';
}

# vim: expandtab shiftwidth=4
