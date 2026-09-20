use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 24;

# A lowered parameter's local needs no container, and the variables of a
# declaration list keep theirs. The QAST shapes checked are those of the
# RakuAST frontend.

sub qast-has-local(Mu $qast, str $prefix, str $decl --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'local' && $qast.decl eq $decl
            && $qast.name.starts-with($prefix);
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-local($_, $prefix, $decl) and return True;
        }
    }
    False
}

sub qast-has-lexical-decl(Mu $qast, str $prefix --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'lexical' && $qast.decl eq 'var'
            && $qast.name.starts-with($prefix);
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-lexical-decl($_, $prefix) and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'sub f($a) { $a + 1 }; f(1)', :full, -> \v {
        qast-has-local(v, '$__lowered_a', 'var')
        and !qast-has-local(v, '$__lowered_a', 'contvar')
    }, 'a lowered parameter declares its local without a container';

    qast-is 'sub f($a is copy) { $a++; $a }; f(1)', :full, -> \v {
        qast-has-local(v, '$__lowered_a', 'var')
        and !qast-has-local(v, '$__lowered_a', 'contvar')
    }, 'a lowered is copy parameter declares its local without a container';

    qast-is 'sub f(@a, %h) { @a.elems + %h.elems }; f([], {})', :full, -> \v {
        qast-has-local(v, '@__lowered_a', 'var')
        and qast-has-local(v, '%__lowered_h', 'var')
        and !qast-has-local(v, '@__lowered_a', 'contvar')
        and !qast-has-local(v, '%__lowered_h', 'contvar')
    }, 'lowered array and hash parameters declare their locals without a container';

    qast-is 'sub f() { my ($a, $b) = 1, 2; $a + $b }; f()', :full, -> \v {
        qast-has-local(v, '$__lowered_a', 'contvar')
        and qast-has-local(v, '$__lowered_b', 'contvar')
    }, 'the lowered variables of a declaration list are declared with containers';

    qast-is 'sub f() { my ($a, $b); $a }; f()', :full, -> \v {
        qast-has-local(v, '$__lowered_a', 'contvar')
    }, 'a declaration list without an initializer keeps its containers';
}
else {
    skip 'QAST shapes are those of the RakuAST frontend', 5;
}

{
    sub f($a is copy) { $a++; $a }
    is f(1), 2, 'an is copy parameter can be assigned to';
}
{
    sub f($a? is copy) is raw { $a }
    my $r := f();
    $r = 9;
    ok f() === Any, 'the container of an is copy parameter is not shared between calls';
}
{
    sub mk($a is copy) { -> { $a++ } }
    my &x = mk(1);
    my &y = mk(10);
    x(); x();
    is x(), 3, 'a closure over an is copy parameter keeps its own container';
    is y(), 10, 'a second call gets an is copy container of its own';
}
{
    sub f($n is copy) { return 0 if $n == 0; my $r = f($n - 1); $n++; $n + $r }
    is f(3), 9, 'an is copy parameter in a recursive routine keeps a container per call';
}
{
    sub f($a?) { $a }
    ok f() === Any, 'an optional parameter without an argument reads as its type object';
}
{
    sub f(Int $a?) { $a }
    ok f() === Int, 'a typed optional parameter without an argument reads as its type object';
}
{
    sub f(:$n) { $n }
    ok f() === Any, 'a named parameter without an argument reads as its type object';
}
{
    sub f(@a?, %h?) { @a.elems + %h.elems }
    is f(), 0, 'optional array and hash parameters without arguments read as empty';
}
{
    sub f($a? is copy) { $a = 5 without $a; $a }
    is f(), 5, 'an optional is copy parameter without an argument can be assigned to';
}
{
    sub f(Int $a? is copy) { $a = "x" }
    dies-ok { f() }, 'a typed optional is copy parameter without an argument keeps its type constraint';
}

{
    sub f() { my ($a, $b) = 1, 2; $a + $b }
    is f(), 3, 'a declaration list in a routine assigns its variables';
}
{
    sub f() { my ($a, $b); $a //= 1; $a++; $a }
    is f(), 2, 'a declaration list without an initializer gives assignable variables';
    is f(), 2, 'a declaration list without an initializer gives fresh variables on each call';
}
{
    sub f() { my (Int $a, Str $b); $a }
    ok f() === Int, 'a typed declaration list variable without an initializer reads as its type object';
}
{
    sub f($n) { my ($a, $b) = $n, 0; -> { $a++ } }
    my &x = f(1);
    my &y = f(10);
    x(); x();
    is x(), 3, 'a closure over a declaration list variable keeps its own container';
    is y(), 10, 'a second call gets a declaration list container of its own';
}
{
    sub f($n) { my ($a, $b) = $n, $n * 2; my $r = $n > 0 ?? f($n - 1) !! 0; $a + $b + $r }
    is f(3), 18, 'a declaration list in a recursive routine keeps a container per call';
}
{
    my @c;
    for 1..3 { my ($a, $b) = $_, 0; @c.push: -> { $a } }
    is @c.map({ $_() }).join(' '), '1 2 3', 'a declaration list in a loop body gets fresh containers each iteration';
}

# vim: expandtab shiftwidth=4
