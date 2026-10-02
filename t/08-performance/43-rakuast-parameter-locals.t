use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 48;

# A lowered parameter's local needs no container, the variables of a
# declaration list keep theirs, and a WhateverCode's parameter lowers
# too. The QAST shapes checked are those of the RakuAST frontend.

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

    qast-is 'my &f = * + 1; f(1)', :full, -> \v {
        qast-has-local(v, '__lowered__whatever_arg', 'var')
        and !qast-has-lexical-decl(v, '_whatever_arg')
    }, 'a WhateverCode parameter lowers to a frame local';

    qast-is 'my &f = * + *; f(1, 2)', :full, -> \v {
        qast-has-local(v, '__lowered__whatever_arg_1', 'var')
        and qast-has-local(v, '__lowered__whatever_arg_2', 'var')
    }, 'each parameter of a WhateverCode lowers to a frame local';

    qast-is 'my &f = * ~~ /\d/; f("a1")', :full, -> \v {
        !qast-has-local(v, '__lowered__whatever_arg', 'var')
        and qast-has-lexical-decl(v, '_whatever_arg')
    }, 'a WhateverCode parameter stays a lexical where a regex can reach it by name';

    qast-is 'use MONKEY-SEE-NO-EVAL; my &f = * + EVAL("1"); f(1)', :full, -> \v {
        !qast-has-local(v, '__lowered__whatever_arg', 'var')
    }, 'a WhateverCode parameter stays a lexical where an EVAL can reach it by name';

    qast-is 'sub f($x where * > 2) { $x }; f(3)', :full, -> \v {
        qast-has-local(v, '__lowered__whatever_arg', 'var')
    }, 'the parameter of a WhateverCode in a where clause lowers to a frame local';

    qast-is 'my &f = 3 < * < 5; f(4)', :full, -> \v {
        qast-has-local(v, '__lowered__whatever_arg', 'var')
    }, 'the parameter of a chained comparison WhateverCode lowers to a frame local';
}
else {
    skip 'QAST shapes are those of the RakuAST frontend', 11;
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

is (1..10).grep(* %% 3).map(* + 1).sum, 21, 'WhateverCodes work in a grep and map pipeline';
is (* + *)(1, 2), 3, 'a WhateverCode with two parameters works';
is (* ~~ /\d/)("a1").Str, '1', 'a WhateverCode that smartmatches against a regex works';
is ((* + 1) o (* * 2))(3), 7, 'composed WhateverCodes work';
is (* - *)(5, 2), 3, 'the parameters of a WhateverCode keep their order';
is (3 < * < 5)(4), True, 'a chained comparison WhateverCode accepts a value in range';
is (3 < * < 5)(7), False, 'a chained comparison WhateverCode rejects a value out of range';
is (*.substr(1, *-1))("abcd"), 'bc', 'a WhateverCode in the arguments of a WhateverCode method call keeps its own parameter';
is (*.map(* + 1))((1, 2)).join(' '), '2 3', 'a WhateverCode nested in another evaluates with its own argument';
is (** + 1)(1, 2, 3).join(' '), '2 3 4', 'a HyperWhatever code works';
{
    my $v = 1;
    (*++)($v);
    is $v, 2, 'a WhateverCode parameter stays bound to the caller container';
}
{
    sub f($x where * > 2) { $x }
    is f(3), 3, 'a WhateverCode where clause accepts a matching value';
    dies-ok { f(1) }, 'a WhateverCode where clause rejects a value that does not match';
}
{
    sub mk($n) { * + $n }
    my &a = mk(1);
    my &b = mk(100);
    is a(1), 2, 'a WhateverCode called after its defining frame exits sees its closed over value';
    is b(1), 101, 'a second WhateverCode from the same routine keeps its own closed over value';
}
{
    sub r($n) { $n == 0 ?? 0 !! (* + r($n - 1))($n) }
    is r(10), 55, 'a WhateverCode whose body calls back into the enclosing routine keeps its argument';
}
{
    my &f = * + *;
    my @p = (1..20).map: -> $i { start { (^500).map({ f($i, $_) }).sum } };
    is await(@p).sum, 2600000, 'one WhateverCode called from several threads keeps the arguments of each call';
}
{
    constant &inc = * + 1;
    is inc(1), 2, 'a WhateverCode in a constant initializer works';
}

# vim: expandtab shiftwidth=4
