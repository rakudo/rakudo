use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 31;

# A WhateverCode that forms at BEGIN time as a type argument, a subset
# where clause or a trait argument, and a block written there, get
# static calls and lowered lexicals. The shapes are the RakuAST frontend's.

sub qast-has-local(Mu $qast, str $prefix --> Bool:D) {
    if nqp::istype($qast, QAST::Var) {
        return True if $qast.scope eq 'local' && $qast.decl
            && $qast.name.starts-with($prefix);
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-local($_, $prefix) and return True;
        }
    }
    False
}

sub qast-has-static-call(Mu $qast, str $name --> Bool:D) {
    if nqp::istype($qast, QAST::Op) {
        return True if $qast.op eq 'callstatic' && $qast.name eq $name;
    }
    if qast-descendable($qast) {
        for $qast.list {
            qast-has-static-call($_, $name) and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my role R1[&f] { method m($x) { f($x) } }; my class C1 does R1[* + 1] { }; C1.m(1)', :full, -> \v {
        qast-has-static-call(v, '&infix:<+>')
    }, 'a WhateverCode argument of a role a class does calls its operator statically';

    qast-is 'my role R2[&f] { method m($x) { f($x) } }; my class C2 does R2[* + 1] { }; C2.m(1)', :full, -> \v {
        qast-has-local(v, '__lowered__whatever_arg')
    }, 'the parameter of a WhateverCode argument of a role a class does lowers to a frame local';

    qast-is 'my role R3[&f] { method m($x) { f($x) } }; my class C3 does R3[* == Less] { }; C3.m(1)', :full, -> \v {
        !qast-contains-op(v, 'getlex_no')
            && qast-has-static-call(v, '&infix:<==>')
    }, 'a setting constant in a WhateverCode argument of a role a class does is a compile time value';

    qast-is 'my subset S1 of Int where * > 0; my S1 $x = 1', :full, -> \v {
        qast-has-static-call(v, '&infix:«>»')
    }, 'a WhateverCode in a subset where clause calls its operator statically';

    qast-is 'my subset S2 of Int where * > 0; my S2 $x = 1', :full, -> \v {
        qast-has-local(v, '__lowered__whatever_arg')
    }, 'the parameter of a WhateverCode in a subset where clause lowers to a frame local';

    qast-is 'my $x is default(* + 1); $x.(1)', :full, -> \v {
        qast-has-static-call(v, '&infix:<+>')
    }, 'a WhateverCode argument of a trait calls its operator statically';

    qast-is 'my $y is default(* + 1); $y.(1)', :full, -> \v {
        qast-has-local(v, '__lowered__whatever_arg')
    }, 'the parameter of a WhateverCode argument of a trait lowers to a frame local';

    qast-is 'my role R4[&f] { method m($x) { f($x) } }; my class C4 does R4[-> $a { $a + 1 }] { }; C4.m(1)', :full, -> \v {
        qast-has-local(v, '$__lowered_a')
    }, 'the parameter of a pointy block argument of a role a class does lowers to a frame local';

    qast-is 'my $z is default(-> $a { $a + 1 }); $z.(1)', :full, -> \v {
        qast-has-local(v, '$__lowered_a')
    }, 'the parameter of a pointy block argument of a trait lowers to a frame local';

    qast-is 'my role R5[&f] { method m($x) { f($x) } }; my class C5 does R5[sub ($a) { $a + 1 }] { }; C5.m(1)', :full, -> \v {
        qast-has-local(v, '$__lowered_a')
    }, 'the parameter of a sub argument of a role a class does lowers to a frame local';

    qast-is 'my role R6[&f] { method m($x) { f($x) } }; my class C6 does R6[-> $a { my $b = $a; $b + 1 }] { }; C6.m(1)', :full, -> \v {
        qast-has-local(v, '$__lowered_b')
    }, 'a lexical declared in a pointy block argument of a role a class does lowers to a frame local';

    qast-is 'my constant &k = -> $a { $a + 1 }; k(1)', :full, -> \v {
        qast-has-local(v, '$__lowered_a')
    }, 'the parameter of a pointy block in a constant initializer lowers to a frame local';

    qast-is 'multi trait_mod:<is>(Variable $v, :$grab!) { }; my $x is grab(-> $a { $a + 1 })', :full, -> \v {
        qast-has-local(v, '$__lowered_a')
    }, 'the parameter of a pointy block argument of a user trait lowers to a frame local';

    qast-is 'my role R7[&f] { method m($x) { f($x) } }; my class C7 does R7[-> $a { -> { $a } }] { }; C7.m(1)', :full, -> \v {
        !qast-has-local(v, '$__lowered_a')
    }, 'a parameter captured by a block nested in a pointy block argument stays a lexical';
}
else {
    skip 'QAST shapes are those of the RakuAST frontend', 14;
}

{
    my role R[&f] { method m($x) { f($x) } }
    my class C does R[* + 1] { }
    is C.m(41), 42, 'a class doing a role with a WhateverCode argument calls it';
}
{
    my $outer = 10;
    my role R[&f] { method m($x) { f($x) } }
    my class C does R[* + $outer] { }
    $outer = 20;
    is C.m(1), 21, 'a WhateverCode argument of a role a class does reads an outer lexical when called';
}
{
    my subset S of Int where * %% 2;
    ok 4 ~~ S && !(3 ~~ S), 'a subset with a WhateverCode where clause checks its values';
}
{
    my $x is default(* + 1);
    is $x.(1), 2, 'a variable whose default is a WhateverCode calls it';
}
{
    my role R[&f] { method m($x) { f($x) } }
    my $outer = 10;
    my class C does R[-> $a { $a + $outer }] { }
    $outer = 20;
    is C.m(1), 21, 'a pointy block argument of a role a class does reads an outer lexical when called';
}
{
    my $z is default(-> $a { $a + 1 });
    is $z.(1), 2, 'a variable whose default is a pointy block calls it';
}
{
    my role R[&f] { method m($x) { f($x) } }
    my class C does R[-> $a { -> { $a } }] { }
    is C.m(7)(), 7, 'a block nested in a pointy block argument of a role a class does reads the parameter when called later';
}
{
    my role R[&f] { method m($x) { f($x) } }
    my class C does R[-> $a { my $b = $a; -> { $b } }] { }
    is C.m(5)(), 5, 'a block nested in a pointy block argument reads a lexical the block declared';
}
my $taken-by-call;
{
    my role R[&f] { method m($x) { f($x) } }
    sub grab(&f) { $taken-by-call = f(5); &f }
    my class C does R[grab(-> $a { -> { $a } })] { }
    is $taken-by-call(), 5, 'a closure taken at BEGIN time from a block in a type argument expression reads its parameter';
    is C.m(9)(), 9, 'the block a BEGIN-time call formed still reads its parameter when called at run time';
}
my $taken-by-trait;
{
    multi trait_mod:<is>(Variable $v, :$grab!) { $taken-by-trait = $grab(6) }
    my $x is grab(-> $a { -> { $a } });
    is $taken-by-trait(), 6, 'a closure taken at BEGIN time by a trait that calls its block argument reads its parameter';
}
{
    my role R[&f] { method m($x) { f($x) } }
    my class C does R[* + 1] { }
    multi sub infix:<+>(Int:D $a where { True }, Int:D $b) { 999 }
    is C.m(1), 999, 'a WhateverCode argument of a role a class does calls an operator declared after the class';
}
{
    my role R[&f] { method m($x) { f($x) } }
    my class C does R[* + 1] { }
    my &infix:<+> = -> $a, $b { 7 };
    is C.m(1), 7, 'a WhateverCode argument of a role a class does calls an operator variable declared after the class';
}
{
    my subset S of Int where * > 0;
    sub f(S $x) { $x }
    is f(1), 1, 'a subset with a WhateverCode where clause accepts a matching parameter';
    dies-ok { f(-1) }, 'a subset with a WhateverCode where clause rejects a parameter that does not match';
}
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    my role R[&f] { method m($x) { f($x) } }
    sub mk() { my $n = 5; my class C does R[* + $n] { }; $n = 7; C }
    is mk().m(1), 8, 'a WhateverCode argument of a role a class does inside a routine reads the routine lexical when called';

    my @r;
    for ^2 { my subset S of Int where * > (FIRST my $x = 5); @r.push(6 ~~ S) }
    is-deeply @r, [True, True], 'a subset where clause with a FIRST phaser inside its WhateverCode compiles and checks its values';
}
else {
    skip 'a class composed inside a routine and a FIRST phaser in a where clause behave differently under the legacy frontend', 2;
}
