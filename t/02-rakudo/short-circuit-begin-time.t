use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use nqp;
use MONKEY-SEE-NO-EVAL;

plan 63;

# A code object that a short-circuit operator, a ternary, or a meta form of an
# operator gives a trait or role argument must survive precompilation, as a
# WhateverCode argument on its own does.

for (
    'DefinedOr',     'Any // *.succ',
      'a WhateverCode right of //',
    'DefinedOrLeft', '*.succ // 0',
      'a WhateverCode left of //',
    'Or',            '0 || *.succ',
      'a WhateverCode right of ||',
    'And',           '1 && *.succ',
      'a WhateverCode right of &&',
    'LooseAnd',      '1 and *.succ',
      'a WhateverCode right of and',
    'LooseOr',       '0 or *.succ',
      'a WhateverCode right of or',
    'Xor',           '0 ^^ *.succ',
      'a WhateverCode right of ^^',
    'XorOfThree',    '0 ^^ 0 ^^ *.succ',
      'a WhateverCode last of three ^^ operands',
    'LooseXor',      '0 xor *.succ',
      'a WhateverCode right of xor',
    'ReverseOr',     '0 R|| *.succ',
      'a WhateverCode right of R||',
    'BracketedOr',   'Any [//] *.succ',
      'a WhateverCode right of [//]',
    'SequencedXor',  '0 S^^ *.succ',
      'a WhateverCode right of S^^',
    'Orelse',        '*.succ orelse 0',
      'a WhateverCode left of orelse',
    'EnvDefinedOr',  '%*ENV<RAKUDO_TEST_UNSET_VARIABLE> // *.succ',
      'a WhateverCode right of // after an environment lookup',
    'CompareOr',     '1 == 2 or *.succ',
      'a WhateverCode right of or after a comparison',
    'CompareXor',    '1 == 2 ^^ *.succ',
      'a WhateverCode right of ^^ after a comparison',
    'Ternary',       'True ?? *.succ !! 0',
      'a WhateverCode in the branch a ternary picks',
    'EnvTernary',    '%*ENV<RAKUDO_TEST_UNSET_VARIABLE> ?? 0 !! *.succ',
      'a WhateverCode in the branch a ternary picks after an environment lookup',
    'PointyOr',      '0 || -> $x { $x + 1 }',
      'a pointy block right of ||',
    'EnvPointyOr',   '%*ENV<RAKUDO_TEST_UNSET_VARIABLE> || -> $x { $x + 1 }',
      'a pointy block right of || after an environment lookup',
    'NestedEnvOr',   '0 || (%*ENV<RAKUDO_TEST_UNSET_VARIABLE> // *.succ)',
      'a WhateverCode right of // after an environment lookup, right of ||',
) -> $name, $expression, $what {
    todo 'the legacy frontend cannot run S^^ on a WhateverCode', 1
        if $name eq 'SequencedXor'
        && nqp::gethllsym('Raku', 'COMPILER-FRONTEND') ne 'rakuast';
    is-run-precompiled $name,
        "class Foo is export \{ has \&.f is default($expression) \}\n",
        'Foo.new.f.(1)', '2',
        "$what in an attribute default";
}

is-run-precompiled 'BracketedCompose',
    "class Foo is export \{ has \&.f is default(-> \$x \{ \$x * 10 \} [∘] *.succ) \}\n",
    'Foo.new.f.(1)', '20',
    'a WhateverCode right of a bracketed ∘ in an attribute default';

for (
    'RoleDefinedOr',    '*.succ // 0',
      'a WhateverCode left of // in a role argument',
    'RoleEnvDefinedOr', '%*ENV<RAKUDO_TEST_UNSET_VARIABLE> // *.succ',
      'a WhateverCode right of // after an environment lookup in a role argument',
    'RoleOr',           '0 || *.succ',
      'a WhateverCode right of || in a role argument',
    'RoleAnd',          '1 && *.succ',
      'a WhateverCode right of && in a role argument',
    'RoleTernary',      'True ?? *.succ !! 0',
      'a WhateverCode in the branch a ternary picks in a role argument',
) -> $name, $expression, $desc {
    is-run-precompiled $name,
        "role R[\&c] is export \{ method go(\$x) \{ c(\$x) \} \}\n"
          ~ "class B does R[$expression] is export \{ \}\n",
        'B.new.go(1)', '2',
        $desc;
}

# An evaluation at BEGIN time takes the operands in turn and in the order the
# compiled code does, and calls an operator declared in scope where the
# compiled code calls it.
{
    constant C = 1 || die "evaluated";
    is C, 1, 'a constant || leaves its right side unevaluated after a true left side';
}
{
    constant D = 1 ^^ 2 ^^ die "evaluated";
    is-deeply D, Nil, 'a constant ^^ stops at a second true operand';
}
{
    constant E = 0 ^^ 0 ^^ 3;
    is E, 3, 'a constant ^^ gives its one true operand';
}
{
    my class F { has $.x is default(die("evaluated") R|| 1) }
    is F.new.x, 1, 'a trait argument R|| evaluates its right side first';
}
{
    constant G = 2 R- 1;
    is G, -1, 'a constant R- applies - to its operands in the other order';
}
{
    constant H = 0 // 5;
    is H, 0, 'a constant // keeps a defined left side that is false';
}
{
    constant Z = 0 ^^ "" ^^ 0e0;
    is-deeply Z, 0e0, 'a constant ^^ of three false operands gives the last';
}
{
    constant J = True ?? 1 !! die("evaluated");
    is J, 1, 'a constant ternary leaves the branch it does not choose unevaluated';
}
{
    sub infix:<||>($, $) { 'custom' }
    constant I = 2 R|| 1;
    is I, 'custom', 'a constant R|| calls a || declared in scope';
}
{
    constant K = Int.HOW.name(Int) // 'none';
    is K, 'Int', 'a constant // tests a string a metamodel call gives as a Raku string';
}
{
    constant L = 0 R^^ "" R^^ 0e0;
    is-deeply L, (0 R^^ "" R^^ 0e0), 'a constant R^^ of three false operands gives what it gives at run time';
}
{
    our @order;
    sub record($x) { @order.push: $x; $x }
    constant X = record("a") Rmin record("b") Rmin record("c");
    is @order.join, 'abc', 'a constant reversed list infix evaluates its operands in order';
}

# An application with an adverb is compiled, as the interpreter would drop
# the adverb.
is (try EVAL ｢
    sub infix:<pw>($a, $b, :$mod = 0) { $a ** $b + $mod }
    constant M = 2 pw 3 :mod(1);
    M
｣), 9, 'a constant infix with an adverb passes the adverb';
is (try EVAL ｢
    sub infix:<cat>(*@a, :$sep = '') is assoc<list> { @a.join($sep) }
    constant N = 1 cat 2 cat 3 :sep<->;
    N
｣), '1-2-3', 'a constant list infix passes its adverb';
{
    sub infix:<pw>($a, $b, :$mod = 0) { $a ** $b + $mod }
    constant M = 2 [pw] 3 :mod(1);
    is M, (2 [pw] 3 :mod(1)), 'a constant bracketed infix with an adverb gives what it gives at run time';
}
todo 'the legacy frontend cannot chain a bracketed list infix', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (try EVAL ｢
    sub infix:<cat>(*@a, :$sep = '') is assoc<list> { @a.join($sep) }
    constant N = 1 [cat] 2 [cat] 3 :sep<->;
    N
｣), '1-2-3', 'a constant bracketed list infix passes its adverb';
{
    sub infix:<pw>($a, $b, :$mod = 0) { $a ** $b + $mod }
    my class A { has $.x is default(2 Rpw 3 :mod(1)) }
    todo 'the legacy frontend drops an adverb on a reversed infix', 1
        unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
    is A.new.x, 10, 'a reversed infix with an adverb in a trait argument passes the adverb';
}

# Only the short-circuit operators take the operands in turn. Another operator
# with a thunked operand gets the value it produces.
{
    my Buf constant b .= new(0 xx 4);
    is-deeply b, Buf.new(0, 0, 0, 0), 'a typed .= constant repeats a value with xx';
}
{
    sub infix:<≈>($a, $b) is pure is tighter(&infix:<&&>) { abs($a - $b) < 0.1 }
    is-deeply (1 ≈ 5), False, 'a pure operator declared tighter than && is called on its operands';
}

# An application whose operands the interpreter cannot all run compiles
# whole, so its operands share one frame, as they do in compiled code.
{
    my class A { has $.a is default("ab" ~~ /b/ ?? ~$/ !! "none") }
    is A.new.a, 'b', 'a trait argument ternary reads the $/ its condition sets';
}
{
    my class A { has $.a is default((try die "boom") // $!.message) }
    is A.new.a, 'boom', 'a trait argument // reads the $! its left side sets';
}
{
    my class A { has $.a is default(("ab" ~~ /(b)/) andthen ~$0) }
    is A.new.a, 'b', 'a trait argument andthen reads the capture its left side sets';
}
{
    my role R[$x] { method x { $x } }
    my class G does R[("ab" ~~ /b/) ?? ~$/ !! "none"] { }
    is G.new.x, 'b', 'a role argument ternary reads the $/ its condition sets';
}
is (try EVAL ｢
    sub versioned() {
        EVAL q[my class B:ver(%*ENV<RAKUDO_TEST_UNSET_VARIABLE> // "1.0") { }; B.^ver]
    }
    my class A { has $.x is default(versioned()) }
    ~A.new.x
｣), '1.0', 'a trait argument calls a sub that compiles code of its own';
{
    my class A { has $.a is default({ 42 } R|| 0) }
    isa-ok A.new.a, Block, 'a trait argument gives the block literal R|| returns';
}
is (try EVAL ｢
    my class A { has $.a is default("ab" andthen m/b/) }
    ~A.new.a
｣), 'b', 'a trait argument matches a regex literal right of andthen against the left side';
is (try EVAL ｢
    my class A { has $.a is default("ab" andthen S/a/x/) }
    A.new.a
｣), 'xb', 'a trait argument applies a substitution right of andthen to the left side';
todo 'the legacy frontend cannot run S||', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (try EVAL ｢
    my class A { has &.f is default(0 S|| -> $x { $x + 1 }) }
    A.new.f.(1)
｣), 2, 'a trait argument gives the pointy block S|| returns';
todo 'the legacy frontend cannot bind a native variable in a trait argument', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (try EVAL ｢
    my class A { has $.a is default((my int $r = 5) ?? $r !! -1) }
    A.new.a
｣), 5, 'a trait argument reads a native variable one of its operands declares';
{
    my class A { has &.f is default(0 || ** + 1) }
    is-deeply A.new.f.((1, 2)).List, (2, 3), 'a trait argument gives a HyperWhatever right of ||';
}
{
    my class A { has &.f is default(True ?? ** * 2 !! 0) }
    is-deeply A.new.f.((1, 2)).List, (2, 4), 'a trait argument gives a HyperWhatever in the branch a ternary picks';
}
throws-like ｢role R[::T] { has $.a is default(T // 5) }; class C does R[Int] { }; C.new.a｣,
    Exception, message => /defined/,
    'a trait argument // calls .defined on its left side';

todo 'the legacy frontend reports an error in a trait argument without a location', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like ｢class A { has $.x is default(0 || die("boom")) }｣, X::Comp,
    message => /boom/, :line(1),
    'an error in a short-circuit trait argument is reported at the declaration the trait is on';
todo 'the legacy frontend reports an error in a trait argument without a location', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
throws-like "class A \{\n    has \$.x\n        is default(1 + die('boom'));\n\}", X::Comp,
    message => /boom/, :line(2),
    'an error in a trait argument over several lines is reported at the declaration the trait is on';
{
    try EVAL q:to/CODE/;
    role R[$x] { }
    class C {
        also does R[Int.new("x")];
    }
    CODE
    unlike (quietly $!.gist), /^^ \h* 'at line' \h* $$/,
        'every error about a role argument that fails is reported with a location';
}
throws-like ｢
    use MONKEY-SEE-NO-EVAL;
    class A {
        has $.x is default(0 || EVAL(q[
            my class B is repr(%*ENV<RAKUDO_TEST_UNSET_VARIABLE> || "P6opaque") { }
            B.REPR
        ]))
    }
｣, Exception, message => /'known at compile time'/,
    'code a trait argument compiles for itself checks a trait argument of its own';

# vim: expandtab shiftwidth=4
