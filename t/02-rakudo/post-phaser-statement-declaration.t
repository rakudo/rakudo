use Test;
use MONKEY-SEE-NO-EVAL;
use nqp;

plan 31;

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 { POST my $a = 5; @seen.push: $a }
    @seen
    CODE
    [Any, Any],
    'a scalar declared by POST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 { POST my $a is default(3) = 5; @seen.push: $a }
    @seen
    CODE
    [3, 3],
    'a traited variable declared by POST is visible in the loop body';

is (try EVAL q:to/CODE/),
    sub f() { POST $_ == 5; 5 }
    f()
    CODE
    5,
    'a POST statement sees the return value as its topic';

throws-like q:to/CODE/, X::Phaser::PrePost,
    sub f() { POST $_ == 5; 6 }
    f()
    CODE
    'a failing POST statement throws';

is (try EVAL q:to/CODE/),
    sub f($x) { POST { my $y = $_; $y == 5 }; $x }
    f(5)
    CODE
    5,
    'a POST block keeps its own declarations';

is (try EVAL q:to/CODE/),
    sub f() { my $y = 1; POST { my $y = 5; True }; -> { $y } }
    f()()
    CODE
    1,
    'a POST block declaration does not reach the enclosing block';

is (try EVAL q:to/CODE/),
    my $seen;
    sub f() { POST my $a = $_; $seen = { $a }; 7 }
    f();
    $seen()
    CODE
    7,
    'a closure in the routine sees the value POST assigns';

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is (try EVAL Q:to/CODE/.AST.raku.EVAL),
        my $seen;
        sub f() { POST my $a = $_; $seen = { $a }; 7 }
        f();
        $seen()
        CODE
        7,
        'a POST statement built as an AST gives its declarations to the routine';
}
else {
    skip 'the legacy frontend rejects the RakuAST source of a round trip';
}

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 { POST sub foo() { 42 }; @seen.push: foo() }
    @seen
    CODE
    [42, 42],
    'a sub declared by POST is callable in the loop body';

is (try EVAL q:to/CODE/),
    POST my $a = 5;
    $a // 'no value yet'
    CODE
    'no value yet',
    'a variable declared by POST at unit scope is visible in the unit';

is (try EVAL q:to/CODE/),
    sub f() { POST (my $/ = 5) > 0; 1 }
    f()
    CODE
    1,
    'a POST statement may declare the $/ of its routine';

is (try EVAL q:to/CODE/),
    sub f() { POST my $a = $_; my $a = 3; 5 }
    f()
    CODE
    5,
    'a POST declaration redeclared in its routine compiles';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    sub f() { POST my ($a, $b) = $_, 2; @seen.push: $b; 5 }
    f();
    @seen
    CODE
    [Any],
    'a list declared by POST is visible in the routine';

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    throws-like q:to/CODE/, X::Comp::AdHoc, message => / 'in a POST statement' /,
        sub f() { POST my ($a, $b) := $_, 2; 5 }
        CODE
        'a list bound by POST is a compile-time error';
}
else {
    skip 'the legacy frontend reports no error for a list bound in a POST statement';
}

throws-like q:to/CODE/, X::Phaser::PrePost,
    sub f() { POST 1 if False; 5 }
    f()
    CODE
    'a POST statement keeps its condition modifier';

is (try EVAL q:to/CODE/),
    my $seen;
    sub f() { POST 1 if my $y = $_; $seen = { $y }; 7 }
    f();
    $seen()
    CODE
    7,
    'a variable declared by a POST modifier is visible in the routine';


is (try EVAL q:to/CODE/),
    sub f() { my $a = 1; POST my $a = 5; $a }
    f()
    CODE
    1,
    'a routine variable redeclared by POST keeps the value the routine gives it';

is (try EVAL q:to/CODE/),
    sub f() { my $a = 3; POST my $a = 1; $a }
    f()
    CODE
    3,
    'a POST declaration redeclared earlier in the routine keeps the value the routine gives it';

throws-like q:to/CODE/, X::Redeclaration::Outer,
    my $a = 'outer';
    sub f() { my $b = $a; POST my $a = 1; $b }
    CODE
    'a POST declaration of a name already used from outside is an error';

throws-like q:to/CODE/, X::Redeclaration::Outer,
    my $a = 'outer';
    sub f() { POST (say $a) && (my $a = 5); 1 }
    CODE
    'a POST condition using a name from outside before declaring it is an error';

throws-like q:to/CODE/, X::Redeclaration::Outer,
    my $a = 'outer';
    sub f() { POST say($a); POST my $a = 1; 5 }
    CODE
    'a POST condition using a name a later POST declares is an error';

throws-like q:to/CODE/, X::Redeclaration::Outer,
    my $a = 'outer';
    for 1..1 { POST say($a); FIRST my $a = 1 }
    CODE
    'a POST condition using a name a later FIRST declares is an error';

is (try EVAL q:to/CODE/),
    $_ = 'outer';
    -> { POST my $_ := True; $_ }()
    CODE
    'outer',
    'a topic declared by POST stays in the POST condition';

is (try EVAL q:to/CODE/),
    sub f() { my &x = &foo; POST sub foo() { 1 }; x() }
    f()
    CODE
    1,
    'a sub declared by POST can be used before the POST';

is (try EVAL q:to/CODE/),
    sub f() { POST sub foo { ... }; sub foo { 42 }; foo() }
    f()
    CODE
    42,
    'a sub stubbed by POST takes a later definition';

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is (try EVAL Q:to/CODE/.AST.raku.EVAL),
        sub f() { POST my $/ = 1; my $/ = 2; $/ }
        f()
        CODE
        2,
        'a POST statement built as an AST shares a redeclared match variable';

    throws-like q:to/CODE/, X::Redeclaration,
        sub f() { POST my \a = 1; my class a { }; 5 }
        CODE
        'a class of a name a POST declares is a redeclaration';

    is (try EVAL Q:to/CODE/.AST.DEPARSE),
        my $seen;
        sub f() { POST my $a = $_; $seen = { $a }; 7 }
        f();
        $seen()
        CODE
        7,
        'a POST statement deparsed and run again gives its declarations to the routine';

    unlike Q[sub f() { POST my $a = $_; 1 }].AST.DEPARSE, / ';;' /,
        'a POST statement deparses with one statement terminator';

    is-deeply (try EVAL q:to/CODE/),
        my @seen;
        sub f() { POST my str $s = "x"; @seen.push: $s.chars; 5 }
        f();
        @seen
        CODE
        [0],
        'a native str declared by POST is empty before its initializer runs';

    is-deeply (try EVAL q:to/CODE/),
        my @seen;
        sub f() { POST (FIRST my $x = 5); @seen.push: $x; 5 }
        f(); f();
        @seen
        CODE
        [5, Any],
        'a FIRST statement nested in a POST statement declares in the routine';
}
else {
    skip 'the legacy frontend neither round trips an AST, reports the class, deparses, gives a hoisted native its default, nor declares for a nested phaser', 6;
}
