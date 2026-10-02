use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use MONKEY-SEE-NO-EVAL;
use nqp;

plan 29;

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my $a = 5; @seen.push: $a }
    @seen
    CODE
    [5, Any, Any],
    'a scalar declared by FIRST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    sub f() {
        my $result;
        for 1..3 { FIRST my @a; @a.push: $_; LAST $result = @a.List }
        $result
    }
    f()
    CODE
    (3,),
    'an array declared by FIRST in a routine is visible to the LAST phaser';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my $a is default(3) = 5; @seen.push: $a }
    @seen
    CODE
    [5, 3, 3],
    'a traited variable declared by FIRST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my $a = 5; my &c = { $a }; @seen.push: c() }
    @seen
    CODE
    [5, Any, Any],
    'a closure in the loop body sees a variable declared by FIRST';

is (try EVAL q:to/CODE/),
    my $count = 0;
    for 1..3 { FIRST $count++ }
    $count
    CODE
    1,
    'a FIRST statement runs once per loop';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 { FIRST { my $a = 7; @seen.push: $a } }
    @seen
    CODE
    [7],
    'a FIRST block keeps its own declarations';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 { FIRST sub foo() { 42 }; @seen.push: foo() }
    @seen
    CODE
    [42, 42],
    'a sub declared by FIRST is callable in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 { my $a = 1; FIRST { my $a = 7 }; @seen.push: $a }
    @seen
    CODE
    [1, 1],
    'a FIRST block declaration does not reach the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my ($a, $b) = 1, 2; @seen.push: $b }
    @seen
    CODE
    [2, Any, Any],
    'a list declared by FIRST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 { my $a = 3; FIRST my $a = 1; @seen.push: $a }
    @seen
    CODE
    [3, 3],
    'a loop body variable redeclared by FIRST keeps the value the loop body gives it';

throws-like q:to/CODE/, X::Redeclaration::Outer,
    my $a = 'outer';
    for 1..2 { my $b = $a; FIRST my $a = 1 }
    CODE
    'a FIRST declaration of a name already used from outside is an error';

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is-deeply (try EVAL Q:to/CODE/.AST.raku.EVAL),
        my @seen;
        for 1..3 { FIRST my $a = 5; @seen.push: $a }
        @seen
        CODE
        [5, Any, Any],
        'a FIRST statement built as an AST gives its declarations to the loop body';
}
else {
    skip 'the legacy frontend rejects the RakuAST source of a round trip';
}

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my %h = a => 1; @seen.push: %h<a> }
    @seen
    CODE
    [1, Any, Any],
    'a hash declared by FIRST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my Int $i = 5; @seen.push: $i }
    @seen
    CODE
    [5, Int, Int],
    'a typed scalar declared by FIRST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my int $n = 5; @seen.push: $n }
    @seen
    CODE
    [5, 0, 0],
    'a native int declared by FIRST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..3 { FIRST my \t = 5; @seen.push: t }
    @seen
    CODE
    [5, Mu, Mu],
    'a term declared by FIRST is visible in the loop body';

is-deeply (try EVAL q:to/CODE/),
    my @c;
    for 1..3 { FIRST my $a = 5; $a = $_ * 10 if $_ > 1; @c.push: { $a } }
    @c.map({ .() }).List
    CODE
    (5, 20, 30),
    'closures made in the loop body keep the FIRST variable of their own iteration';

is-deeply (try EVAL q:to/CODE/),
    my @seen;
    for 1..2 {
        FIRST my $a = 'outer';
        for 1..2 { FIRST my $a = 'inner'; @seen.push: $a }
        @seen.push: $a
    }
    @seen
    CODE
    ['inner', Any, 'outer', 'inner', Any, Any],
    'nested loops each keep the variable their own FIRST declares';

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is (try EVAL q:to/CODE/),
        my $x = 0;
        for ^2 { FIRST while $x < 3 { $x++ } }
        $x
        CODE
        3,
        'a while loop in a FIRST statement runs';

    is (try EVAL q:to/CODE/),
        my $x = 0;
        for ^2 { FIRST $x++ while $x < 3 }
        $x
        CODE
        3,
        'a while modifier in a FIRST statement runs';

    is-deeply (try EVAL q:to/CODE/),
        my @r;
        for 1..2 { my $v = FIRST (1, 2) ~~ :(Int, Int); @r.push: $v }
        @r
        CODE
        [True, True],
        'a signature literal in a FIRST statement matches';

    is-deeply (try EVAL q:to/CODE/),
        my @lines;
        for 1..2 {
            FIRST @lines.push: +callframe(0).line;
        }
        @lines
        CODE
        [3],
        'a FIRST statement reports its own line';

    is-deeply (try EVAL q:to/CODE/),
        my @seen;
        for 1..3 { FIRST my str $s = "x"; @seen.push: $s.chars }
        @seen
        CODE
        [1, 0, 0],
        'a native str declared by FIRST is empty where its initializer did not run';

    is-deeply (try EVAL q:to/CODE/),
        my @seen;
        for 1..3 { FIRST (FIRST my $x = 5); @seen.push: $x }
        @seen
        CODE
        [5, Any, Any],
        'a FIRST statement nested in a FIRST statement declares in the loop body';

    is-deeply (try EVAL Q:to/CODE/.AST.DEPARSE),
        my @seen;
        for 1..3 { FIRST my $a = 5; @seen.push: $a }
        @seen
        CODE
        [5, Any, Any],
        'a FIRST statement deparsed and run again gives its declarations to the loop body';

    throws-like q:to/CODE/, X::Comp::AdHoc, message => / 'in a FIRST statement' /,
        sub f() { FIRST my ($a, $b) := (1, 2); 42 }
        CODE
        'a list bound by FIRST is a compile-time error';

    my $in = "a\nb\nc\n";
    is-run 'FIRST my $x = 0; $x++; say $x',
        'a variable declared by FIRST in a -n program persists across lines',
        :compiler-args['-n'], :in($in), :out("1\n2\n3\n");
    is-run 'FIRST my int $n = 5; say $n',
        'a native declared by FIRST in a -n program persists across lines',
        :compiler-args['-n'], :in($in), :out("5\n5\n5\n");
    is-run 'FIRST my @a; @a.push: $_; say @a.elems',
        'an array declared by FIRST in a -n program persists across lines',
        :compiler-args['-n'], :in($in), :out("1\n2\n3\n");
}
else {
    skip 'the legacy frontend runs no loop in a FIRST statement, matches no signature literal there, reports no line, and hoists no FIRST declaration', 11;
}
