use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use experimental :rakuast;
use nqp;

plan 52;

my @end-r;
my $check-end;
END is-deeply @end-r, [10], 'an END statement initializes a state variable' if $check-end;

unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    skip-rest 'state initialization in a thunk needs the RakuAST frontend';
    exit;
}

# The EVAL runs its END before the one above.
$check-end = True;
EVAL q[END @end-r.push: (state $n = 10)++];

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { ENTER @r.push: (state $n = 10)++ }
    @r
    CODE
    [10, 11, 12],
    'an ENTER statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { NEXT @r.push: (state $n = 10)++ }
    @r
    CODE
    [10, 11, 12],
    'a NEXT statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { LEAVE @r.push: (state $n = 10)++ }
    @r
    CODE
    [10, 11, 12],
    'a LEAVE statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { LAST @r.push: (state $n = 10)++ }
    @r
    CODE
    [10],
    'a LAST statement initializes a state variable';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { FIRST @r.push: (state $n = 10)++ }
    @r
    CODE
    [10],
    'a FIRST statement initializes a state variable';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..2 { @r.append: (state $n = 10)++ xx 2 }
    @r
    CODE
    [10, 11, 12, 13],
    'a thunk run repeatedly by xx initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { @r.push: try (state $n = 10)++ }
    @r
    CODE
    [10, 11, 12],
    'a try thunk initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..2 { NEXT @r.push: (state ($a, $b) = 10, 20)[0]++ }
    @r
    CODE
    [10, 11],
    'a NEXT statement initializes a state list declaration once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { NEXT @r.push: (state (\x, $b) = 10, 20)[1]++ }
    @r
    CODE
    [20, 21, 22],
    'a NEXT statement initializes a state list declaration led by a term';

lives-ok { EVAL q:to/CODE/ },
    my @r;
    for 1..3 { ENTER @r.push: (state ($a, $b) := (10, 20))[0] }
    CODE
    'an ENTER statement binding a state list declaration runs';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    sub f() { for 1..2 { NEXT @r.push: (state $n = 10)++ } }
    f(); f();
    @r
    CODE
    [10, 11, 10, 11],
    'each clone of the declaring block initializes its state variable';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    sub f() { ENTER @r.push: (state $n = 10)++ }
    f(); f(); f();
    @r
    CODE
    [10, 11, 12],
    'a state variable initialized by a phaser persists across calls';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    @r.push: (state $n = 10)++ for 1..3;
    @r
    CODE
    [10, 11, 12],
    'a loop modifier at unit scope initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { @r.push: (1 andthen (state $n = 10)++) }
    @r
    CODE
    [10, 11, 12],
    'an andthen thunk initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { @r.push: (gather take (state $n = 10)++)[0] }
    @r
    CODE
    [10, 11, 12],
    'a gather thunk initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { ENTER @r.push: (state @a = 1, 2).push(0).elems }
    @r
    CODE
    [3, 4, 5],
    'an ENTER statement initializes a state array once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { ENTER @r.push: (state $ = 10)++ }
    @r
    CODE
    [10, 11, 12],
    'an ENTER statement initializes an anonymous state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { NEXT @r.push: (state $n = 10)++ + (state $m = 100)++ }
    @r
    CODE
    [110, 112, 114],
    'a NEXT statement initializes two state variables once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    sub f() { ENTER @r.push: (state $n = 10)++ }
    BEGIN { f(); f() }
    @r
    CODE
    [10, 11],
    'a phaser of a routine called at BEGIN time initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    sub f($x = (state $n = 10)++) { $x }
    (f(), f(), f())
    CODE
    (10, 11, 12),
    'a parameter default initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    class StateInitInThunkPackage { our @r; @r.push: (state $n = 10)++ for 1..3 }
    @StateInitInThunkPackage::r
    CODE
    [10, 11, 12],
    'a loop modifier in a package body initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my $v = CHECK (state $c = 6)++;
    ($v, $c)
    CODE
    (6, 7),
    'a CHECK statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my $v = BEGIN (state $c = 6)++;
    ($v, $c)
    CODE
    (6, 7),
    'a BEGIN statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my $v = INIT (state $c = 6)++;
    ($v, $c)
    CODE
    (6, 7),
    'an INIT statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r = INIT (state $n = 10)++ xx 3;
    @r
    CODE
    [10, 11, 12],
    'a thunk in an INIT statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    BEGIN my &f = sub { state $n = 10; $n++ };
    (f(), f(), f())
    CODE
    (10, 11, 12),
    'a routine made by a BEGIN statement initializes its state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    my $i = 0;
    while $i++ < 3 { @r.push: try (state $n = 10)++ }
    @r
    CODE
    [10, 11, 12],
    'a try thunk in a while body initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    loop (my $i = 0; $i < 3; $i++) { NEXT @r.push: (state $n = 10)++ }
    @r
    CODE
    [10, 11, 12],
    'a NEXT statement in a loop body initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { if $_ { ENTER @r.push: (state $n = 10)++ } }
    @r
    CODE
    [10, 10, 10],
    'an ENTER statement in an if body initializes the state variable of each clone';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { with $_ { @r.push: try (state $n = 10)++ } }
    @r
    CODE
    [10, 10, 10],
    'a try thunk in a with body initializes the state variable of each clone';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { ENTER { @r.push: ((state $n = 10)++ xx 1)[0] } }
    @r
    CODE
    [10, 11, 12],
    'a thunk in an ENTER block initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { if $_ { FIRST @r.push: (state $n = 10)++ } }
    @r
    CODE
    [10, 10, 10],
    'a FIRST statement in an if body initializes the state variable of each clone';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { next if $_ == 1; @r.push: (state $n = 10)++ }
    @r
    CODE
    [10, 11],
    'a state initializer runs on its first reach';

is-deeply (try EVAL q:to/CODE/),
    sub f() { my @s; my $i = 0; @s.push((state $n = 10)++) while $i++ < 3; @s }
    (f(), f())
    CODE
    ([10, 11, 12], [13, 14, 15]),
    'a while modifier initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    my $j = 0;
    loop (; $j < 3; (state $n = 10)++) { @r.push: $n; $j++ }
    @r
    CODE
    [Any, 11, 12],
    'a loop increment initializes a state variable once';

is (try EVAL q:to/CODE/),
    my $count = 0;
    while (state $n = 10)++ < 12 { last if ++$count > 5 }
    $count
    CODE
    2,
    'a while condition initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { (state $n = 10)++ ==> push @r }
    @r
    CODE
    [10, 11, 12],
    'a feed initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    sub f() { my @r; @r.push($_) if (state $n = 10)++ < 12 for ^4; @r }
    (f(), f())
    CODE
    ([0, 1], []),
    'a condition modifier under a loop modifier initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r;
    for 1..3 { 'a' ~~ / :state $n = 10; { @r.push: $n++ } a / }
    @r
    CODE
    [10, 11, 12],
    'a regex initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r = BEGIN (state $n = 10)++ xx 3;
    @r
    CODE
    [10, 11, 12],
    'a thunk in a BEGIN statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r = CHECK (state $n = 10)++ xx 3;
    @r
    CODE
    [10, 11, 12],
    'a thunk in a CHECK statement initializes a state variable once';

is-deeply (try EVAL q:to/CODE/),
    my @r = BEGIN ((state $n = 10)++ for 1..3);
    @r
    CODE
    [10, 11, 12],
    'a loop modifier in a BEGIN statement initializes a state variable once';

is (try EVAL q:to/CODE/),
    constant K = (state $n = 10)++;
    K
    CODE
    10,
    'a constant initializes a state variable';

is-deeply (try EVAL q:to/CODE/),
    constant K = (state $n = 10)++ xx 2;
    K
    CODE
    (10, 11),
    'a thunk in a constant initializes a state variable once';

is (try EVAL q:to/CODE/),
    my $x is default((state $n = 7)++);
    $x
    CODE
    7,
    'a trait argument initializes a state variable';

lives-ok { EVAL q[my $v = BEGIN (state ($a, $b) := (10, 20))[0]] },
    'a BEGIN statement binding a state list declaration compiles';

lives-ok { EVAL q[INIT (state ($a, $b) := (1, 2))] },
    'an INIT statement binding a state list declaration runs';

is-deeply (try do {
    my $list = RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(expression => RakuAST::ApplyPostfix.new(
        operand => RakuAST::VarDeclaration::Simple.new(
          scope => 'state', sigil => '$',
          desigilname => RakuAST::Name.from-identifier('n'),
          initializer => RakuAST::Initializer::Assign.new(RakuAST::IntLiteral.new(10))),
        postfix => RakuAST::Postfix.new(operator => '++'))));
    ($list.EVAL, $list.EVAL, $list.EVAL)
}), (10, 10, 10), 'a statement list evaluated again initializes its state variable again';

my $in = "a\nb\nc\n";
is-run 'say (state $n = 10)++',
    'a state initializer in a -n program runs once',
    :compiler-args['-n'], :in($in), :out("10\n11\n12\n");
is-run '$_ ~= (state $n = 10)++',
    'a state initializer in a -p program runs once',
    :compiler-args['-p'], :in($in), :out("a10\nb11\nc12\n");
is-run 'state ($a, $b) = 10, 20; say $a',
    'a state list initializer in a -n program runs once',
    :compiler-args['-n'], :in($in), :out("10\n10\n10\n");
