use MONKEY-SEE-NO-EVAL;
use Test;

plan 15;

my $ast;

sub no-args() {
    444
}
subtest 'Can make a named call with no arguments' => {
    # no-args()
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('no-args')
    );
    is-deeply $_, 444
      for EVAL($ast), EVAL($ast.DEPARSE);
}

sub one-arg($x) {
    9 * $x
}
subtest 'Can make a named call with one positional argument' => {
    # one-arg(5)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('one-arg'),
      args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(5))
    );
    is-deeply $_, 45
      for EVAL($ast), EVAL($ast.DEPARSE);
}

sub two-args($x, $y) {
    $x - $y
}
subtest 'Can make a named call with two positional arguments' => {
    # two-args(5, 3)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('two-args'),
      args => RakuAST::ArgList.new(
        RakuAST::IntLiteral.new(5),
        RakuAST::IntLiteral.new(3),
      )
    );
    is-deeply $_, 2
      for EVAL($ast), EVAL($ast.DEPARSE);
}

sub two-named(:$n1, :$n2) {
    $n1 / $n2
}
subtest 'Can make a named call with two named arguments' => {
    # two-named(n1 => 200, n2 => 4)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('two-named'),
      args => RakuAST::ArgList.new(
        RakuAST::FatArrow.new(
          key => 'n1',
          value => RakuAST::IntLiteral.new(200)
        ),
        RakuAST::FatArrow.new(
          key => 'n2',
          value => RakuAST::IntLiteral.new(4)
        )
      )
    );
    is-deeply $_, 50.0
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Duplicated named arguments are correctly handled' => {
    # two-named(n1 => 200, n2 => 4, n1 => 400)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('two-named'),
      args => RakuAST::ArgList.new(
        RakuAST::FatArrow.new(
          key => 'n1',
          value => RakuAST::IntLiteral.new(200)
        ),
        RakuAST::FatArrow.new(
          key => 'n2',
          value => RakuAST::IntLiteral.new(4)
        ),
        RakuAST::FatArrow.new(
          key => 'n1',
          value => RakuAST::IntLiteral.new(400)
        ),
      )
    );
    is-deeply $_, 100.0
      for EVAL($ast), EVAL($ast.DEPARSE);
}

my $target = -> $a, $b { $a - $b }
subtest 'Can make a call on a term with two positional arguments' => {
    # $target(9, 4)
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::Var::Lexical.new('$target'),
      postfix => RakuAST::Call::Term.new(
        args => RakuAST::ArgList.new(
          RakuAST::IntLiteral.new(9),
          RakuAST::IntLiteral.new(4),
        )
      )
    );
    is-deeply $_, 5
      for EVAL($ast), EVAL($ast.DEPARSE);
}

class TestTarget {
    my $.route = 66;
    method subtract($x, $y) { $x - $y }
}
subtest 'Can make a call on a method without arguments' => {
    # TestTarget.route
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::Type::Simple.new(
        RakuAST::Name.from-identifier('TestTarget')
      ),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('route')
      )
    );
    is-deeply $_, 66
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Can make a call on a method with positional arguments' => {
    # TestTarget.subtract(14, 6)
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::Type::Simple.new(
        RakuAST::Name.from-identifier('TestTarget')
      ),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('subtract'),
        args => RakuAST::ArgList.new(
          RakuAST::IntLiteral.new(14),
          RakuAST::IntLiteral.new(6),
        )
      )
    );
    is-deeply $_, 8
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Method call WHAT compiles into MOP primitive' => {
    # 42.WHAT
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::IntLiteral.new(42),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('WHAT')
      )
    );
    is-deeply $_, Int
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Method call HOW compiles into MOP primitive' => {
    # 42.HOW
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::IntLiteral.new(42),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('HOW')
      )
    );
    is-deeply $_, Int.HOW
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Method call WHO compiles into MOP primitive' => {
    # 42.WHO
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::IntLiteral.new(42),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('WHO')
      )
    );
    isa-ok $_, Stash
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Method call DEFINITE compiles into MOP primitive' => {
    # 42.DEFINITE
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::IntLiteral.new(42),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('DEFINITE')
      )
    );
    is-deeply $_, True
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Method call REPR compiles into MOP primitive' => {
    # 42.REPR
    $ast := RakuAST::ApplyPostfix.new(
      operand => RakuAST::IntLiteral.new(42),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier('REPR')
      )
    );
    is-deeply $_, 'P6opaque'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Can make a call that flattens into array' => {
    my @args;
    # no-args(|@args)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('no-args'),
      args => RakuAST::ArgList.new(
        RakuAST::ApplyPrefix.new(
          prefix => RakuAST::Prefix.new('|'),
          operand => RakuAST::Var::Lexical.new('@args')
        )
      )
    );
    is-deeply $_, 444, 'flattening empty list'
      for EVAL($ast), EVAL($ast.DEPARSE);

    @args = 95, 40;
    # two-args(|@args)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('two-args'),
      args => RakuAST::ArgList.new(
        RakuAST::ApplyPrefix.new(
          prefix => RakuAST::Prefix.new('|'),
          operand => RakuAST::Var::Lexical.new('@args')
        )
      )
    );
    is-deeply $_, 55, 'two positional arguments'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Can make a call that flattens into hash' => {
    my %args;
    # no-args(|%args)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('no-args'),
      args => RakuAST::ArgList.new(
        RakuAST::ApplyPrefix.new(
          prefix => RakuAST::Prefix.new('|'),
          operand => RakuAST::Var::Lexical.new('%args')
        )
      )
    );
    is-deeply $_, 444, 'flattening empty list'
      for EVAL($ast), EVAL($ast.DEPARSE);

    %args<n1 n2> = 60, 12;
    # two-named(|%args)
    $ast := RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier('two-named'),
      args => RakuAST::ArgList.new(
        RakuAST::ApplyPrefix.new(
          prefix => RakuAST::Prefix.new('|'),
          operand => RakuAST::Var::Lexical.new('%args')
        )
      )
    );
    is-deeply $_, 5.0, 'flattening two named arguments'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

# vim: expandtab shiftwidth=4
