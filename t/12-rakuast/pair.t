use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'Fat arrow syntax forms a Pair' => {
    # answer => 42
    ast RakuAST::FatArrow.new(
      key => 'answer',
      value => RakuAST::IntLiteral.new(42)
    );

    is-deeply $_, (answer => 42),
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'True colonpair forms a Pair with value True' => {
    # :r
    ast RakuAST::ColonPair::True.new(key => 'r');

    is-deeply $_, (r => True),
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'False colonpair forms a Pair with value False' => {
    # :!r
    ast RakuAST::ColonPair::False.new(key => 'r');

    is-deeply $_, (r => False),
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Number colonpair forms a Pair with the correct Int value' => {
    # :answer(42)
    ast RakuAST::ColonPair::Number.new(
      key => 'answer',
      value => RakuAST::IntLiteral.new(42)
    );

    is-deeply $_, (answer => 42)
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Value colonpair forms a Pair with the correct value' => {
    # :cheese<stilton>
    ast RakuAST::ColonPair::Value.new(
      key => 'cheese',
      value => RakuAST::StrLiteral.new('stilton')
    );

    is-deeply $_, (cheese => 'stilton')
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Variable colonpair forms a Pair that looks up the variable' => {
    my $curry = 'red';

    # :$curry
    ast RakuAST::ColonPair::Variable.new(
      key => 'curry',
      value => RakuAST::Var::Lexical.new('$curry')
    );

    is-deeply $_, (curry => 'red')
      for EVAL($ast), EVAL($ast.DEPARSE);
}

# vim: expandtab shiftwidth=4
