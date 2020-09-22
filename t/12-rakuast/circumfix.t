use MONKEY-SEE-NO-EVAL;
use Test;

plan 7;

my $ast;

subtest 'Parenthesized expressions compile correctly' => {
    # 2 * (3 + 4)
    $ast := RakuAST::ApplyInfix.new(
      left => RakuAST::IntLiteral.new(2),
      infix => RakuAST::Infix.new('*'),
      right => RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
          RakuAST::Statement::Expression.new(
            RakuAST::ApplyInfix.new(
              left => RakuAST::IntLiteral.new(3),
              infix => RakuAST::Infix.new('+'),
              right => RakuAST::IntLiteral.new(4)
            )
          )
        )
      )
    );
    is-deeply $_, 14
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Multi-statement semilist compiles into a List' => {
    # (3, 4)
    $ast := RakuAST::Circumfix::Parentheses.new(
      RakuAST::SemiList.new(
        RakuAST::IntLiteral.new(3),
        RakuAST::IntLiteral.new(4)
      )
    );
    is-deeply $_, (3, 4)
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Array composer produces an array' => {
    # [9, 10, 11]
    $ast := RakuAST::Circumfix::ArrayComposer.new(
      RakuAST::SemiList.new(
        RakuAST::IntLiteral.new(9),
        RakuAST::IntLiteral.new(10),
        RakuAST::IntLiteral.new(11)
      )
    );
    is-deeply $_, [9, 10, 11]
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Array composer works correctly with a single argument' => {
    # [5 .. 9]
    $ast := RakuAST::Circumfix::ArrayComposer.new(
      RakuAST::SemiList.new(
        RakuAST::ApplyInfix.new(
          left => RakuAST::IntLiteral.new(5),
          infix => RakuAST::Infix.new('..'),
          right => RakuAST::IntLiteral.new(9)
        )
      )
    );
    is-deeply $_, [5, 6, 7, 8, 9]
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Empty hash composer works correctly' => {
    # {}
    $ast := RakuAST::Circumfix::HashComposer.new;
    is-deeply $_, hash()
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Hash composer with fatarrow works correctly' => {
    # {a => 42}
    $ast := RakuAST::Circumfix::HashComposer.new(
      RakuAST::FatArrow.new(
        key => 'a',
        value => RakuAST::IntLiteral.new(42)
      )
    );
    is-deeply $_, {a => 42},
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Hash composer with list of fat arrows works correctly' => {
    # {x => 11, y => 22}
    $ast := RakuAST::Circumfix::HashComposer.new(
      RakuAST::ApplyListInfix.new(
        infix => RakuAST::Infix.new(','),
        operands => [
          RakuAST::FatArrow.new(key => 'x', value => RakuAST::IntLiteral.new(11)),
          RakuAST::FatArrow.new(key => 'y', value => RakuAST::IntLiteral.new(22))
        ]
      )
    );
    is-deeply $_, {x => 11, y => 22},
      for EVAL($ast), EVAL($ast.DEPARSE);
}
