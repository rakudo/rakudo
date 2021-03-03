use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'The postfix if statement works with a true expression' => {
    # 42 if 666
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
        condition-modifier => RakuAST::StatementModifier::Condition::If.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(666)
          )
        )
      )
    );

    is-deeply $_, 42
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix if statement works with a false expression' => {
    # 42 if ""
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
        condition-modifier => RakuAST::StatementModifier::Condition::If.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::StrLiteral.new("")
          )
        )
      )
    );

    # some weird interaction with Empty and postfix for
    is-deeply EVAL($ast), Empty;
    is-deeply EVAL($ast.DEPARSE), Empty;
}

subtest 'The postfix unless statement works with a false expression' => {
    # 42 unless ""
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
        condition-modifier => RakuAST::StatementModifier::Condition::Unless.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::StrLiteral.new("")
          )
        )
      )
    );

    is-deeply $_, 42
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix if statement works with a false expression' => {
    # 42 unless 666
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
        condition-modifier => RakuAST::StatementModifier::Condition::Unless.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(666)
          )
        )
      )
    );

    # some weird interaction with Empty and postfix for
    is-deeply EVAL($ast), Empty;
    is-deeply EVAL($ast.DEPARSE), Empty;
}

# vim: expandtab shiftwidth=4
