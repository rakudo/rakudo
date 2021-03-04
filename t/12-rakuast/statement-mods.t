use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

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
        condition-modifier => RakuAST::StatementModifier::If.new(
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
        condition-modifier => RakuAST::StatementModifier::If.new(
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
        condition-modifier => RakuAST::StatementModifier::Unless.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::StrLiteral.new("")
          )
        )
      )
    );

    is-deeply $_, 42
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix unless statement works with a true expression' => {
    # 42 unless 666
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
        condition-modifier => RakuAST::StatementModifier::Unless.new(
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

subtest 'The postfix with statement works with defined expression' => {
    # $_ * 2 with 42
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left => RakuAST::Var::Lexical.new('$_'),
          infix => RakuAST::Infix.new('*'),
          right => RakuAST::IntLiteral.new(2)
        ),
        condition-modifier => RakuAST::StatementModifier::With.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(42)
          )
        )
      )
    );

    is-deeply $_, 84
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix with statement works with a type object' => {
    # $_ * 2 with Int
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left => RakuAST::Var::Lexical.new('$_'),
          infix => RakuAST::Infix.new('*'),
          right => RakuAST::IntLiteral.new(2)
        ),
        condition-modifier => RakuAST::StatementModifier::With.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('Int')
            )
          )
        )
      )
    );

    # some weird interaction with Empty and postfix for
    is-deeply EVAL($ast), Empty;
    is-deeply EVAL($ast.DEPARSE), Empty;
}

# vim: expandtab shiftwidth=4
