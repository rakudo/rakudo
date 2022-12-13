use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 13;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'The postfix if statement works with a true expression' => {
    # 42 if 666
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::IntLiteral.new(42),
      condition-modifier => RakuAST::StatementModifier::If.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::IntLiteral.new(666)
        )
      )
    );

    is-deeply $_, 42
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix if statement works with a false expression' => {
    # 42 if ""
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::IntLiteral.new(42),
      condition-modifier => RakuAST::StatementModifier::If.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::StrLiteral.new("")
        )
      )
    );

    # some weird interaction with Empty and postfix for
    is-deeply EVAL($ast), Empty;
    is-deeply EVAL($ast.DEPARSE), Empty;
}

subtest 'The postfix unless statement works with a false expression' => {
    # 42 unless ""
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::IntLiteral.new(42),
      condition-modifier => RakuAST::StatementModifier::Unless.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::StrLiteral.new("")
        )
      )
    );

    is-deeply $_, 42
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix unless statement works with a true expression' => {
    # 42 unless 666
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::IntLiteral.new(42),
      condition-modifier => RakuAST::StatementModifier::Unless.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::IntLiteral.new(666)
        )
      )
    );

    # some weird interaction with Empty and postfix for
    is-deeply EVAL($ast), Empty;
    is-deeply EVAL($ast.DEPARSE), Empty;
}

subtest 'The postfix with statement works with defined expression' => {
    # $_ * 2 with 42
    ast RakuAST::Statement::Expression.new(
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
    );

    is-deeply $_, 84
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix with statement works with a type object' => {
    # $_ * 2 with Int
    ast RakuAST::Statement::Expression.new(
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
    );

    # some weird interaction with Empty and postfix for
    is-deeply EVAL($ast), Empty;
    is-deeply EVAL($ast.DEPARSE), Empty;
}

subtest 'The postfix without statement works with defined expression' => {
    # $_ * 2 without 42
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::ApplyInfix.new(
        left => RakuAST::Var::Lexical.new('$_'),
        infix => RakuAST::Infix.new('*'),
        right => RakuAST::IntLiteral.new(2)
      ),
      condition-modifier => RakuAST::StatementModifier::Without.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::IntLiteral.new(42)
        )
      )
    );

    # some weird interaction with Empty and postfix for
    is-deeply EVAL($ast), Empty;
    is-deeply EVAL($ast.DEPARSE), Empty;
}

subtest 'The postfix without statement works with a type object' => {
    # $_.Str without Int
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::ApplyPostfix.new(
        operand => RakuAST::Var::Lexical.new('$_'),
        postfix => RakuAST::Call::Method.new(
          name => RakuAST::Name.from-identifier('Str')
        )
      ),
      condition-modifier => RakuAST::StatementModifier::Without.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('Int')
          )
        )
      )
    );

    is-deeply $_, ""
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix given statement works with defined expression' => {
    # $_ * 2 given 42
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::ApplyInfix.new(
        left => RakuAST::Var::Lexical.new('$_'),
        infix => RakuAST::Infix.new('*'),
        right => RakuAST::IntLiteral.new(2)
      ),
      loop-modifier => RakuAST::StatementModifier::Given.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::IntLiteral.new(42)
        )
      )
    );

    is-deeply $_, 84
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix given statement works with a type object' => {
    # $_.Str given Int
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::ApplyPostfix.new(
        operand => RakuAST::Var::Lexical.new('$_'),
        postfix => RakuAST::Call::Method.new(
          name => RakuAST::Name.from-identifier('Str')
        )
      ),
      loop-modifier => RakuAST::StatementModifier::Given.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('Int')
          )
        )
      )
    );

    is-deeply $_, ""
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'The postfix while statement works' => {
    my $foo;

    # ++$foo while $foo < 5
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::ApplyPrefix.new(
        prefix => RakuAST::Prefix.new('++'),
        operand => RakuAST::Var::Lexical.new('$foo')
      ),
      loop-modifier => RakuAST::StatementModifier::While.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$foo'),
            infix => RakuAST::Infix.new('<'),
            right => RakuAST::IntLiteral.new(5),
          )
        )
      )
    );

    $foo = 0;
    EVAL($ast);  # return value is VMNull
    is-deeply $foo, 5;

    $foo = 0;
    EVAL($ast.DEPARSE);  # return value is VMNull
    is-deeply $foo, 5;
}

subtest 'The postfix until statement works' => {
    my $foo;

    # ++$foo until $foo >= 5
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::ApplyPrefix.new(
        prefix => RakuAST::Prefix.new('++'),
        operand => RakuAST::Var::Lexical.new('$foo')
      ),
      loop-modifier => RakuAST::StatementModifier::Until.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$foo'),
            infix => RakuAST::Infix.new('>='),
            right => RakuAST::IntLiteral.new(5),
          )
        )
      )
    );

    $foo = 0;
    EVAL($ast);  # return value is VMNull
    is-deeply $foo, 5;

    $foo = 0;
    EVAL($ast.DEPARSE);  # return value is VMNull
    is-deeply $foo, 5;
}

subtest 'The postfix for statement works' => {
    my $foo;

    # ++$foo for 1 .. 5
    ast RakuAST::Statement::Expression.new(
      expression => RakuAST::ApplyPrefix.new(
        prefix => RakuAST::Prefix.new('++'),
        operand => RakuAST::Var::Lexical.new('$foo')
      ),
      loop-modifier => RakuAST::StatementModifier::For.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::ApplyInfix.new(
            left => RakuAST::IntLiteral.new(1),
            infix => RakuAST::Infix.new('..'),
            right => RakuAST::IntLiteral.new(5),
          )
        )
      )
    );

    $foo = 0;
    EVAL($ast);  # return value is VMNull
    is-deeply $foo, 5;

    $foo = 0;
    EVAL($ast.DEPARSE);  # return value is VMNull
    is-deeply $foo, 5;
}

# vim: expandtab shiftwidth=4
