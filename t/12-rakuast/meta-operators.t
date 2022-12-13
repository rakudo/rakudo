use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 4;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'Assignment meta-op evaluates to expected value' => {
    my $a = 10;

    # $a += 3
    ast RakuAST::ApplyInfix.new(
      left => RakuAST::Var::Lexical.new('$a'),
      infix => RakuAST::MetaInfix::Assign.new(RakuAST::Infix.new('+')),
      right => RakuAST::IntLiteral.new(3)
    );

    is-deeply EVAL($ast), 13, 'AST: evaluates to expected value';
    is-deeply $a, 13, 'Really did mutate the variable';

    is-deeply EVAL($ast.DEPARSE), 16, 'DEPARSE: evaluates to expected value';
    is-deeply $a, 16, 'Really did mutate the variable';
}

subtest 'Assignment meta-op with short-circuit || evaluates to true LHS' => {
    my $test = 10;
    my $update = 2;

    # $test ||= $update.++
    ast RakuAST::ApplyInfix.new(
      left => RakuAST::Var::Lexical.new('$test'),
      infix => RakuAST::MetaInfix::Assign.new(
        RakuAST::Infix.new('||')
      ),
      right => RakuAST::ApplyPostfix.new(
        operand => RakuAST::Var::Lexical.new('$update'),
        postfix => RakuAST::Postfix.new('++')
      )
    );

    is-deeply EVAL($ast), 10,
      'AST: short-circuit || evaluates to true LHS';
    is-deeply $update, 2,
      'AST: Really did short-circuit, and not evaluate RHS';

    is-deeply EVAL($ast.DEPARSE), 10,
      'DEPARSE: short-circuit || evaluates to true LHS';
    is-deeply $update, 2,
      'DEPARSE: Really did short-circuit, and not evaluate RHS';

    $test = 0;
    is-deeply EVAL($ast), 2,
      'AST: no short-circuit || evaluates to RHS when LHS false';
    is-deeply $update, 3,
      'AST: Really did evaluate RHS';

    $test = 0;
    is-deeply EVAL($ast.DEPARSE), 3,
      'DEPARSE: no short-circuit || evaluates to RHS when LHS false';
    is-deeply $update, 4,
      'DEPARSE: Really did evaluate RHS';
}

subtest 'Reduce meta-op on left associative operator' => {
    # [+](1, 2, 3)
    ast RakuAST::Term::Reduce.new(
        infix => RakuAST::Infix.new('+'),
        args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(1),
            RakuAST::IntLiteral.new(2),
            RakuAST::IntLiteral.new(3))
    );

    is-deeply EVAL($ast), 6, 'AST: evaluates to expected value';
    is-deeply EVAL($ast.DEPARSE), 6, 'DEPARSE: evaluates to expected value';
}

subtest 'Triangle reduce meta-op on left associative operator' => {
    # [\+](1, 2, 3)
    ast RakuAST::Term::Reduce.new(
        infix => RakuAST::Infix.new('+'),
        args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(1),
            RakuAST::IntLiteral.new(2),
            RakuAST::IntLiteral.new(3)),
        triangle => True
    );

    is-deeply EVAL($ast), (1, 3, 6), 'AST: evaluates to expected value';
    is-deeply EVAL($ast.DEPARSE), (1, 3, 6), 'DEPARSE: evaluates to expected value';
}

# vim: expandtab shiftwidth=4
