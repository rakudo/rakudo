use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

{
    my $a = 10;
    is-deeply
        EVAL(RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$a'),
            infix => RakuAST::MetaInfix::Assign.new(RakuAST::Infix.new('+')),
            right => RakuAST::IntLiteral.new(3)
        )),
        13,
        'Assignment meta-op evaluates to expected value';
    is-deeply $a, 13, 'Really did mutate the variable';
}

{
    my $test = 10;
    my $update = 2;

    my $or-ast = RakuAST::ApplyInfix.new(
        left => RakuAST::Var::Lexical.new('$test'),
        infix => RakuAST::MetaInfix::Assign.new(RakuAST::Infix.new('||')),
        right => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('$update'),
            postfix => RakuAST::Postfix.new('++')
        )
    );

    is-deeply EVAL($or-ast), 10,
        'Assignment meta-op with short-circuit || evaluates to true LHS';
    is-deeply $update, 2, 'Really did short-circuit, and not evaluate RHS';

    $test = 0;
    is-deeply EVAL($or-ast), 2,
        'Assignment meta-op with short-circuit || evaluates to RHS when LHS false';
    is-deeply $update, 3, 'Really did evaluate RHS';
}
