use MONKEY-SEE-NO-EVAL;
use Test;

plan 7;

{
    my $x = 12;
    my $y = 99;
    is-deeply
            EVAL(RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                    RakuAST::ApplyPrefix.new(
                        prefix => RakuAST::Prefix.new('++'),
                        operand => RakuAST::Var::Lexical.new('$x'))),
                RakuAST::Statement::Expression.new(
                    RakuAST::ApplyPrefix.new(
                        prefix => RakuAST::Prefix.new('++'),
                        operand => RakuAST::Var::Lexical.new('$y')))
            )),
            100,
            'Statement list evaluates to its final statement';
    is $x, 13, 'First side-effecting statement was executed';
    is $y, 100, 'Second side-effecting statement was executed';
}

{
    my $x = False;
    my $y = 9;
    is-deeply
            EVAL(RakuAST::Statement::Unless.new(
                condition => RakuAST::Var::Lexical.new('$x'),
                body => RakuAST::Block.new(body =>
                    RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::ApplyPrefix.new(
                                    prefix => RakuAST::Prefix.new('++'),
                                    operand => RakuAST::Var::Lexical.new('$y'))))))
            )),
            10,
            'An unless block with a false condition evaluates to its body';
    is $y, 10, 'Side-effect of the body was performed';
}

{
    my $x = True;
    my $y = 9;
    is-deeply
            EVAL(RakuAST::Statement::Unless.new(
                condition => RakuAST::Var::Lexical.new('$x'),
                body => RakuAST::Block.new(body =>
                    RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::ApplyPrefix.new(
                                    prefix => RakuAST::Prefix.new('++'),
                                    operand => RakuAST::Var::Lexical.new('$y'))))))
            )),
            Empty,
            'An unless block with a false condition evaluates to Empty';
    is $y, 9, 'Side-effect of the body was not performed';
}
