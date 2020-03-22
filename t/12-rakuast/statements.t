use MONKEY-SEE-NO-EVAL;
use Test;

plan 3;

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
