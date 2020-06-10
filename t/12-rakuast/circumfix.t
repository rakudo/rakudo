use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

is-deeply EVAL(RakuAST::ApplyInfix.new(
        left => RakuAST::IntLiteral.new(2),
        infix => RakuAST::Infix.new('*'),
        right => RakuAST::Circumfix::Parentheses.new(
            RakuAST::SemiList.new(
                RakuAST::Statement::Expression.new(
                    RakuAST::ApplyInfix.new(
                        left => RakuAST::IntLiteral.new(3),
                        infix => RakuAST::Infix.new('+'),
                        right => RakuAST::IntLiteral.new(4)
                    ))))
    )),
    14,
    'Parenthesized expressions compile correctly';

is-deeply EVAL(
    RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
            RakuAST::IntLiteral.new(3),
            RakuAST::IntLiteral.new(4)
        )
    )),
    (3, 4),
    'Multi-statement semilist compiles into a List';

is-deeply EVAL(
    RakuAST::Circumfix::ArrayComposer.new(
        RakuAST::SemiList.new(
            RakuAST::IntLiteral.new(9),
            RakuAST::IntLiteral.new(10),
            RakuAST::IntLiteral.new(11)
        )
    )),
    [9,10,11],
    'Array composer produces an array';

is-deeply EVAL(
    RakuAST::Circumfix::ArrayComposer.new(
        RakuAST::SemiList.new(
            RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(5),
                infix => RakuAST::Infix.new('..'),
                right => RakuAST::IntLiteral.new(9)
            )
        )
    )),
    [5,6,7,8,9],
    'Array composer works correctly with a single argument';
