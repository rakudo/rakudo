use MONKEY-SEE-NO-EVAL;
use Test;

plan 1;

is-deeply
        EVAL(RakuAST::ApplyInfix.new(
            left => RakuAST::IntLiteral.new(44),
            infix => RakuAST::Infix.new('+'),
            right => RakuAST::IntLiteral.new(22)
        )),
        66,
        'Application of an infix operator on two literals';
