use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

is-deeply
        EVAL(RakuAST::ApplyInfix.new(
            left => RakuAST::IntLiteral.new(44),
            infix => RakuAST::Infix.new('+'),
            right => RakuAST::IntLiteral.new(22)
        )),
        66,
        'Application of an infix operator on two literals';

is-deeply
        EVAL(RakuAST::ApplyPrefix.new(
            prefix => RakuAST::Prefix.new('?'),
            operand => RakuAST::IntLiteral.new(2)
        )),
        True,
        'Application of a prefix operator to a literal (1)';

is-deeply
        EVAL(RakuAST::ApplyPrefix.new(
            prefix => RakuAST::Prefix.new('?'),
            operand => RakuAST::IntLiteral.new(0)
        )),
        False,
        'Application of a prefix operator to a literal (2)';

{
    sub postfix:<!>($n) {
        [*] 1..$n
    }
    is-deeply
            EVAL(RakuAST::ApplyPostfix.new(
                operand => RakuAST::IntLiteral.new(4),
                postfix => RakuAST::Postfix.new('!'),
            )),
            24,
            'Application of a (user-defined) postfix operator to a literal';
}
