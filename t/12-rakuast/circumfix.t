use MONKEY-SEE-NO-EVAL;
use Test;

plan 7;

is-deeply  # 2 * (3 + 4)
    EVAL(RakuAST::ApplyInfix.new(
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

is-deeply  # (3, 4)
    EVAL(RakuAST::Circumfix::Parentheses.new(
        RakuAST::SemiList.new(
            RakuAST::IntLiteral.new(3),
            RakuAST::IntLiteral.new(4)
        )
    )),
    (3, 4),
    'Multi-statement semilist compiles into a List';

is-deeply  # [9, 10, 11]
    EVAL(RakuAST::Circumfix::ArrayComposer.new(
        RakuAST::SemiList.new(
            RakuAST::IntLiteral.new(9),
            RakuAST::IntLiteral.new(10),
            RakuAST::IntLiteral.new(11)
        )
    )),
    [9, 10, 11],
    'Array composer produces an array';

is-deeply  # [5 .. 9]
    EVAL(RakuAST::Circumfix::ArrayComposer.new(
        RakuAST::SemiList.new(
            RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(5),
                infix => RakuAST::Infix.new('..'),
                right => RakuAST::IntLiteral.new(9)
            )
        )
    )),
    [5, 6, 7, 8, 9],
    'Array composer works correctly with a single argument';

is-deeply  # {}
    EVAL(RakuAST::Circumfix::HashComposer.new),
    hash(),
    'Empty hash composer works correctly';

is-deeply  # {a => 42}
    EVAL(RakuAST::Circumfix::HashComposer.new(
        RakuAST::FatArrow.new(key => 'a', value => RakuAST::IntLiteral.new(42))
    )),
    {a => 42},
    'Hash composer with fatarrow works correctly';

is-deeply  # {x => 11, y => 22}
    EVAL(RakuAST::Circumfix::HashComposer.new(
        RakuAST::ApplyListInfix.new(
            infix => RakuAST::Infix.new(','),
            operands => [
                RakuAST::FatArrow.new(key => 'x', value => RakuAST::IntLiteral.new(11)),
                RakuAST::FatArrow.new(key => 'y', value => RakuAST::IntLiteral.new(22))
            ]
        )
    )),
    {x => 11, y => 22},
    'Hash composer with list of fat arrows works correctly';
