use MONKEY-SEE-NO-EVAL;
use Test;

plan 31;

is-deeply  # 44 + 22
        EVAL(RakuAST::ApplyInfix.new(
            left => RakuAST::IntLiteral.new(44),
            infix => RakuAST::Infix.new('+'),
            right => RakuAST::IntLiteral.new(22)
        )),
        66,
        'Application of an infix operator on two literals';

{
    is-deeply  # 22 || 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(22),
                infix => RakuAST::Infix.new('||'),
                right => RakuAST::IntLiteral.new(44)
            )),
            22,
            'The special form || operator works (1)';
    is-deeply  # 0 || 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(0),
                infix => RakuAST::Infix.new('||'),
                right => RakuAST::IntLiteral.new(44)
            )),
            44,
            'The special form || operator works (2)';

    is-deeply  # 22 or 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(22),
                infix => RakuAST::Infix.new('or'),
                right => RakuAST::IntLiteral.new(44)
            )),
            22,
            'The special form or operator works (1)';
    is-deeply  # 0 or 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(0),
                infix => RakuAST::Infix.new('or'),
                right => RakuAST::IntLiteral.new(44)
            )),
            44,
            'The special form or operator works (2)';

    is-deeply  # 22 && 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(22),
                infix => RakuAST::Infix.new('&&'),
                right => RakuAST::IntLiteral.new(44)
            )),
            44,
            'The special form && operator works (1)';
    is-deeply  # 0 && 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(0),
                infix => RakuAST::Infix.new('&&'),
                right => RakuAST::IntLiteral.new(44)
            )),
            0,
            'The special form && operator works (2)';

    is-deeply  # 22 and 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(22),
                infix => RakuAST::Infix.new('and'),
                right => RakuAST::IntLiteral.new(44)
            )),
            44,
            'The special form and operator works (1)';
    is-deeply  # 0 and 44
            EVAL(RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(0),
                infix => RakuAST::Infix.new('and'),
                right => RakuAST::IntLiteral.new(44)
            )),
            0,
            'The special form and operator works (2)';
}

is-deeply  # ?2
        EVAL(RakuAST::ApplyPrefix.new(
            prefix => RakuAST::Prefix.new('?'),
            operand => RakuAST::IntLiteral.new(2)
        )),
        True,
        'Application of a prefix operator to a literal (1)';

is-deeply  # ?0
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
    is-deeply  # 4!
            EVAL(RakuAST::ApplyPostfix.new(
                operand => RakuAST::IntLiteral.new(4),
                postfix => RakuAST::Postfix.new('!'),
            )),
            24,
            'Application of a (user-defined) postfix operator to a literal';
}

{
    my $a = 1;
    is-deeply  # $a = 4
        EVAL(RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$a'),
            infix => RakuAST::Infix.new('='),
            right => RakuAST::IntLiteral.new(4)
        )),
        4,
        'Basic assignment to a Scalar container';
}

{
    my @a = 10..20;
    is-deeply  # @a[5]
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('@a'),
            postfix => RakuAST::Postcircumfix::ArrayIndex.new(
                RakuAST::SemiList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(5)
                    )
                )
            )
        )),
        15,
        'Basic single-dimension array index';

    is-deeply  # @a[]
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('@a'),
            postfix => RakuAST::Postcircumfix::ArrayIndex.new(
                RakuAST::SemiList.new()
            )
        )),
        @a,
        'Zen array slice';
}

{
    my @a[3;3] = <a b c>, <d e f>, <g h i>;
    is-deeply  # @a[2;1]
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('@a'),
            postfix => RakuAST::Postcircumfix::ArrayIndex.new(
                RakuAST::SemiList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(2)
                    ),
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(1)
                    )
                )
            )
        )),
        'h',
        'Multi-dimensional array indexing';
}

{
    my %h = a => 'add', s => 'subtract';
    is-deeply  # %h{('s',)}         XXX not sure
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('%h'),
            postfix => RakuAST::Postcircumfix::HashIndex.new(
                RakuAST::SemiList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::StrLiteral.new('s')
                    )
                )
            )
        )),
        'subtract',
        'Basic single-dimension hash index';

    is-deeply  # %h{}
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('%h'),
            postfix => RakuAST::Postcircumfix::HashIndex.new(
                RakuAST::SemiList.new()
            )
        )),
        %h,
        'Zen hash slice';

    is-deeply  # %h<s>
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('%h'),
            postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
                RakuAST::QuotedString.new(
                    segments => [RakuAST::StrLiteral.new('s')],
                    processors => ['words']
                )
            )
        )),
        'subtract',
        'Basic literal hash index';

    is-deeply  # %h<s a>
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('%h'),
            postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
                RakuAST::QuotedString.new(
                    segments => [RakuAST::StrLiteral.new('s a')],
                    processors => ['words']
                )
            )
        )),
        ('subtract', 'add'),
        'Literal hash index with multiple keys';

    is-deeply  # %h<>
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('%h'),
            postfix => RakuAST::Postcircumfix::LiteralHashIndex.new(
                RakuAST::QuotedString.new(
                    segments => [RakuAST::StrLiteral.new('')],
                    processors => ['words']
                )
            )
        )),
        %h,
        'Empty literal hash index works as zen slice';
}

{  # %h{'y'}{'a'}
    my %h = x => { :1a, :2b }, y => { :3a, :4b };
    is-deeply
        EVAL(RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('%h'),
            postfix => RakuAST::Postcircumfix::HashIndex.new(
                RakuAST::SemiList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::StrLiteral.new('y')
                    ),
                    RakuAST::Statement::Expression.new(
                        RakuAST::StrLiteral.new('a')
                    )
                )
            )
        )),
        (3,), # Is this actually a CORE.setting bug?
        'Multi-dimensional hash indexing';
}

is-deeply  # (10, 11, 12)
        EVAL(RakuAST::ApplyListInfix.new(
            infix => RakuAST::Infix.new(','),
            operands => (
                RakuAST::IntLiteral.new(10),
                RakuAST::IntLiteral.new(11),
                RakuAST::IntLiteral.new(12),
            )
        )),
        (10, 11, 12),
        'Application of a list infix operator on three operands';

is-deeply  # ()
        EVAL(RakuAST::ApplyListInfix.new(
            infix => RakuAST::Infix.new(','),
            operands => ()
        )),
        (),
        'Application of a list infix operator on no operands';

{  # 5 > $x++ > 3
    my $x = 4;
    is-deeply
        EVAL(RakuAST::ApplyInfix.new(
            left => RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(5),
                infix => RakuAST::Infix::Chaining.new('>'),
                right => RakuAST::ApplyPostfix.new(
                    postfix => RakuAST::Postfix.new('++'),
                    operand => RakuAST::Var::Lexical.new('$x')
                )
            ),
            infix => RakuAST::Infix::Chaining.new('>'),
            right => RakuAST::IntLiteral.new(3)
        )),
        True,
        'Chaining operator has correct outcome';
    is-deeply $x, 5, 'Middle expression of chain only evaluated once';
}

{  # $a ?? 22 !! 33
    my $ternary = RakuAST::Ternary.new(
        condition => RakuAST::Var::Lexical.new('$a'),
        then => RakuAST::IntLiteral.new(22),
        else => RakuAST::IntLiteral.new(33)
    );

    my $a = 1;
    is EVAL($ternary), 22, 'Correct outcome of ternary operator with true condition';
    $a = 0;
    is EVAL($ternary), 33, 'Correct outcome of ternary operator with false condition';
}

is-deeply  # "foo" . uc
        EVAL(RakuAST::ApplyDottyInfix.new(
            left => RakuAST::StrLiteral.new('foo'),
            infix => RakuAST::DottyInfix::Call.new(),
            right => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('uc')
            )
        )),
        'FOO',
        'Application of dotty infix `.`';

{
    my $var = 'foo';
    is-deeply  # $var .= tc
            EVAL(RakuAST::ApplyDottyInfix.new(
                left => RakuAST::Var::Lexical.new('$var'),
                infix => RakuAST::DottyInfix::CallAssign.new(),
                right => RakuAST::Call::Method.new(
                    name => RakuAST::Name.from-identifier('tc')
                )
            )),
            'Foo',
            'Application of dotty infix `.=` evaluates to expected value';
    is-deeply $var, 'Foo', 'The assignment to the variable happened correctly also';
}
