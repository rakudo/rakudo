use MONKEY-SEE-NO-EVAL;
use Test;

plan 7;

{
    my $x = 42;
    is-deeply
            EVAL(RakuAST::Var::Lexical.new('$x')),
            42,
            'Lexical variable lookup ($ sigil)';
}

is-deeply
        EVAL(RakuAST::Var::Lexical.new('&plan')),
        &plan,
        'Lexical variable lookup (& sigil)';

{
    my $/;
    "abc" ~~ /(.)(.)/;
    is-deeply
        EVAL(RakuAST::Var::PositionalCapture.new(0)).Str,
        "a",
        'Positional capture variable lookup works (1)';
    is-deeply
        EVAL(RakuAST::Var::PositionalCapture.new(1)).Str,
        "b",
        'Positional capture variable lookup works (2)';
}

{
    my $/;
    "abc" ~~ /$<x>=(.)$<y>=(.)/;
    is-deeply
        EVAL(RakuAST::Var::NamedCapture.new('y')).Str,
        "b",
        'Named capture variable lookup works (1)';
    is-deeply
        EVAL(RakuAST::Var::NamedCapture.new("x")).Str,
        "a",
        'Named capture variable lookup works (2)';
}

{
    is-deeply
        EVAL(RakuAST::CompUnit.new(
            RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                    RakuAST::Declaration::Var.new(name => '$foo')
                ),
                RakuAST::Statement::Expression.new(
                    RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$foo'),
                        infix => RakuAST::Infix.new('='),
                        right => RakuAST::IntLiteral.new(10)
                    ),
                ),
                RakuAST::Statement::Expression.new(
                    RakuAST::Var::Lexical.new('$foo')
                ),
            )
        )),
        10,
        'Lexical variable declarations work';
}
