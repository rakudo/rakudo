use MONKEY-SEE-NO-EVAL;
use Test;

plan 53;

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

{
    my \cont = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(name => '$foo')
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('$foo')
            ),
        )
    ));
    is-deeply cont, Any, 'Default value of untyped container is Any';
    ok cont.VAR.of =:= Mu, 'Default constraint of untyped container is Mu';
}

is-deeply
    EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(
                    name => '$foo',
                    type => RakuAST::Type::Simple.new('Int'),
                )
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$foo'),
                    infix => RakuAST::Infix.new('='),
                    right => RakuAST::IntLiteral.new(99)
                ),
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('$foo')
            ),
        )
    )),
    99,
    'Typed variable declarations work (type matches in assignment)';

throws-like
    {
        EVAL(RakuAST::CompUnit.new(
            RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                    RakuAST::Declaration::Var.new(
                        name => '$foo',
                        type => RakuAST::Type::Simple.new('Int'),
                    )
                ),
                RakuAST::Statement::Expression.new(
                    RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$foo'),
                        infix => RakuAST::Infix.new('='),
                        right => RakuAST::NumLiteral.new(1e5)
                    ),
                ),
                RakuAST::Statement::Expression.new(
                    RakuAST::Var::Lexical.new('$foo')
                ),
            )
        ))
    },
    X::TypeCheck::Assignment,
    expected => Int,
    got => 1e5,
    'Typed variable declarations work (type mismatch throws)';

{
    my \result = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(
                    name => '$var',
                    initializer => RakuAST::Initializer::Assign.new(RakuAST::IntLiteral.new(125))
                )
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('$var')
            ),
        )
    ));
    is-deeply result, 125,
        'Lexical variable declarations with assignment initializer';
    ok result.VAR.isa(Scalar),
        'Really was an assignment into a Scalar container';
    nok result.VAR.dynamic, 'Is not dynamic';
    lives-ok { result = 42 },
        'Can update the container that was produced';
}

{
    my \result = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(
                    name => '$var',
                    initializer => RakuAST::Initializer::Bind.new(RakuAST::IntLiteral.new(225))
                )
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('$var')
            ),
        )
    ));
    is-deeply result, 225,
        'Lexical variable declarations with bind initializer';
    nok result.VAR.isa(Scalar),
        'Really was bound; no Scalar container';
    dies-ok { result = 42 },
        'Cannot assign as it is not a container';
}

{
    sub with-dyn(&test) {
        my $*dyn = 'in';
    }
    my $*dyn = 'out';
    is-deeply EVAL(RakuAST::Var::Dynamic.new('$*dyn')), 'out',
        'Dynamic variable access (1)';
    is-deeply with-dyn({ EVAL(RakuAST::Var::Dynamic.new('$*dyn')) }), 'in',
        'Dynamic variable access (2)';
    is-deeply EVAL(RakuAST::Var::Dynamic.new('$*OUT')), $*OUT,
        'Dynamic variable fallback also works';
}

{
    my \result = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(
                    name => '$*var',
                    initializer => RakuAST::Initializer::Assign.new(RakuAST::IntLiteral.new(360))
                )
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Dynamic.new('$*var')
            ),
        )
    ));
    is-deeply result, 360,
        'Dynamic variable declarations with assignment initializer, dynamic lookup';
    ok result.VAR.isa(Scalar),
        'Dynamic did an assignment into a Scalar container';
    ok result.VAR.dynamic, 'Is a dynamic';
    lives-ok { result = 99 },
        'Can update the container that was produced';
}

{
    my \cont = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(name => '@arr')
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('@arr')
            ),
        )
    ));
    is-deeply cont.WHAT, Array, '@ sigil var is initialized to Array';
    is-deeply cont.VAR.WHAT, Array, '@ sigil var not wrapped in Scalar';
    ok cont.defined, 'It is a defined Array instance';
    is cont.elems, 0, 'It is empty';
    is-deeply cont[0].VAR.WHAT, Scalar, 'Element is a Scalar';
    is-deeply cont[0], Any, 'Contains an Any by default';
    ok cont[0].VAR.of =:= Mu, 'Constraint is Mu by default';
}

{
    my \cont = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(name => '%hash')
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('%hash')
            ),
        )
    ));
    is-deeply cont.WHAT, Hash, '% sigil var is initialized to Hash';
    is-deeply cont.VAR.WHAT, Hash, '% sigil var not wrapped in Scalar';
    ok cont.defined, 'It is a defined Hash instance';
    is cont.elems, 0, 'It is empty';
    is-deeply cont<k>.VAR.WHAT, Scalar, 'Element is a Scalar';
    is-deeply cont<k>, Any, 'Contains an Any by default';
    ok cont<k>.VAR.of =:= Mu, 'Constraint is Mu by default';
}

{
    my \cont = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(
                    name => '@arr',
                    type => RakuAST::Type::Simple.new('Int')
                ),
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('@arr')
            ),
        )
    ));
    ok cont ~~ Array, '@ sigil var with Int type is an Array';
    ok cont ~~ Positional[Int], 'It does Positional[Int]';
    is-deeply cont.of, Int, '.of gives Int';
    is cont.elems, 0, 'It is empty';
    is-deeply cont[0].VAR.WHAT, Scalar, 'Element is a Scalar';
    is-deeply cont[0], Int, 'Contains an Int';
    ok cont[0].VAR.of =:= Int, 'Constraint is Int';
}

{
    my \cont = EVAL(RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Declaration::Var.new(
                    name => '%hash',
                    type => RakuAST::Type::Simple.new('Int')
                ),
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('%hash')
            ),
        )
    ));
    ok cont ~~ Hash, '% sigil var with Int type is a Hash';
    ok cont ~~ Associative[Int], 'It does Associative[Int]';
    is-deeply cont.of, Int, '.of gives Int';
    is cont.elems, 0, 'It is empty';
    is-deeply cont<k>.VAR.WHAT, Scalar, 'Element is a Scalar';
    is-deeply cont<k>, Int, 'Contains an Int';
    ok cont<k>.VAR.of =:= Int, 'Constraint is Int';
}
