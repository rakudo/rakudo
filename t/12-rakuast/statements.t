use MONKEY-SEE-NO-EVAL;
use Test;

plan 50;

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
    my ($a, $b, $c);

    my $test-ast := RakuAST::Statement::If.new(
        condition => RakuAST::Var::Lexical.new('$a'),
        then => RakuAST::Block.new(body =>
            RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(1)
                    )))),
        elsifs => [
            RakuAST::Statement::Elsif.new(
                condition => RakuAST::Var::Lexical.new('$b'),
                then => RakuAST::Block.new(body =>
                    RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::IntLiteral.new(2)
                            ))))),
            RakuAST::Statement::Elsif.new(
                condition => RakuAST::Var::Lexical.new('$c'),
                then => RakuAST::Block.new(body =>
                    RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::IntLiteral.new(3)
                            ))))),
        ],
        else => RakuAST::Block.new(body =>
            RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(4)
                    ))))
    );

    $a = $b = $c = False;
    is-deeply EVAL($test-ast), 4, 'When all conditions False, else is evaluated';

    $c = True;
    is-deeply EVAL($test-ast), 3, 'Latest elsif reachable when matched';

    $b = True;
    is-deeply EVAL($test-ast), 2, 'First elsif reachable when matched';

    $a = True;
    is-deeply EVAL($test-ast), 1, 'When the main condition is true, the then block is picked';
}

{
    my $a;

    my $test-ast := RakuAST::Statement::If.new(
        condition => RakuAST::Var::Lexical.new('$a'),
        then => RakuAST::Block.new(body =>
            RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(1)
                    ))))
    );

    $a = True;
    is-deeply EVAL($test-ast), 1, 'When simple if with no else has true condition, evaluates to branch';

    $a = False;
    is-deeply EVAL($test-ast), Empty, 'When simple if with no else has false condition, evaluates to Empty';
}

{
    my ($a, $b, $c);

    my $test-ast := RakuAST::Statement::With.new(
        condition => RakuAST::Var::Lexical.new('$a'),
        then => RakuAST::PointyBlock.new(
            signature => RakuAST::Signature.new(
                parameters => (
                    RakuAST::Parameter.new(
                        target => RakuAST::ParameterTarget::Var.new('$x')
                    ),
                )
            ),
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(1)
                    )))),
        elsifs => [
            RakuAST::Statement::Orwith.new(
                condition => RakuAST::Var::Lexical.new('$b'),
                then => RakuAST::PointyBlock.new(
                    signature => RakuAST::Signature.new(
                        parameters => (
                            RakuAST::Parameter.new(
                                target => RakuAST::ParameterTarget::Var.new('$x')
                            ),
                        )
                    ),
                    body => RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::IntLiteral.new(2)
                            ))))),
            RakuAST::Statement::Orwith.new(
                condition => RakuAST::Var::Lexical.new('$c'),
                then => RakuAST::PointyBlock.new(
                    signature => RakuAST::Signature.new(
                        parameters => (
                            RakuAST::Parameter.new(
                                target => RakuAST::ParameterTarget::Var.new('$x')
                            ),
                        )
                    ),
                    body => RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::IntLiteral.new(3)
                            ))))),
        ],
        else => RakuAST::PointyBlock.new(
            signature => RakuAST::Signature.new(
                parameters => (
                    RakuAST::Parameter.new(
                        target => RakuAST::ParameterTarget::Var.new('$x')
                    ),
                )
            ),
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(4)
                    ))))
    );

    $a = $b = $c = Nil;
    is-deeply EVAL($test-ast), 4, 'When all conditions undefined, else is evaluated';

    $c = False;
    is-deeply EVAL($test-ast), 3, 'Latest orwith reachable when matched';

    $b = False;
    is-deeply EVAL($test-ast), 2, 'First orwith reachable when matched';

    $a = False;
    is-deeply EVAL($test-ast), 1, 'When the main condition is defined, the then block is picked';
}

{
    my $a;

    my $test-ast := RakuAST::Statement::With.new(
        condition => RakuAST::Var::Lexical.new('$a'),
        then => RakuAST::PointyBlock.new(
            signature => RakuAST::Signature.new(
                parameters => (
                    RakuAST::Parameter.new(
                        target => RakuAST::ParameterTarget::Var.new('$x')
                    ),
                )
            ),
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::IntLiteral.new(1)
                    ))))
    );

    $a = False;
    is-deeply EVAL($test-ast), 1, 'When simple when with no else has defined condition, evaluates to branch';

    $a = Nil;
    is-deeply EVAL($test-ast), Empty, 'When simple with if with no else has undefined condition, evaluates to Empty';
}

{
    my $ast = RakuAST::Statement::With.new(
        condition => RakuAST::Var::Lexical.new('$a'),
        then => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::Var::Lexical.new('$_')
                    )))),
        else => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::Var::Lexical.new('$_')
                    )))),
    );
    my $a = 42;
    is-deeply EVAL($ast), 42, 'with topicalizes in the body';
    $a = Int;
    is-deeply EVAL($ast), Int, 'with topicalizes in the else body too';
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

{
    my $x = Nil;
    my $y = 9;
    is-deeply
            EVAL(RakuAST::Statement::Without.new(
                condition => RakuAST::Var::Lexical.new('$x'),
                body => RakuAST::Block.new(body =>
                    RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::ApplyPostfix.new(
                                    postfix => RakuAST::Postfix.new('++'),
                                    operand => RakuAST::Var::Lexical.new('$y'))))))
            )),
            9,
            'An without block with an undefined object evaluates to its body';
    is $y, 10, 'Side-effect of the body was performed';
}

{
    my $x = True;
    my $y = 9;
    is-deeply
            EVAL(RakuAST::Statement::Without.new(
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
            'An without block with a defined object evaluates to Empty';
    is $y, 9, 'Side-effect of the body was not performed';
}

{
    my $x = Cool;
    is-deeply
            EVAL(RakuAST::Statement::Without.new(
                condition => RakuAST::Var::Lexical.new('$x'),
                body => RakuAST::Block.new(body =>
                    RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                RakuAST::Var::Lexical.new('$_')))))
            )),
            Cool,
            'Without block with no argument sets the topic';
}

{
    my $x = 5;
    is-deeply
        EVAL(RakuAST::Statement::Loop::While.new(
            condition => RakuAST::Var::Lexical.new('$x'),
            body => RakuAST::Block.new(body =>
                RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::ApplyPrefix.new(
                                prefix => RakuAST::Prefix.new('--'),
                                operand => RakuAST::Var::Lexical.new('$x')))))))),
        Nil,
        'While loop at statement level evaluates to Nil';
    is-deeply $x, 0, 'Loop variable was decremented to zero';
}

{
    my $x = 5;
    is-deeply
        EVAL(RakuAST::Statement::Loop::Until.new(
            condition => RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('!'),
                operand => RakuAST::Var::Lexical.new('$x')
            ),
            body => RakuAST::Block.new(body =>
                RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::ApplyPrefix.new(
                                prefix => RakuAST::Prefix.new('--'),
                                operand => RakuAST::Var::Lexical.new('$x')))))))),
        Nil,
        'Until loop at statement level evaluates to Nil';
    is-deeply $x, 0, 'Loop variable was decremented to zero';
}

{
    my $x = 0;
    is-deeply
        EVAL(RakuAST::Statement::Loop::RepeatUntil.new(
            condition => RakuAST::Var::Lexical.new('$x'),
            body => RakuAST::Block.new(body =>
                RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::ApplyPrefix.new(
                                prefix => RakuAST::Prefix.new('--'),
                                operand => RakuAST::Var::Lexical.new('$x')))))))),
        Nil,
        'Repeat while loop at statement level evaluates to Nil';
    is-deeply $x, -1, 'Repeat while loop ran once';
}

{
    my $count = 0;
    is-deeply
        EVAL(RakuAST::Statement::Loop.new(
            setup => RakuAST::VarDeclaration::Simple.new(
                name => '$i',
                initializer => RakuAST::Initializer::Assign.new(
                    RakuAST::IntLiteral.new(9)
                )
            ),
            condition => RakuAST::Var::Lexical.new('$i'),
            increment => RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('--'),
                operand => RakuAST::Var::Lexical.new('$i')
            ),
            body => RakuAST::Block.new(body =>
                RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::ApplyPrefix.new(
                                prefix => RakuAST::Prefix.new('++'),
                                operand => RakuAST::Var::Lexical.new('$count')))))))),
        Nil,
        'Loop block with setup and increment expression evalutes to Nil';
    is-deeply $count, 9, 'Loop with setup/increment runs as expected';
}

{
    my $count = 0;
    is-deeply
        EVAL(RakuAST::Statement::For.new(
            source => RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(2),
                infix => RakuAST::Infix.new('..'),
                right => RakuAST::IntLiteral.new(7)
            ),
            body => RakuAST::PointyBlock.new(
                signature => RakuAST::Signature.new(
                    parameters => (
                        RakuAST::Parameter.new(
                            target => RakuAST::ParameterTarget::Var.new('$x')
                        ),
                    )
                ),
                body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::ApplyPrefix.new(
                                prefix => RakuAST::Prefix.new('++'),
                                operand => RakuAST::Var::Lexical.new('$count')))))))),
        Nil,
        'Statement level for loop evalutes to Nil';
    is-deeply $count, 6, 'For loop does expected number of iterations';
}

{
    my $total = 0;
    EVAL(RakuAST::Statement::For.new(
        source => RakuAST::ApplyInfix.new(
            left => RakuAST::IntLiteral.new(2),
            infix => RakuAST::Infix.new('..'),
            right => RakuAST::IntLiteral.new(7)
        ),
        body => RakuAST::PointyBlock.new(
            signature => RakuAST::Signature.new(
                parameters => (
                    RakuAST::Parameter.new(
                        target => RakuAST::ParameterTarget::Var.new('$x')
                    ),
                )
            ),
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$total'),
                            infix => RakuAST::Infix.new('='),
                            right => RakuAST::ApplyInfix.new(
                                left => RakuAST::Var::Lexical.new('$total'),
                                infix => RakuAST::Infix.new('+'),
                                right => RakuAST::Var::Lexical.new('$x')))))))));
    is-deeply $total, (2..7).sum, 'For loop puts correct value into explicit iteration variable';
}

{
    my $total = 0;
    is-deeply
        EVAL(RakuAST::Statement::For.new(
            source => RakuAST::ApplyInfix.new(
                left => RakuAST::IntLiteral.new(2),
                infix => RakuAST::Infix.new('..'),
                right => RakuAST::IntLiteral.new(7)
            ),
            body => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::ApplyInfix.new(
                                left => RakuAST::Var::Lexical.new('$total'),
                                infix => RakuAST::Infix.new('='),
                                right => RakuAST::ApplyInfix.new(
                                    left => RakuAST::Var::Lexical.new('$total'),
                                    infix => RakuAST::Infix.new('+'),
                                    right => RakuAST::Var::Lexical.new('$_'))))))))),
        Nil,
        'Statement level for loop with implicit topic evalutes to Nil';
    is-deeply $total, (2..7).sum, 'For loop puts correct value in imlicit topic $_';
}

{
    my $ast = RakuAST::Statement::Given.new(
        source => RakuAST::Var::Lexical.new('$a'),
        body => RakuAST::PointyBlock.new(
            signature => RakuAST::Signature.new(
                parameters => (
                    RakuAST::Parameter.new(
                        target => RakuAST::ParameterTarget::Var.new('$x')
                    ),
                )
            ),
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::Var::Lexical.new('$x')
                    ))))
    );
    my $a = 'concrete';
    is-deeply EVAL($ast), 'concrete', 'given topicalizes on the source (signature)';
    $a = Str;
    is-deeply EVAL($ast), Str, 'given topicalizes even an undefined source (signature)';
}

{
    my $ast = RakuAST::Statement::Given.new(
        source => RakuAST::Var::Lexical.new('$a'),
        body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        RakuAST::Var::Lexical.new('$_')
                    ))))
    );
    my $a = 'concrete';
    is-deeply EVAL($ast), 'concrete', 'given topicalizes on the source (implicit $_)';
    $a = Str;
    is-deeply EVAL($ast), Str, 'given topicalizes even an undefined source (implicit $_)';
}

{
    my $ast = RakuAST::Statement::Given.new(
        source => RakuAST::Var::Lexical.new('$a'),
        body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::When.new(
                        condition => RakuAST::IntLiteral.new(2),
                        body => RakuAST::Block.new(
                            body => RakuAST::Blockoid.new(
                                RakuAST::StatementList.new(
                                    RakuAST::Statement::Expression.new(
                                        RakuAST::StrLiteral.new('two')
                    ))))),
                    RakuAST::Statement::When.new(
                        condition => RakuAST::IntLiteral.new(3),
                        body => RakuAST::Block.new(
                            body => RakuAST::Blockoid.new(
                                RakuAST::StatementList.new(
                                    RakuAST::Statement::Expression.new(
                                        RakuAST::StrLiteral.new('three')
                    ))))),
                    RakuAST::Statement::Default.new(
                        body => RakuAST::Block.new(
                            body => RakuAST::Blockoid.new(
                                RakuAST::StatementList.new(
                                    RakuAST::Statement::Expression.new(
                                        RakuAST::StrLiteral.new('another')
                    )))))
    ))));

    my $a = 2;
    is-deeply EVAL($ast), 'two', 'First when statement matching gives correct result';
    $a = 3;
    is-deeply EVAL($ast), 'three', 'Second when statement matching gives correct result';
    $a = 4;
    is-deeply EVAL($ast), 'another', 'No when statement matching gives default';
}

{
    my $handled = False;
    is-deeply EVAL(RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('die'),
                    args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('oops'))
            )),
            RakuAST::Statement::Catch.new(body => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                    RakuAST::Statement::Default.new(body => RakuAST::Block.new(
                        body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::ApplyPostfix.new(
                                operand => RakuAST::Var::Lexical.new('$handled'),
                                postfix => RakuAST::Postfix.new('++')
                        )))))
            )))))
        )))),
        Nil,
        'Block with CATCH/default handles exception and evalutes to Nil';
    ok $handled, 'The exception handler ran';
    is $!, 'oops', '$! in the outer scope has the exception';
}

throws-like
    {
        EVAL(RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('die'),
                    args => RakuAST::ArgList.new(RakuAST::StrLiteral.new('gosh'))
            )),
            RakuAST::Statement::Catch.new(body => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(RakuAST::StatementList.new()
            )))
        ))));
    },
    X::AdHoc,
    message => /gosh/,
    'Exception is rethrown if unhandled';
