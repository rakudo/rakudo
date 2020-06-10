use MONKEY-SEE-NO-EVAL;
use Test;

plan 17;

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
            setup => RakuAST::Declaration::Var.new(
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
