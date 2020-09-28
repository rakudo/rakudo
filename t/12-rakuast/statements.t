use MONKEY-SEE-NO-EVAL;
use Test;

plan 34; # Do not change this file to done-testing

my $ast;

subtest 'Statement list evaluates to its final statement' => {
    my $x = 12;
    my $y = 99;

    # ++$x; ++$y
    $ast := RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        RakuAST::ApplyPrefix.new(
          prefix => RakuAST::Prefix.new('++'),
          operand => RakuAST::Var::Lexical.new('$x')
        )
      ),
      RakuAST::Statement::Expression.new(
        RakuAST::ApplyPrefix.new(
          prefix => RakuAST::Prefix.new('++'),
          operand => RakuAST::Var::Lexical.new('$y')
        )
      )
    );

    is-deeply EVAL($ast), 100,
      'AST: Statement list evaluates to its final statement';
    is $x, 13,
      'AST: First side-effecting statement was executed';
    is $y, 100,
      'AST: Second side-effecting statement was executed';

    is-deeply EVAL($ast.DEPARSE), 101,
      'DEPARSE: Statement list evaluates to its final statement';
    is $x, 14,
      'DEPARSE: First side-effecting statement was executed';
    is $y, 101,
      'DEPARSE: Second side-effecting statement was executed';
}

subtest 'Basic if / elsif / else structure' => {
    my ($a, $b, $c);

    # if $a { 1 }
    # elsif $b { 2 }
    # elsif $c { 3 }
    # else { 4 }
    $ast := RakuAST::Statement::If.new(
      condition => RakuAST::Var::Lexical.new('$a'),
      then => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::IntLiteral.new(1)
            )
          )
        )
      ),
      elsifs => [
        RakuAST::Statement::Elsif.new(
          condition => RakuAST::Var::Lexical.new('$b'),
          then => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  RakuAST::IntLiteral.new(2)
                )
              )
            )
          )
        ),
        RakuAST::Statement::Elsif.new(
          condition => RakuAST::Var::Lexical.new('$c'),
          then => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  RakuAST::IntLiteral.new(3)
                )
              )
            )
          )
        )
      ],
      else => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::IntLiteral.new(4)
            )
          )
        )
      )
    );

    for 'AST', 'DEPARSE' -> $type {
        $a = $b = $c = False;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 4,
          "$type: When all conditions False, else is evaluated";

        $c = True;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 3,
          "$type: Latest elsif reachable when matched";

        $b = True;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 2,
          "$type: First elsif reachable when matched";

        $a = True;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 1,
          "$type: When the main condition is true, the then block is picked";
    }
}

subtest 'simple if evaluation' => {
    my $a;

    # if $a { 1 }
    $ast := RakuAST::Statement::If.new(
      condition => RakuAST::Var::Lexical.new('$a'),
      then => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::IntLiteral.new(1)
            )
          )
        )
      )
    );

    for 'AST', 'DEPARSE' -> $type {
        $a = True;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 1,
          "$type: When simple if with no else has true condition";

        $a = False;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), Empty,
          "$type: When simple if with no else has false condition";
    }
}

subtest 'Basic with / orwith / else structure' => {
    my ($a, $b, $c);

    # with $a -> $x { 1 }
    # orwith -> $x { 2 }
    # orwith -> $x { 3 }
    # else -> $x { 4 }
    $ast := RakuAST::Statement::With.new(
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
            )
          )
        )
      ),
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
                )
              )
            )
          )
        ),
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
                )
              )
            )
          )
        )
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
            )
          )
        )
      )
    );

    for 'AST', 'DEPARSE' -> $type {
        $a = $b = $c = Nil;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 4,
          "$type: When all conditions undefined, else is evaluated";

        $c = False;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 3,
          "$type: Latest orwith reachable when matched";

        $b = False;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 2,
          "$type: First orwith reachable when matched";

        $a = False;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 1,
          "$type: When the main condition is defined, the then block is picked";
    }
}

subtest 'simple with evaluation' => {
    my $a;

    # with $a -> $x { 1 }
    $ast := RakuAST::Statement::With.new(
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
            )
          )
        )
      )
    );
    for 'AST', 'DEPARSE' -> $type {
        $a = False;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 1,
          "$type: When simple when with no else has defined condition";

        $a = Nil;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), Empty,
          "$type: When simple with if with no else has undefined condition";
    }
}

subtest 'with topicalizes in the body' => {
    # with $a { $_ } else { $_ }
    $ast := RakuAST::Statement::With.new(
      condition => RakuAST::Var::Lexical.new('$a'),
      then => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::Var::Lexical.new('$_')
            )
          )
        )
      ),
      else => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::Var::Lexical.new('$_')
            )
          )
        )
      )
    );
    for 'AST', 'DEPARSE' -> $type {
        my $a = 42;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), 42,
          "$type: with topicalizes in the body";

        $a = Int;
        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), Int,
          "$type: with topicalizes in the else body too";
    }
}

subtest 'simple unless with a false condition' => {
    my $x = False;
    my $y = 9;

    # unless $x { ++$y }
    $ast := RakuAST::Statement::Unless.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('++'),
                operand => RakuAST::Var::Lexical.new('$y')
              )
            )
          )
        )
      )
    );

    is-deeply EVAL($ast), 10,
      'AST: unless block with a false condition evaluates to its body';
    is $y, 10, 'AST: Side-effect of the body was performed';

    is-deeply EVAL($ast.DEPARSE), 11,
      'DEPARSE: unless block with a false condition evaluates to its body';
    is $y, 11, 'DEPARSE: Side-effect of the body was performed';
}

subtest 'simple unless with a false condition' => {
    my $x = True;
    my $y = 9;

    # unless $x { ++$y }
    $ast := RakuAST::Statement::Unless.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPrefix.new(
                prefix  => RakuAST::Prefix.new('++'),
                operand => RakuAST::Var::Lexical.new('$y')
              )
            )
          )
        )
      )
    );

    is-deeply EVAL($ast), Empty,
      'AST: unless block with a false condition evaluates to Empty';
    is $y, 9, 'AST: Side-effect of the body was not performed';

    is-deeply EVAL($ast.DEPARSE), Empty,
      'DEPARSE: unless block with a false condition evaluates to Empty';
    is $y, 9, 'DEPARSE: Side-effect of the body was not performed';
}

subtest 'simple without with an undefined condition' => {
    my $x = Nil;
    my $y = 9;

    # without $x { $y++ }
    $ast := RakuAST::Statement::Without.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPostfix.new(
                postfix => RakuAST::Postfix.new('++'),
                operand => RakuAST::Var::Lexical.new('$y')
              )
            )
          )
        )
      )
    );

    is-deeply EVAL($ast), 9,
      'AST: without block with an undefined object evaluates to its body';
    is $y, 10, 'AST: Side-effect of the body was performed';

    is-deeply EVAL($ast.DEPARSE), 10,
      'DEPARSE: without block with an undefined object evaluates to its body';
    is $y, 11, 'DEPARSE: Side-effect of the body was performed';
}

subtest 'simple without with a defined condition' => {
    my $x = True;
    my $y = 9;

    # without $x { ++$y }
    $ast := RakuAST::Statement::Without.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('++'),
                operand => RakuAST::Var::Lexical.new('$y')
              )
            )
          )
        )
      )
    );

    is-deeply EVAL($ast), Empty,
      'AST: An without block with a defined object evaluates to Empty';
    is $y, 9, 'AST: Side-effect of the body was not performed';

    is-deeply EVAL($ast.DEPARSE), Empty,
      'DEPARSE: An without block with a defined object evaluates to Empty';
    is $y, 9, 'DEPARSE: Side-effect of the body was not performed';
}

subtest 'simple without with an undefined condition' => {
    my $x = Cool;

    # without $x { $_ }
    $ast := RakuAST::Statement::Without.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(body =>
        RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::Var::Lexical.new('$_')
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $result {
        is-deeply $result, Cool,
          "$type: without block sets the topic";
    }
}

subtest 'While loop at statement level evaluates to Nil' => {
    my $x;

    # while $x { --$x }
    $ast := RakuAST::Statement::Loop::While.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('--'),
                operand => RakuAST::Var::Lexical.new('$x')
              )
            )
          )
        )
      )
    );

    for 'AST', 'DEPARSE' -> $type {
        $x = 5;

        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), Nil,
          "$type: while loop at statement level evaluates to Nil";
        is-deeply $x, 0, "$type: Loop variable was decremented to zero";
    }
}

subtest 'Until loop at statement level evaluates to Nil' => {
    my $x;

    # until !$x { --$x }
    $ast := RakuAST::Statement::Loop::Until.new(
      condition => RakuAST::ApplyPrefix.new(
        prefix => RakuAST::Prefix.new('!'),
        operand => RakuAST::Var::Lexical.new('$x')
      ),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('--'),
                operand => RakuAST::Var::Lexical.new('$x')
              )
            )
          )
        )
      )
    );

    for 'AST', 'DEPARSE' -> $type {
        $x = 5;

        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), Nil,
          "$type: until loop at statement level evaluates to Nil";
        is-deeply $x, 0, "$type: Loop variable was decremented to zero";
    }
}

subtest 'Repeat while loop at statement level evaluates to Nil' => {
    my $x;

    # repeat { --$x } while $x
    $ast:= RakuAST::Statement::Loop::RepeatWhile.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('--'),
                operand => RakuAST::Var::Lexical.new('$x')
              )
            )
          )
        )
      )
    );

    for 'AST', 'DEPARSE' -> $type {
        $x = 5;

        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), Nil,
          "$type: repeat until loop at statement level evaluates to Nil";
        is-deeply $x, 0, "$type: loop variable decremented to 0";
    }
}

subtest 'Repeat until loop at statement level evaluates to Nil' => {
    my $x;

    # repeat { --$x } until $x
    $ast:= RakuAST::Statement::Loop::RepeatUntil.new(
      condition => RakuAST::Var::Lexical.new('$x'),
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::ApplyPrefix.new(
                prefix => RakuAST::Prefix.new('--'),
                operand => RakuAST::Var::Lexical.new('$x')
              )
            )
          )
        )
      )
    );

    for 'AST', 'DEPARSE' -> $type {
        $x = 0;

        is-deeply EVAL($type eq 'AST' ?? $ast !! $ast.DEPARSE), Nil,
          "$type: repeat until loop at statement level evaluates to Nil";
        is-deeply $x, -1, "$type: loop ran once";
    }
}

{  # loop (my $i = 9; $i; --$i) { ++$count }
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

{  # for 2 .. 7 -> $x { ++$count }
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

{  # for 2 .. 7 -> $x { $total = $total + $x }
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

{  # for 2 .. 7 { $total = $total + $_ }
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

{  # given $a -> $x { $x }
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

{  # given $a { $_ }
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

{  # given $a { when 2 { "two" } when 3 { "three" } default { "another" } }
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

{  # { die "oops"; CATCH { $handled++ } }
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
    {  # { die "gosh"; CATCH { } }
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

# This test calls an imported `&ok` to check the `use` works; the test plan
# verifies that it really works.
{
    sub ok(|) { die "Imported ok was not used" };
    # use Test; ok 1, "use statement works"
    EVAL RakuAST::StatementList.new(
        RakuAST::Statement::Use.new(
            module-name => RakuAST::Name.from-identifier('Test')
        ),
        RakuAST::Statement::Expression.new(
            RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('ok'),
                args => RakuAST::ArgList.new(
                    RakuAST::IntLiteral.new(1),
                    RakuAST::StrLiteral.new('use statements works')
                )
        ))
    );
}

# vim: expandtab shiftwidth=4
