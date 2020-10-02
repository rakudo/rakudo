use MONKEY-SEE-NO-EVAL;
use Test;

plan 11;

my $ast;
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'A pointy block node evaluates to a Block' => {
    # -> () { 101 }
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::PointyBlock.new(
          signature => RakuAST::Signature.new(
            parameters => ()
          ),
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(101)
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $block {
        ok $block.WHAT === Block,
          "$type: A pointy block node evaluates to a Block";
        is $block.signature.params.elems, 0,
          "$type: The block has no parameters";
        is-deeply $block.arity, 0,
          "$type: The block has 0 arity";
        is-deeply $block.count, 0,
          "$type: The block has 0 count";
        is $block(), 101,
          "$type: Invoking the block returns the expected value";
        dies-ok { $block(1) },
          "$type: Invoking the block with an argument dies";
    }
}

subtest 'A pointy block node taking a parameter evaluates to a Block' => {
    # -> $param { $param };
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::PointyBlock.new(
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new('$param')
              ),
            )
          ),
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new('$param')
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $block {
        ok $block.WHAT === Block,
          "$type: A pointy block node taking a parameter evaluates to a Block";
        is $block.signature.params.elems, 1,
          "$type: The block has one parameters";
        is-deeply $block.arity, 1,
          "$type: The block has 1 arity";
        is-deeply $block.count, 1,
          "$type: The block has 1 count";
        is $block(199), 199,
          "$type: Invoking the block with an argument returns the expected value";
        dies-ok { $block(my $a = 42) = 1 },
          "$type: Argument is bound read-only";
        dies-ok { $block() },
          "$type: Invoking the block without an argument dies";
    }
}

subtest 'Bare block at statement level is executed' => {
    my $x = 99;

    # { $x++ };
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyPostfix.new(
                  operand => RakuAST::Var::Lexical.new('$x'),
                  postfix => RakuAST::Postfix.new('++')
                )
              )
            )
          )
        )
      )
    );

    is-deeply EVAL($ast), 99,
      'AST: Bare block at statement level is executed';
    is-deeply $x, 100,
      'AST: Side-effects were performed as expected';

    is-deeply EVAL($ast.DEPARSE), 100,
      'DEPARSE: Bare block at statement level is executed';
    is-deeply $x, 101,
      'DEPARSE: Side-effects were performed as expected';
}

subtest 'Bare block in parentheses evaluates to Block' => {
    my $x = 99;

    # ({ $x++ })
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Circumfix::Parentheses.new(
          RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::ApplyPostfix.new(
                        operand => RakuAST::Var::Lexical.new('$x'),
                        postfix => RakuAST::Postfix.new('++')
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    );

    given EVAL($ast) -> $result {
        is-deeply $result.WHAT, Block,
          'AST: Bare block in parentheses evaluates to Block';
        is $result.arity, 0,
          'AST: Block has arity 0';
        is $result.count, 1,
          'AST: Block has count 1';
        is-deeply $x, 99,
          'AST: No side-effects were performed';
        is-deeply $result(), 99,
          'AST: Can evaluate the returned block';
        is-deeply $x, 100,
          'AST: Block did perform side-effects when evaluated';
    }

    given EVAL($ast.DEPARSE) -> $result {
        is-deeply $result.WHAT, Block,
          'DEPARSE: Bare block in parentheses evaluates to Block';
        is $result.arity, 0,
          'DEPARSE: Block has arity 0';
        is $result.count, 1,
          'DEPARSE: Block has count 1';
        is-deeply $x, 100,
          'DEPARSE: No side-effects were performed';
        is-deeply $result(), 100,
          'DEPARSE: Can evaluate the returned block';
        is-deeply $x, 101,
          'DEPARSE: Block did perform side-effects when evaluated';
    }
}

subtest 'Block has default parameter' => {
    # ({ $_ })
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Circumfix::Parentheses.new(
          RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      expression => RakuAST::Var::Lexical.new('$_')
                    ),
                  )
                )
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $result {
        is-deeply $result('xxx'), 'xxx',
          "$type: Block has default $type parameter";
        lives-ok { $result() },
          "$type: That $type parameter is optional";
    }
}

subtest 'A sub node evaluates to a Sub' => {
    # sub ($param) { $param };
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new('$param')
              ),
            )
          ),
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new('$param')
              ),
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.WHAT === Sub,
          "$type: A sub node evaluates to a Sub";
        is $sub.signature.params.elems, 1,
          "$type: The sub has one parameter";
        is-deeply $sub.arity, 1,
          "$type: The block has 1 arity";
        is-deeply $sub.count, 1,
          "$type: The block has 1 count";
        is $sub(189), 189,
          "$type: Invoking the sub with an argument returns the expected value";
        dies-ok { $sub() },
          "$type: Invoking the sub without an argument dies";
    }
}

subtest 'Can call a named sub declaration' => {
    # sub my-sub($param) { $param }; my-sub(66)
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name => RakuAST::Name.from-identifier('my-sub'),
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new('$param')
              ),
            )
          ),
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new('$param')
              ),
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier('my-sub'),
          args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(66),
          )
        )
      )
    );

    is-deeply $_, 66
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'A routine declared anonymous does not declare anything' => {
    # anon sub my-sub() { 66 }; my-sub()
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          scope => 'anon',
          name => RakuAST::Name.from-identifier('my-sub'),
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(66)
              ),
            )
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
          name => RakuAST::Name.from-identifier('my-sub')
        )
      )
    );

    dies-ok $_
      for { EVAL($ast) }, { EVAL($ast.DEPARSE) };
}

subtest 'A sub node with a trait evaluates to a Sub' => {
    # sub () returns Int { 66 }
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          traits => [
            RakuAST::Trait::Returns.new(
              RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Int')
              )
            )
          ],
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(66)
              )
            )
          )
        )
      )
    );

    my $sub = EVAL($ast);
    ok $sub ~~ Sub, 'A sub node with a trait evaluates to a Sub';
    is-deeply $sub.returns, Int, 'The returns trait was applied and .returns is correct';
    ok $sub ~~ Callable[Int], 'It also does the correct parametric Callable';
}

subtest 'Return type constraint' => {
    my $x;

    # sub () returns Int { $x }
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          traits => [
            RakuAST::Trait::Returns.new(
              RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Int')
              )
            )
          ],
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new('$x')
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        $x = 42;
        lives-ok { $sub() }, "$type: type matches";
        $x = 'oops';
        dies-ok { $sub() }, "$type: type does not match";
    }
}

subtest 'Using return with acceptable type works' => {
    my $x;

    # sub () returns Int { return $x }
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          traits => [
            RakuAST::Trait::Returns.new(
              RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Int')
              )
            )
          ],
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier('return'),
                  args => RakuAST::ArgList.new(
                    RakuAST::Var::Lexical.new('$x')
                  )
                )
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        $x = 42;
        lives-ok { $sub() }, "$type: type matches";
        $x = 'oops';
        dies-ok { $sub() }, "$type: type does not match";
    }
}

# vim: expandtab shiftwidth=4
