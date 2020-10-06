use MONKEY-SEE-NO-EVAL;
use Test;

plan 35;

my $ast;   # so we don't need to repeat the "my" all the time
sub ast(RakuAST::Node:D $node --> Nil) {
    $ast := $node;
    diag $ast.DEPARSE.chomp;
}

subtest 'Lexical variable lookup ($ sigil)' => {
    my $x = 42;

    # $x
    ast RakuAST::Var::Lexical.new('$x');
    is-deeply $_, 42
      for EVAL($ast), try EVAL($ast.DEPARSE);
}

subtest 'Lexical variable lookup (& sigil)' => {

    # &plan
    ast RakuAST::Var::Lexical.new('&plan');
    is-deeply $_, &plan
      for EVAL($ast), try EVAL($ast.DEPARSE);
}

subtest 'Positional capture variable lookup works' => {
    my $/;
    "abc" ~~ /(.)(.)/;

    # $0
    ast RakuAST::Var::PositionalCapture.new(0);
    is-deeply $_, "a"
      for EVAL($ast).Str, try EVAL($ast.DEPARSE).Str;

    # $1
    ast RakuAST::Var::PositionalCapture.new(1);
    is-deeply $_, "b"
      for EVAL($ast).Str, try EVAL($ast.DEPARSE).Str;
}

subtest 'Named capture variable lookup works' => {
    my $/;
    "abc" ~~ /$<x>=(.)$<y>=(.)/;

    # $<y>
    ast RakuAST::Var::NamedCapture.new(
      RakuAST::QuotedString.new(
        segments => [RakuAST::StrLiteral.new('y')],
        processors => ['words','val']
      )
    );
    is-deeply $_, "b"
      for EVAL($ast).Str, try EVAL($ast.DEPARSE).Str;

    # $<x>
    $ast := RakuAST::Var::NamedCapture.new(
      RakuAST::QuotedString.new(
        segments => [RakuAST::StrLiteral.new('x')],
        processors => ['words','val']
      )
    );
    is-deeply $_, "a"
      for EVAL($ast).Str, try EVAL($ast.DEPARSE).Str;
}

subtest 'variable declaration takes scope and name' => {
    for 'my', 'state' -> $scope {
        # my|state $foo
        ast RakuAST::VarDeclaration::Simple.new(
          scope => $scope,
          name  => '$foo'
        );

        is $ast.scope, $scope, "did we get scope: $scope";
        is $ast.name,  '$foo', 'did we get the right name';
    }
}

subtest 'Lexical variable my|state declarations work' => {
    for 'my', 'state' -> $scope {
        # my|state $foo = 10; $foo
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '$foo'
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
              left => RakuAST::Var::Lexical.new('$foo'),
              infix => RakuAST::Infix.new('='),
              right => RakuAST::IntLiteral.new(10)
            ),
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$foo')
          )
        );
        is-deeply $_, 10, $scope
          for EVAL($ast), EVAL($ast.DEPARSE);
    }
}

subtest 'Defaults of my|state untyped container' => {
    for 'my', 'state' -> $scope {
        # my|state $foo; $foo
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '$foo'
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$foo')
          )
        );

        for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
            is-deeply cont, Any, "$type: $scope default value is Any";
            ok cont.VAR.of =:= Mu, "$type: $scope Default constraint is Mu";
        }
    }
}

subtest 'Typed variable my|state declaration (type matches in assignment)' => {
    for 'my', 'state' -> $scope {
        # my|state Int $foo; $foo = 99; $foo
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '$foo',
              type  => RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Int')
              )
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
              left => RakuAST::Var::Lexical.new('$foo'),
              infix => RakuAST::Infix.new('='),
              right => RakuAST::IntLiteral.new(99)
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$foo')
          )
        );
        is-deeply $_, 99, $scope
          for EVAL($ast), EVAL($ast.DEPARSE);
    }
}

subtest 'Typed variable my|state declaration (type mismatch throws)' => {
    for 'my', 'state' -> $scope {
        # my|state Int $foo; $foo = 1e5; $foo
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '$foo',
              type  => RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Int')
              )
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
              left => RakuAST::Var::Lexical.new('$foo'),
              infix => RakuAST::Infix.new('='),
              right => RakuAST::NumLiteral.new(1e5)
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$foo')
          )
        );

        throws-like { EVAL($ast) },
          X::TypeCheck::Assignment,
          expected => Int,
          got      => 1e5
        ;
        todo 'string EVAL produces different error';
        throws-like { EVAL($ast.DEPARSE) },
          X::TypeCheck::Assignment,
          expected => Int,
          got      => 1e5
        ;
    }
}

subtest 'Lexical variable my|state declaration with assignment initializer' => {
    for 'my', 'state' -> $scope {
        # my|state $var = 125; $var
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope       => $scope,
              name        => '$var',
              initializer => RakuAST::Initializer::Assign.new(
                RakuAST::IntLiteral.new(125)
              )
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$var')
          )
        );

        for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \result {
            is-deeply result, 125,
              "$type: $scope variable declarations with assignment initializer";
            ok result.VAR.isa(Scalar),
              "$type: $scope was an assignment into a Scalar container";
            nok result.VAR.dynamic,
              "$type: $scope is not dynamic";
            lives-ok { result = 42 },
              "$type: $scope can update the container that was produced";
        }
    }
}

subtest 'Lexical my|state array declaration with assignment initializer' => {
    for 'my', 'state' -> $scope {
        # my|state @var = 22, 33; @var
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope       => $scope,
              name        => '@var',
              initializer => RakuAST::Initializer::Assign.new(
                RakuAST::ApplyListInfix.new(
                  infix => RakuAST::Infix.new(','),
                  operands => (
                    RakuAST::IntLiteral.new(22),
                    RakuAST::IntLiteral.new(33)
                  )
                )
              )
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('@var')
          ),
        );
        is-deeply $_, [22,33], $scope
          for EVAL($ast), EVAL($ast.DEPARSE);
    }
}

subtest 'Lexical my|state variable declarations with bind initializer' => {
    for 'my', 'state' -> $scope {
        # my|state $var := 225
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope       => $scope,
              name        => '$var',
              initializer => RakuAST::Initializer::Bind.new(
                RakuAST::IntLiteral.new(225)
              )
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$var')
          ),
        );

        for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \result {
            # bug in rakudo main branch, "state $var := 225" is Mu
            next if $scope eq 'state' && $type eq 'DEPARSE';

            is-deeply result, 225,
              "$type: $scope variable declarations with bind initializer";
            nok result.VAR.isa(Scalar),
              "$type: $scope really was bound; no Scalar container";
            dies-ok { result = 42 },
              "$type: $scope cannot assign as it is not a container";
        }
    }
}

subtest 'Dynamic variable access' => {
    sub with-dyn(&test) {
        my $*dyn = 'in';
    }
    my $*dyn = 'out';

    # $*dyn
    ast RakuAST::Var::Dynamic.new('$*dyn');
    is-deeply $_, 'out', 'access outside'
      for EVAL($ast), EVAL($ast.DEPARSE);

    is-deeply $_, 'in', 'access inside'
      for with-dyn({ EVAL($ast) }), with-dyn({ EVAL($ast.DEPARSE) });

    # $*OUT
    ast RakuAST::Var::Dynamic.new('$*OUT');
    is-deeply $_, $*OUT, 'checking $*OUT'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Dynamic variable declaration and assignment, dynamic lookup' => {
    # my $*var = 360;
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          name => '$*var',
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::IntLiteral.new(360)
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Dynamic.new('$*var')
      ),
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \result {
        is-deeply result, 360,
          "$type: Dynamic variable declaration and assignment, dynamic lookup";
        ok result.VAR.isa(Scalar),
          "$type: Dynamic did an assignment into a Scalar container";
        ok result.VAR.dynamic,
          "$type: Is a dynamic";
        lives-ok { result = 99 },
          "$type: Can update the container that was produced";
    }
}

subtest '@ sigil my|state var is initialized to Array' => {
    for 'my', 'state' -> $scope {
        # my|state @arr; @arr
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '@arr'
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('@arr')
          ),
        );

        for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
            is-deeply cont.WHAT, Array,
              "$type: $scope @ sigil var is initialized to Array";
            is-deeply cont.VAR.WHAT, Array,
              "$type: $scope @ sigil var not wrapped in Scalar";
            ok cont.defined,
              "$type: $scope it is a defined Array instance";
            is cont.elems, 0,
              "$type: $scope it is empty";
            is-deeply cont[0].VAR.WHAT, Scalar,
              "$type: $scope element is a Scalar";
            is-deeply cont[0], Any,
              "$type: $scope contains an Any by default";
            ok cont[0].VAR.of =:= Mu,
              "$type: $scope constraint is Mu by default";
        }
    }
}

subtest '% sigil my|state var is initialized to Hash' => {
    for 'my', 'state' -> $scope {
        # my|state %hash; %hash
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '%hash'
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('%hash')
          ),
        );

        for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
            is-deeply cont.WHAT, Hash,
              "$type: $scope % sigil var is initialized to Hash";
            is-deeply cont.VAR.WHAT, Hash,
              "$type: $scope % sigil var not wrapped in Scalar";
            ok cont.defined,
              "$type: $scope it is a defined Hash instance";
            is cont.elems, 0,
              "$type: $scope it is empty";
            is-deeply cont<k>.VAR.WHAT, Scalar,
              "$type: $scope element is a Scalar";
            is-deeply cont<k>, Any,
              "$type: $scope contains an Any by default";
            ok cont<k>.VAR.of =:= Mu,
              "$type: $scope constraint is Mu by default";
        }
    }
}

subtest '@ sigil my|state var with Int type is an Array' => {
    for 'my', 'state' -> $scope {
        # my|state Int @arr; @arr
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '@arr',
              type  => RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Int')
              )
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('@arr')
          ),
        );

        for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
            ok cont ~~ Array,
              "$type: $scope @ sigil var with Int type is an Array";
            ok cont ~~ Positional[Int],
              "$type: $scope it does Positional[Int]";
            is-deeply cont.of, Int,
              "$type: $scope .of gives Int";
            is cont.elems, 0,
              "$type: $scope it is empty";
            is-deeply cont[0].VAR.WHAT, Scalar,
              "$type: $scope element is a Scalar";
            is-deeply cont[0], Int,
              "$type: $scope contains an Int";
            ok cont[0].VAR.of =:= Int,
              "$type: $scope constraint is Int";
        }
    }
}

subtest '% sigil my|state var with Int type is a Hash' => {
    for 'my', 'state' -> $scope {
        # my|state Int %hash; %hash
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => $scope,
              name  => '%hash',
              type  => RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Int')
              )
            )
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('%hash')
          )
        );

        for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
            ok cont ~~ Hash,
              "$type: $scope % sigil var with Int type is a Hash";
            ok cont ~~ Associative[Int],
              "$type: $scope it does Associative[Int]";
            is-deeply cont.of, Int,
              "$type: $scope .of gives Int";
            is cont.elems, 0,
              "$type: $scope it is empty";
            is-deeply cont<k>.VAR.WHAT, Scalar,
              "$type: $scope element is a Scalar";
            is-deeply cont<k>, Int,
              "$type: $scope contains an Int";
            ok cont<k>.VAR.of =:= Int,
              "$type: $scope constraint is Int";
        }
    }
}

subtest 'Can access external native int var' => {
    my int $x = 42;

    # $x
    ast RakuAST::Var::Lexical.new('$x');
    is-deeply $_, 42
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Can access external native num var' => {
    my num $x = 4e2;

    # $x
    ast RakuAST::Var::Lexical.new('$x');
    is-deeply $_, 4e2
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Can access external native str var' => {
    my str $x = 'answer';

    # $x
    ast RakuAST::Var::Lexical.new('$x');
    is-deeply $_, 'answer'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'int declaration creates a native int container' => {
    # my int $native-int; $native-int
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          name => '$native-int',
          type => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('int')
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$native-int')
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
        my $desc = "$type: int declaration creates a native int container";
        multi check(int $x) { pass $desc }
        multi check($x) { flunk $desc }

        check(cont);
        is-deeply cont, 0,
          "$type: Native int initialized to 0 by default";
    }
}

subtest 'num declaration creates a native num container' => {
    # my num $native-num; $native-num
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          name => '$native-num',
          type => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('num')
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$native-num')
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
        my $desc = "$type: num declaration creates a native num container";
        multi check(num $x) { pass $desc }
        multi check($x) { flunk $desc }

        check(cont);
        is-deeply cont, 0e0,
          "$type: Native num initialized to 0e0 by default";
    }
}

subtest 'str declaration creates a native str container' => {
    # my str $native-str; $native-str
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          name => '$native-str',
          type => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('str')
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$native-str')
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, \cont {
        my $desc = "$type: str declaration creates a native str container";
        multi check(str $x) { pass $desc }
        multi check($x) { flunk $desc }

        check(cont);
        is-deeply cont, '',
          "$type: Native str initialized to empty string by default";
    }
}

subtest 'Native int assign initializer works' => {
    # my int $native-int = 963; $native-int
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          name => '$native-int',
          type => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('int')
          ),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::IntLiteral.new(963)
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$native-int')
      )
    );
    is-deeply $_, 963
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Native num assign initializer works' => {
    # my int $native-num = 963; $native-num
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          name => '$native-num',
          type => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('num')
          ),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::NumLiteral.new(96e3)
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$native-num')
      )
    );
    is-deeply $_, 96e3
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Native str assign initializer works' => {
    # my int $native-str = 'nine six three'; $native-str
    ast RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          name => '$native-str',
          type => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('str')
          ),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::StrLiteral.new('nine six three')
          )
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$native-str')
      )
    );
    is-deeply $_, 'nine six three'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

{
    my module M {
        our $var;

        # our $var; $var
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => 'our',
              name  => '$var',
            ),
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$var')
          ),
        );

        subtest 'AST: our-scoped variable declaration without initializer takes current value (eval mode)' => {
            $var = 66;
            is-deeply EVAL($ast), 66, 'eval AST';
            is-deeply $var, 66, 'Value intact after eval';
        }

        subtest 'DEPARSE: our-scoped variable declaration without initializer takes current value (eval mode)' => {
            $var = 99;
            is-deeply EVAL($ast), 99, 'eval DEPARSE';
            is-deeply $var, 99, 'Value intact after evaL';
        }

        # our $x = 42; $x
        ast RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
              scope => 'our',
              name => '$x',
              initializer => RakuAST::Initializer::Assign.new(
                RakuAST::IntLiteral.new(42)
              )
            ),
          ),
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$x')
          ),
        );

        subtest 'our-scoped variable declaration with initializer works (eval mode)' => {
            is-deeply $_, 42
              for EVAL($ast), EVAL($ast.DEPARSE);
        }

        # our $y = 99; $y
        ast RakuAST::CompUnit.new(
          :!eval,
          :comp-unit-name('TEST_1'),
          :statement-list(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::VarDeclaration::Simple.new(
                scope => 'our',
                name => '$y',
                initializer => RakuAST::Initializer::Assign.new(
                  RakuAST::IntLiteral.new(99)
                )
              ),
            ),
            RakuAST::Statement::Expression.new(
              expression => RakuAST::Var::Lexical.new('$y')
            )
          ))
        );

        # There is no equivalent string representation of a compilation unit
        # so we cannot actually test the DEPARSE of the ast, as that would
        # interfere with other tests.
        is-deeply EVAL($ast), 99,
          'our-scoped variable declaration with initializer works (top-level mode)';
    }
    is-deeply $M::x, 42, 'our variable set in eval mode is installed into the current package';
    ok $M::x.VAR ~~ Scalar, 'It is a bound scalar';
    nok M.WHO<$y>:exists, 'our-scoped variable declaration in top-level comp unit does not leak out';
}

subtest 'A pointy block node with a state variable' => {
    # -> () { state $foo = 42; $foo++ }
    ast RakuAST::PointyBlock.new(
      signature => RakuAST::Signature.new(
        parameters => ()
      ),
      body => RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
          expression => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::VarDeclaration::Simple.new(
                  scope       => 'state',
                  name        => '$foo',
                  initializer => RakuAST::Initializer::Assign.new(
                    RakuAST::IntLiteral.new(42)
                  )
                )
              ),
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyPostfix.new(
                  postfix => RakuAST::Postfix.new('++'),
                  operand => RakuAST::Var::Lexical.new('$foo')
                )
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $block {
        is $block(), 42, "$type: state variable initialized";
        todo("does not keep value in AST just yet") if $type eq 'AST';
        is $block(), 43, "$type: state variable kept value";
    }
}

# vim: expandtab shiftwidth=4
