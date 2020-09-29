use MONKEY-SEE-NO-EVAL;
use Test;

plan 12;

my $ast;

subtest 'Default type for block and routine' => {
    # sub ($param) { }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('$param'),
          ),
        )
      )
    );

    ok .signature.params[0].type =:= Any, 'Default type of sub'
      for EVAL($ast), EVAL($ast.DEPARSE);

    # -> $param { }
    $ast := RakuAST::PointyBlock.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('$param'),
          ),
        )
      )
    );
    ok .signature.params[0].type =:= Mu, 'Default type of block'
      for EVAL($ast), EVAL($ast.DEPARSE);
}

subtest 'Type constraint is enforced on Scalar' => {
    # sub (Int $x) { }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('Int')
            ),
            target => RakuAST::ParameterTarget::Var.new('$x'),
          ),
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].type =:= Int,
          "$type: Specified type is introspectable";
        lives-ok { $sub(42) },
          "$type: Passing correct type lives";
        dies-ok { $sub("foo") },
          "$type: Passing wrong type dies";
    }
}

subtest 'Anonymous parameter still enforces type constraint' => {
    # sub (Int) { }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('Int')
            )
          ),
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].type =:= Int,
          "$type: Specified type is introspectable";
        lives-ok { $sub(42) },
          "$type: Passing correct type lives";
        dies-ok { $sub("foo") },
          "$type: Passing wrong type dies";
    }
}

subtest 'Optional Scalar parameter defaults to type object' => {
    # sub (Int $x?) { $x }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('Int')
            ),
            target => RakuAST::ParameterTarget::Var.new('$x'),
            optional => True,
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$x')
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].optional,
          "$type: Parameter introspects as optional";
        is $sub.arity, 0,
          "$type: Correct arity";
        is $sub.count, 1,
          "$type: Correct count";
        is-deeply $sub(66), 66,
          "$type: Passing an argument gets the value";
        is-deeply $sub(), Int,
          "$type: Passing no argument gets the type object";
    }
}

subtest 'One required named parameter' => { 
    # sub (:$named!) { $named }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('$named'),
            names => ['named']
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$named')
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        is $sub.signature.params.elems, 1,
          "$type: Sub has one params elem";
        is-deeply $sub.arity, 0,
          "$type: The block has 0 arity";
        is-deeply $sub.count, 0,
          "$type: The block has 0 count";
        given $sub.signature.params[0] {
            is-deeply .name, '$named',
              "$type: Correct variable name";
            is-deeply .named_names, ('named',),
              "$type: Correct named name";
            nok .optional,
              "$type: It is not optional";
        }
        is $sub(named => 99), 99,
          "$type: Invoking the sub with a named argument works";
        dies-ok { $sub() },
          "$type: Invoking the sub without an argument dies";
    }
}

subtest 'Required named parameter with alias' => { 
    # sub (:fst(:first($var))!) { $var }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('$var'),
            names => ['first', 'fst']
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('$var')
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        is $sub.signature.params.elems, 1,
          "$type: Sub has one params elem";
        is-deeply $sub.arity, 0,
          "$type: The block has 0 arity";
        is-deeply $sub.count, 0,
          "$type: The block has 0 count";
        given $sub.signature.params[0] {
            is-deeply .name, '$var',
              "$type: Correct variable name";
            is-deeply .named_names.sort, ('first','fst'),
              "$type: Correct named names";
            nok .optional,
              "$type: It is not optional";
        }
        is $sub(first => 33), 33,
          "$type: Invoking the sub with first alias works";
        is $sub(fst => 44), 44,
          "$type: Invoking the sub with second alias works";
        dies-ok { $sub() },
          "$type: Invoking the sub without an argument dies";
        dies-ok { $sub(var => 99) },
          "$type: Invoking the sub with non-named name dies";
    }
}

subtest 'Slurpy hash parameter' => {
    # sub (*%h) { %h }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('%h'),
            slurpy => RakuAST::Parameter::Slurpy::Flattened
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('%h')
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].slurpy,
          "$type: Parameter introspects as slurpy";
        ok $sub.signature.params[0].named,
          "$type: Parameter introspects as named";
        is $sub.arity, 0,
          "$type: Correct arity";
        is $sub.count, 0,
          "$type: Correct count";
        is-deeply $sub(), {},
          "$type: Passing no argument gets empty hash";
        is-deeply $sub(a => 1), {a => 1},
          "$type: Passing one named argument has correct hash";
        is-deeply $sub(a => 1, b => 2), {a => 1, b => 2},
          "$type: Passing two named arguments has correct hash";
    }
}

subtest 'Slurpy flattening array parameter' => {
    # sub (*@a) { @a }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('@a'),
            slurpy => RakuAST::Parameter::Slurpy::Flattened
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('@a')
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].slurpy,
          "$type: Parameter introspects as slurpy";
        nok $sub.signature.params[0].named,
          "$type: Parameter does not introspect as named";
        is $sub.arity, 0,
          "$type: Correct arity";
        is $sub.count, Inf,
          "$type: Correct count";
        is-deeply $sub(), [],
          "$type: Passing no argument gets empty array";
        is-deeply $sub(1), [1],
          "$type: Passing one argument has correct array";
        is-deeply $sub(1, 2), [1, 2],
          "$type: Passing two arguments has correct array";
        is-deeply $sub(1, 2, (3, 4)), [1, 2, 3, 4],
          "$type: Flattening happens";
    }
}

subtest 'Slurpy non-flattening array parameter' => {
    # sub (**@a) { @a }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('@a'),
            slurpy => RakuAST::Parameter::Slurpy::Unflattened
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('@a')
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].slurpy,
          "$type: Parameter introspects as slurpy";
        nok $sub.signature.params[0].named,
          "$type: Parameter does not introspect as named";
        is $sub.arity, 0,
          "$type: Correct arity";
        is $sub.count, Inf,
          "$type: Correct count";
        is-deeply $sub(), [],
          "$type: Passing no argument gets empty array";
        is-deeply $sub(1), [1],
          "$type: Passing one argument has correct array";
        is-deeply $sub(1, 2), [1, 2],
          "$type: Passing two arguments has correct array";
        is-deeply $sub(1, 2, (3, 4)), [1, 2, (3, 4)],
          "$type: Flattening does not happen";
        is-deeply $sub((3, 4)), [(3, 4),],
          "$type: Passing a list results in one-element array with the list";
    }
}

subtest 'Slurpy single arg rule array parameter' => {
    # sub (+@a) { @a }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Var.new('@a'),
            slurpy => RakuAST::Parameter::Slurpy::SingleArgument
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Var::Lexical.new('@a')
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].slurpy,
          "$type: Parameter introspects as slurpy";
        nok $sub.signature.params[0].named,
          "$type: Parameter does not introspect as named";
        is $sub.arity, 0,
          "$type: Correct arity";
        is $sub.count, Inf,
          "$type: Correct count";
        is-deeply $sub(), [],
          "$type: Passing no argument gets empty array";
        is-deeply $sub(1), [1],
          "$type: Passing one argument has correct array";
        is-deeply $sub(1, 2), [1, 2],
          "$type: Passing two arguments has correct array";
        is-deeply $sub(1, 2, (3, 4)), [1, 2, (3, 4)],
          "$type: Flattening does not happen";
        is-deeply $sub((3, 4)), [3, 4],
          "$type: Passing a list is treated as the single argument";
    }
}

subtest 'Sigilless parameter' => {
    # sub (Int \trm) { term }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier('Int')
            ),
            target => RakuAST::ParameterTarget::Term.new(RakuAST::Name.from-identifier('trm')),
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Term::Name.new(RakuAST::Name.from-identifier('trm'))
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].type =:= Int,
          "$type: Type is introspectable";
        is $sub.signature.params[0].name, 'trm',
          "$type: The name matches";
        is $sub(42), 42,
          "$type: The term can be resolved in the sub";
        dies-ok { $sub("foo") },
          "$type: Passing wrong type dies";
    }
}

subtest 'Capture parameter' => {
    # sub (|cappy) { cappy }
    $ast := RakuAST::Sub.new(
      signature => RakuAST::Signature.new(
        parameters => (
          RakuAST::Parameter.new(
            target => RakuAST::ParameterTarget::Term.new(RakuAST::Name.from-identifier('c')),
            slurpy => RakuAST::Parameter::Slurpy::Capture
          ),
        )
      ),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::Term::Name.new(RakuAST::Name.from-identifier('c'))
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $sub {
        ok $sub.signature.params[0].capture,
          "$type: Parameter introspects as a capture";
        is $sub.arity, 0,
          "$type: Correct arity";
        is $sub.count, Inf,
          "$type: Correct count";
        is-deeply $sub(), \(),
          "$type: Passing no argument gets empty capture";
        is-deeply $sub(1, 2), \(1,2),
          "$type: Passing positional arguments gets correct capture";
        is-deeply $sub(1, 2, (3, 4)), \(1, 2, (3, 4)),
          "$type: Flattening does not happen";
        is-deeply $sub(:b, :!b), \(b => True, b => False),
          "$type: Passing named arguments gets correct capture";
        is-deeply $sub("foo", :bar<baz>), \("foo", bar => 'baz'),
          "$type: Passing a mix of positional and named arguments gets correct capture";
    }
}

# vim: expandtab shiftwidth=4
