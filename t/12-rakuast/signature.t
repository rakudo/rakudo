use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

subtest 'Default type for block and routine' => {
    # sub ($param) { }
    my $sub := EVAL RakuAST::Sub.new(
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$param'),
                ),
            )
        )
    );
    ok $sub.signature.params[0].type =:= Any, 'Default type of sub parameter is Any';

    # -> $param { }
    my $block := EVAL RakuAST::PointyBlock.new(
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$param'),
                ),
            )
        )
    );
    ok $block.signature.params[0].type =:= Mu, 'Default type of block parameter is Mu';
}

subtest 'Type constraint is enforced on Scalar' => {
    # sub (Int $x) { }
    my $sub := EVAL RakuAST::Sub.new(
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new(
                    type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
                    target => RakuAST::ParameterTarget::Var.new('$x'),
                ),
            )
        )
    );
    ok $sub.signature.params[0].type =:= Int, 'Specified type is introspectable';
    lives-ok { $sub(42) }, 'Passing correct type lives';
    dies-ok { $sub("foo") }, 'Passing wrong type dies';
}

subtest 'Anonymous parameter still enforces type constraint' => {
    # sub (Int) { }
    my $sub := EVAL RakuAST::Sub.new(
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new(
                    type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
                ),
            )
        )
    );
    ok $sub.signature.params[0].type =:= Int, 'Specified type is introspectable';
    lives-ok { $sub(42) }, 'Passing correct type lives';
    dies-ok { $sub("foo") }, 'Passing wrong type dies';
}

subtest 'Optional Scalar parameter defaults to type object' => {
    # sub (Int $x?) { $x }
    my $sub := EVAL RakuAST::Sub.new(
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new(
                    type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
                    target => RakuAST::ParameterTarget::Var.new('$x'),
                    optional => 1,
                ),
            )
        ),
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('$x')
            )
        ))
    );
    ok $sub.signature.params[0].optional, 'Parameter introspects as optional';
    is $sub.arity, 0, 'Correct arity';
    is $sub.count, 1, 'Correct count';
    is-deeply $sub(66), 66, 'Passing an argument gets the value';
    is-deeply $sub(), Int, 'Passing no argument gets the type object';
}

subtest 'One required named parameter' => { 
    # sub (:$named!) { $named }
    my $sub := EVAL RakuAST::Sub.new(
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$named'),
                    names => ['named']
                ),
            )
        ),
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('$named')
            )
        ))
    );
    is $sub.signature.params.elems, 1, 'Sub has one params elem';
    is-deeply $sub.arity, 0, 'The block has 0 arity';
    is-deeply $sub.count, 0, 'The block has 0 count';
    given $sub.signature.params[0] {
        is-deeply .name, '$named', 'Correct variable name';
        is-deeply .named_names, ('named',), 'Correct named name';
        nok .optional, 'It is not optional';
    }
    is $sub(named => 99), 99, 'Invoking the sub with a named argument works';
    dies-ok { $sub() }, 'Invoking the sub without an argument dies';
}

subtest 'Required named parameter with alias' => { 
    # sub (:first(:fst($var))!) { $var }
    my $sub := EVAL RakuAST::Sub.new(
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$var'),
                    names => ['first', 'fst']
                ),
            )
        ),
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Var::Lexical.new('$var')
            )
        ))
    );
    is $sub.signature.params.elems, 1, 'Sub has one params elem';
    is-deeply $sub.arity, 0, 'The block has 0 arity';
    is-deeply $sub.count, 0, 'The block has 0 count';
    given $sub.signature.params[0] {
        is-deeply .name, '$var', 'Correct variable name';
        is-deeply .named_names, ('first','fst'), 'Correct named names';
        nok .optional, 'It is not optional';
    }
    is $sub(first => 33), 33, 'Invoking the sub with first alias works';
    is $sub(fst => 44), 44, 'Invoking the sub with second alias works';
    dies-ok { $sub() }, 'Invoking the sub without an argument dies';
    dies-ok { $sub(var => 99) }, 'Invoking the sub with non-named name dies';
}
