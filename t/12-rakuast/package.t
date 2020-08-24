use MONKEY-SEE-NO-EVAL;
use Test;

plan 7;

{
    my $class = EVAL RakuAST::Package.new:
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('MyTestClass'),
        repr => 'P6opaque';
    nok $class.DEFINITE, 'Class evluates to a type object';
    is $class.^name, 'MyTestClass', 'Correct class name';
    is $class.REPR, 'P6opaque', 'Correct representation';
}

{
    my $class = EVAL RakuAST::Package.new:
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('TestClassWithMethods'),
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::Method.new(
                    name => RakuAST::Name.from-identifier('test-meth'),
                    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::IntLiteral.new(456)
                        )
                    ))
                )
            )
        ));
    nok $class.DEFINITE, 'Class with method evluates to a type object';
    is $class.^name, 'TestClassWithMethods', 'Correct class name';
    ok $class.^lookup('test-meth'), 'The class has a test-meth method';
    #is $class.test-meth(), 456, 'Can call method without signature and get expected return value';
}

is-deeply EVAL(RakuAST::Type::Simple.new(RakuAST::Name.from-identifier-parts('Proc', 'Async'))),
    Proc::Async,
    'Can resolve a multi-part type name from the setting';
