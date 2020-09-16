use MONKEY-SEE-NO-EVAL;
use Test;

plan 40;

{  # my class MyTestClass is repr<P6opaque> { }
    my $class = EVAL RakuAST::Package.new:
        scope => 'my',
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('MyTestClass'),
        repr => 'P6opaque';
    nok $class.DEFINITE, 'Class evaluates to a type object';
    is $class.^name, 'MyTestClass', 'Correct class name';
    is $class.REPR, 'P6opaque', 'Correct representation';
}

{  # my class TestClassWithMethods { method test-meth() { 456 } }
    my $class = EVAL RakuAST::Package.new:
        scope => 'my',
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('TestClassWithMethods'),
        body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
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
        )));
    nok $class.DEFINITE, 'Class with method evluates to a type object';
    is $class.^name, 'TestClassWithMethods', 'Correct class name';
    ok $class.^lookup('test-meth'), 'The class has a test-meth method';
    is $class.test-meth(), 456, 'Can call method without signature and get expected return value';
}

is-deeply EVAL(RakuAST::Type::Simple.new(RakuAST::Name.from-identifier-parts('Proc', 'Async'))),
    Proc::Async,
    'Can resolve a multi-part type name from the setting';

{  # { my class LexicalTestClass is repr<P6opaque> { }; LexicalTestClass }
    my $result := EVAL RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
            RakuAST::Package.new(
                scope => 'my',
                package-declarator => 'class',
                how => Metamodel::ClassHOW,
                name => RakuAST::Name.from-identifier('LexicalTestClass'),
                repr => 'P6opaque'
            )
        ),
        RakuAST::Statement::Expression.new(
            RakuAST::Type::Simple.new(RakuAST::Name.from-identifier-parts('LexicalTestClass'))
        ));
    nok $result.defined, 'Got type object back from looking up package';
    is $result.^name, 'LexicalTestClass', 'Resolved lexically to the correct class';
    nok GLOBAL::<LexicalTestClass>:exists, 'Was not installed globally';
}

{  # { our class OurTestClass is repr<P6opaque> { }; OurTestClass }
    my $result := EVAL RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
            RakuAST::Package.new(
                scope => 'our',
                package-declarator => 'class',
                how => Metamodel::ClassHOW,
                name => RakuAST::Name.from-identifier('OurTestClass'),
                repr => 'P6opaque'
            )
        ),
        RakuAST::Statement::Expression.new(
            RakuAST::Type::Simple.new(RakuAST::Name.from-identifier-parts('OurTestClass'))
        ));
    nok $result.defined, 'Got type object back from looking up our-scoped package';
    is $result.^name, 'OurTestClass', 'Resolved to the correct class';
    ok GLOBAL::<OurTestClass>:exists, 'Was installed globally';
    ok GLOBAL::<OurTestClass> === $result, 'Correct thing installed';
}

module Enclosing {  # our class OurEnclosedClass is repr<P6opaque> { }; OurEnclosedClass
    my $result := EVAL RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(
            RakuAST::Package.new(
                scope => 'our',
                package-declarator => 'class',
                how => Metamodel::ClassHOW,
                name => RakuAST::Name.from-identifier('OurEnclosedClass'),
                repr => 'P6opaque'
            )
        ),
        RakuAST::Statement::Expression.new(
            RakuAST::Type::Simple.new(RakuAST::Name.from-identifier-parts('OurEnclosedClass'))
        ));
    is $result.^name, 'OurEnclosedClass', 'EVAL of package AST inside a module works';
    nok GLOBAL::<OurEnclosedClass>:exists, 'Was not installed globally';
    ok Enclosing::<OurEnclosedClass>:exists, 'Was installed in the current package';
    ok Enclosing::<OurEnclosedClass> === $result, 'Correct thing installed';
}

{  # my class TestClassWithAttribute { has $!foo }
    my $class = EVAL RakuAST::Package.new:
        scope => 'my',
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('TestClassWithAttribute'),
        body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::VarDeclaration::Simple.new(
                    scope => 'has',
                    name => '$!foo',
                )
            )
        )));
    nok $class.DEFINITE, 'Class with attribute evluates to a type object';
    is $class.^name, 'TestClassWithAttribute', 'Correct class name';
    is $class.^attributes.elems, 1, 'Class has one attribute';
    given $class.^attributes[0] {
        is .name, '$!foo', 'Correct attribute name';
        ok .type =:= Mu, 'Correct (default) type';
        nok .has_accessor, 'Correctly claims to have no accessor';
    }
    nok $class.^lookup('foo'), 'No accessor method was generated';
}

{  # my class TestClassWithAttributeAccessor { has Int $.foo }
    my $class = EVAL RakuAST::Package.new:
        scope => 'my',
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('TestClassWithAttributeAccessor'),
        body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::VarDeclaration::Simple.new(
                    scope => 'has',
                    name => '$.foo',
                    type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int'))
                )
            )
        )));
    nok $class.DEFINITE, 'Class with attribute with accessor evluates to a type object';
    is $class.^name, 'TestClassWithAttributeAccessor', 'Correct class name';
    is $class.^attributes.elems, 1, 'Class has one attribute';
    given $class.^attributes[0] {
        is .name, '$!foo', 'Correct attribute name';
        is-deeply .type, Int, 'Correct type constraint';
        ok .has_accessor, 'Correctly claims to have an accessor';
    }
    ok $class.^lookup('foo'), 'Seems like an accessor method was generated';
    is $class.new(foo => 42).foo, 42, 'Accessor and default constructor work fine';
}

{  # my class TestClassWithAttributeUsage { has Int $.bar; method test-meth() { $!bar } }
    my $class = EVAL RakuAST::Package.new:
        scope => 'my',
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('TestClassWithAttributeUsage'),
        body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::VarDeclaration::Simple.new(
                    scope => 'has',
                    name => '$.bar',
                    type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int'))
                )
            ),
            RakuAST::Statement::Expression.new(
                RakuAST::Method.new(
                    name => RakuAST::Name.from-identifier('test-meth'),
                    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::Var::Attribute.new('$!bar')
                        )
                    ))
                )
            )
        )));
    nok $class.DEFINITE, 'Class with attribute with accessor usage evaluates to a type object';
    is $class.^name, 'TestClassWithAttributeUsage', 'Correct class name';
    is $class.new(bar => 99).test-meth, 99, 'Attribute access compiles correctly';
}

{
    my role TestRole {
        method test-meth() { 'role meth' }
    }
    my $class = EVAL RakuAST::Package.new:
        scope => 'my',
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => RakuAST::Name.from-identifier('TestRoleTarget'),
        traits => [RakuAST::Trait::Does.new(RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('TestRole')
        ))];
    is $class.^name, 'TestRoleTarget', 'Class with does trait gets correct name';
    ok $class ~~ TestRole, 'Class with does trait does the role';
    is $class.test-meth, 'role meth', 'The role method can be called';
}
