use MONKEY-SEE-NO-EVAL;
use Test;

plan 10;

my $ast;

subtest 'Create an empty class' => {
    # my class MyTestClass { }
    $ast := RakuAST::Package.new(
      scope              => 'my',
      package-declarator => 'class',
      name               => RakuAST::Name.from-identifier('MyTestClass'),
      how  => Metamodel::ClassHOW,
      repr => 'P6opaque'
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $class {
        nok $class.DEFINITE,
          "$type: Class evaluates to a type object";
        is $class.^name, 'MyTestClass',
          "$type: Correct class name";
        is $class.REPR, 'P6opaque',
          "$type: Correct representation";
    }
}

subtest 'Create a class with a method' => {
    # my class TestClassWithMethods { method test-meth() { 456 } }
    $ast := RakuAST::Package.new(
      scope => 'my',
      package-declarator => 'class',
      name  => RakuAST::Name.from-identifier('TestClassWithMethods'),
      how   => Metamodel::ClassHOW,
      body  => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::Method.new(
                name => RakuAST::Name.from-identifier('test-meth'),
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      RakuAST::IntLiteral.new(456)
                    )
                  )
                )
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $class {
        nok $class.DEFINITE,
          "$type: Class with method evaluates to a type object";
        is $class.^name, 'TestClassWithMethods',
          "$type: Correct class name";
        ok $class.^lookup('test-meth'),
          "$type: The class has a test-meth method";
        is $class.test-meth(), 456,
          "$type: Can call method without signature and get expected value";
    }
}

is-deeply EVAL(RakuAST::Type::Simple.new(RakuAST::Name.from-identifier-parts('Proc', 'Async'))),
    Proc::Async,
    'Can resolve a multi-part type name from the setting';

subtest 'Check lexically resolving of a class' => {
    # my class LexicalTestClass { }; LexicalTestClass
    $ast := RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        RakuAST::Package.new(
          scope => 'my',
          package-declarator => 'class',
          name  => RakuAST::Name.from-identifier('LexicalTestClass'),
          how   => Metamodel::ClassHOW,
          repr  => 'P6opaque'
        )
      ),
      RakuAST::Statement::Expression.new(
        RakuAST::Type::Simple.new(
          RakuAST::Name.from-identifier-parts('LexicalTestClass')
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $result {
        nok $result.defined,
          "$type: Got type object back from looking up package";
        is $result.^name, 'LexicalTestClass',
          "$type: Resolved lexically to the correct class";
        nok GLOBAL::<LexicalTestClass>:exists,
          "$type: Was not installed globally";
    }
}

subtest 'Check globally resolving of a class' => {
    for 'AST', 'DEPARSE' -> $type {
        my $class = "OurTestClass$type";

        # class OurTestClass$type { }; OurTestClass$type
        $ast := RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            RakuAST::Package.new(
              scope => 'our',
              package-declarator => 'class',
              name  => RakuAST::Name.from-identifier($class),
              how   => Metamodel::ClassHOW,
              repr  => 'P6opaque'
            )
          ),
          RakuAST::Statement::Expression.new(
            RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier-parts($class)
            )
          )
        );

        my $result := $type eq 'AST' ?? EVAL($ast) !! EVAL($ast.DEPARSE);
        nok $result.defined,
          "Got type object back from looking up our-scoped package";
        is $result.^name, $class,
          "Resolved to the correct class";
        ok GLOBAL::{$class}:exists,
          "Was installed globally";
        ok GLOBAL::{$class} === $result,
          "Correct thing installed";
    }
}

module Enclosing {
    subtest 'our class inside an enclosing module' => {
        for 'AST', 'DEPARSE' -> $type {
            my $class = "OurEnclosedClass$type";

            # class OurEnclosedClass$type { }; OurEnclosedClass$type
            $ast := RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                RakuAST::Package.new(
                  scope => 'our',
                  package-declarator => 'class',
                  name  => RakuAST::Name.from-identifier($class),
                  how   => Metamodel::ClassHOW,
                  repr  => 'P6opaque'
                )
              ),
              RakuAST::Statement::Expression.new(
                RakuAST::Type::Simple.new(
                  RakuAST::Name.from-identifier-parts($class)
                )
              )
            );

            my $result := $type eq 'AST' ?? EVAL($ast) !! EVAL($ast.DEPARSE);
            is $result.^name, "Enclosing::$class",
              "$type: EVAL of package AST inside a module works";
            nok GLOBAL::{$class}:exists,
              "$type: Was not installed globally";
            ok Enclosing::{$class}:exists,
              "$type: Was installed in the current package";
            ok Enclosing::{$class} === $result,
              "$type: Correct thing installed";
        }
    }
}

subtest 'class with attribute' => {
    # my class TestClassWithAttribute { has $!foo }
    $ast := RakuAST::Package.new(
      scope => 'my',
      package-declarator => 'class',
      name  => RakuAST::Name.from-identifier('TestClassWithAttribute'),
      how   => Metamodel::ClassHOW,
      body  => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::VarDeclaration::Simple.new(
                  scope => 'has',
                  name => '$!foo',
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $class {
        nok $class.DEFINITE,
          "$type: Class with attribute evluates to a type object";
        is $class.^name, 'TestClassWithAttribute',
          "$type: Correct class name";
        is $class.^attributes.elems, 1,
          "$type: Class has one attribute";
        given $class.^attributes[0] {
            is .name, '$!foo',
              "$type: Correct attribute name";
            ok .type =:= Mu,
              "$type: Correct (default) type";
            nok .has_accessor,
              "$type: Correctly claims to have no accessor";
        }
        nok $class.^lookup('foo'),
          "$type: No accessor method was generated";
    }
}

subtest 'class with attribute and accessor' => {
    # my class TestClassWithAttributeAccessor { has Int $.foo }
    $ast := RakuAST::Package.new(
      scope => 'my',
      package-declarator => 'class',
      name  => RakuAST::Name.from-identifier('TestClassWithAttributeAccessor'),
      how   => Metamodel::ClassHOW,
      body  => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::VarDeclaration::Simple.new(
                scope => 'has',
                name  => '$.foo',
                type  => RakuAST::Type::Simple.new(
                  RakuAST::Name.from-identifier('Int')
                )
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $class {
        nok $class.DEFINITE,
          "$type: Class with attribute with accessor evluates to a type object";
        is $class.^name, 'TestClassWithAttributeAccessor',
          "$type: Correct class name";
        is $class.^attributes.elems, 1,
          "$type: Class has one attribute";
        given $class.^attributes[0] {
            is .name, '$!foo',
              "$type: Correct attribute name";
            is-deeply .type, Int,
              "$type: Correct type constraint";
            ok .has_accessor,
              "$type: Correctly claims to have an accessor";
        }
        ok $class.^lookup('foo'),
          "$type: Seems like an accessor method was generated";
        is $class.new(foo => 42).foo, 42,
          "$type: Accessor and default constructor work fine";
    }
}

subtest 'class with accessor usage' => {
    # my class TestClassWithAttributeUsage {
    #     has Int $.bar;
    #     method test-meth() { $!bar }
    # }
    $ast := RakuAST::Package.new(
      scope => 'my',
      package-declarator => 'class',
      name => RakuAST::Name.from-identifier('TestClassWithAttributeUsage'),
      how => Metamodel::ClassHOW,
      body => RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
          RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
              RakuAST::VarDeclaration::Simple.new(
                scope => 'has',
                name  => '$.bar',
                type  => RakuAST::Type::Simple.new(
                  RakuAST::Name.from-identifier('Int')
                )
              )
            ),
            RakuAST::Statement::Expression.new(
              RakuAST::Method.new(
                name => RakuAST::Name.from-identifier('test-meth'),
                body => RakuAST::Blockoid.new(
                  RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                      RakuAST::Var::Attribute.new('$!bar')
                    )
                  )
                )
              )
            )
          )
        )
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $class {
        nok $class.DEFINITE,
          "$type: Class with accessor usage evaluates to a type object";
        is $class.^name, 'TestClassWithAttributeUsage',
          "$type: Correct class name";
        is $class.new(bar => 99).test-meth, 99,
          "$type: Attribute access compiles correctly";
    }
}

subtest 'class with does trait gets correct name' => {
    my role TestRole {
        method test-meth() { 'role meth' }
    }
    $ast := RakuAST::Package.new(
      scope => 'my',
      package-declarator => 'class',
      name => RakuAST::Name.from-identifier('TestRoleTarget'),
      how => Metamodel::ClassHOW,
      traits => [
        RakuAST::Trait::Does.new(
          RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier('TestRole')
          )
        )
      ]
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $class {
        is $class.^name, 'TestRoleTarget',
          "$type: Class with does trait gets correct name";
        ok $class ~~ TestRole,
          "$type: Class with does trait does the role";
        is $class.test-meth, 'role meth',
          "$type: The role method can be called";
    }
}
