use MONKEY-SEE-NO-EVAL;
use Test;

plan 3;

{
    my $class = EVAL RakuAST::Package.new:
        package-declarator => 'class',
        how => Metamodel::ClassHOW,
        name => 'MyTestClass',
        repr => 'P6opaque';
    nok $class.DEFINITE, 'Class evluates to a type object';
    is $class.^name, 'MyTestClass', 'Correct class name';
    is $class.REPR, 'P6opaque', 'Correct representation';
}

