# Coercion types, of the form TargetType(ConstraintType), are implemented with
# 6model parametrics. We create a single BEGIN-time "root" for the coercion
# type family, and the target and constraint types are stored as parameters.
# This means we get cross-compilation-unit interning "for free", as well as
# avoiding a meta-object instance per coercion type created.
class Perl6::Metamodel::CoercionHOW
    does Perl6::Metamodel::MethodDelegation
    does Perl6::Metamodel::TypePretense
{
    my $archetypes := Perl6::Metamodel::Archetypes.new(:coercive);
    method archetypes() {
        $archetypes
    }

    method new_type($target, $constraint) {
        nqp::parameterizetype((Perl6::Metamodel::CoercionHOW.WHO)<root>,
            [$target, $constraint]);
    }

    method name($coercion_type) {
        if nqp::isnull(nqp::typeparameterized($coercion_type)) {
            '?(?)'
        }
        else {
            my $target := nqp::typeparameterat($coercion_type, 0);
            my $constraint := nqp::typeparameterat($coercion_type, 1);
            $target.HOW.name($target) ~ '(' ~ $constraint.HOW.name($constraint) ~ ')'
        }
    }

    sub check_instantiated($coercion_type) {
        nqp::die('Cannot perform this operation on an uninstantiated coercion type')
            if nqp::isnull(nqp::typeparameterized($coercion_type));
    }

    method target_type($coercion_type) {
        check_instantiated($coercion_type);
        nqp::typeparameterat($coercion_type, 0)
    }

    method constraint_type($coercion_type) {
        check_instantiated($coercion_type);
        nqp::typeparameterat($coercion_type, 1)
    }
}
BEGIN {
    my $root := nqp::newtype(Perl6::Metamodel::CoercionHOW, 'Uninstantiable');
    nqp::settypehll($root, 'perl6');
    nqp::setparameterizer($root, sub ($type, $params) {
        # Re-use same HOW.
        nqp::settypehll(nqp::newtype($type.HOW, 'Uninstantiable'), 'perl6');
    });
    (Perl6::Metamodel::CoercionHOW.WHO)<root> := $root;
}
