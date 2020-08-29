# Coercion types, of the form TargetType(ConstraintType), are implemented with
# 6model parametrics. We create a single BEGIN-time "root" for the coercion
# type family, and the target and constraint types are stored as parameters.
# This means we get cross-compilation-unit interning "for free", as well as
# avoiding a meta-object instance per coercion type created.
class Perl6::Metamodel::CoercionHOW
    # does Perl6::Metamodel::MethodDelegation
    # does Perl6::Metamodel::TypePretense
{
    has $!composed;
    has $!target_type;
    has $!constraint_type;

    my $archetypes := Perl6::Metamodel::Archetypes.new(:coercive, :nominalizable);
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type($target, $constraint) {
        my $coercion_type := nqp::parameterizetype((Perl6::Metamodel::CoercionHOW.WHO)<root>,
            [$target, $constraint]);
        nqp::setdebugtypename($coercion_type, $coercion_type.HOW.name($coercion_type));
        $coercion_type
    }

    method compose($coercion_type) {
        if $!composed {
            return $coercion_type;
        }
        my $tt := $coercion_type.HOW.target_type($coercion_type);
        my $ct := $coercion_type.HOW.constraint_type($coercion_type);
        nqp::settypecheckmode($coercion_type, 2);
        $!composed := 1;
        $coercion_type
    }

    method set_target_type($target_type) {
        $!target_type := $target_type;
    }

    method set_constraint_type($constraint_type) {
        $!constraint_type := $constraint_type;
    }

    method name($coercion_type) {
        $!target_type.HOW.name($!target_type) ~ '(' ~ $!constraint_type.HOW.name($!constraint_type) ~ ')'
    }

    method shortname($coercion_type) {
        $!target_type.HOW.shortname($!target_type) ~ '(' ~ $!constraint_type.HOW.shortname($!constraint_type) ~ ')'
    }

    method target_type($coercion_type) {
        $!target_type
    }

    method constraint_type($coercion_type) {
        $!constraint_type
    }

    method nominalize($coercion_type) {
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        $target_type.HOW.archetypes.nominalizable
            ?? $target_type.HOW.nominalize($target_type)
            !! $target_type
    }

    method find_method($coercion_type, $name, *%c) {
        say('find_method(', $coercion_type.HOW.name($coercion_type), ", ", $name, ')') if nqp::getenvhash<RAKUDO_DEBUG>;
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        $target_type.HOW.find_method($target_type, $name, |%c)
    }

    method type_check($obj, $checkee) {
        say("type_check, obj:", $obj.HOW.name($obj), ", checkee:", $checkee.HOW.name($checkee)) if nqp::getenvhash<RAKUDO_DEBUG>;
        if $obj =:= $checkee {
            return 1;
        }
        my $target_type := $obj.HOW.target_type($obj);
        my $rc := $target_type.HOW.type_check($target_type, $checkee);
        say("type checked: ", $rc) if nqp::getenvhash<RAKUDO_DEBUG>;
        $rc
    }

    method accepts_type($coercion_type, $checkee) {
        say("accepts_type, obj:", $coercion_type.HOW.name($coercion_type), ", checkee:", $checkee.HOW.name($checkee)) if nqp::getenvhash<RAKUDO_DEBUG>;
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        my $constraint_type := $coercion_type.HOW.constraint_type($coercion_type);
        my $rc := nqp::istype($checkee, $target_type) || nqp::istype($checkee, $constraint_type);
        say("accepted: ", $rc) if nqp::getenvhash<RAKUDO_DEBUG>;
        $rc
    }

    # Coercion protocol method.
    method coerce($obj, $value) {
        my $value_type := nqp::what($value);
        if nqp::istype($value_type, $!target_type) {
            return $value
        }

        # First we try $value.TargetType() approach
        my $method := $value_type.HOW.find_method($value_type, $!target_type.HOW.name($!target_type), :no_fallback);
        unless nqp::isnull($method) {
            return $method($value)
        }

        # Next we try $value.COERCE-INTO(TargetType). This would make possible coercion into types with compound names
        # like MyPackage::TargetType.
        $method := $value_type.HOW.find_method($value_type, 'COERCE-INTO');
        if nqp::defined($method) {
            return $method($value, $!target_type);
        }

        # As the last resort we fallback to TargetType.COERCE-FROM($value). This is the worst possible variant because
        # the best possible coercion may require access to source calss private data. Yet, this may work for many simple
        # cases like TargetType(Str), for example.
        $method := $!target_type.HOW.find_method($!target_type, 'COERCE-FROM');
        if nqp::defined($method) {
            return $method($!target_type, $value);
        }

        # TODO To be replaced with a proper Exception throwing.
        nqp::die("Impossible coercion of " ~ $value_type.HOW.name($value_type)
                    ~ " into " ~ $!target_type.HOW.name($!target_type));
    }
}
BEGIN {
    my $root := nqp::newtype(Perl6::Metamodel::CoercionHOW.new, 'Uninstantiable');
    nqp::settypehll($root, 'Raku');
    nqp::setparameterizer($root, sub ($type, $params) {
        my $metaclass := $type.HOW.new();
        $metaclass.set_target_type($params[0]);
        $metaclass.set_constraint_type($params[1]);
        nqp::settypehll(nqp::newtype($metaclass, 'Uninstantiable'), 'Raku');
    });
    (Perl6::Metamodel::CoercionHOW.WHO)<root> := $root;
}

# vim: expandtab sw=4
