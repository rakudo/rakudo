# Coercion types, of the form TargetType(ConstraintType), are implemented with
# 6model parametrics. We create a single BEGIN-time "root" for the coercion
# type family, and the target and constraint types are stored as parameters.
# This means we get cross-compilation-unit interning "for free", as well as
# avoiding a meta-object instance per coercion type created.
class Perl6::Metamodel::CoercionHOW
    does Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::Nominalizable
{
    has $!target_type;
    has $!nominal_target;
    has $!constraint_type;
    has $!archetypes;


    method archetypes() {
        unless nqp::isconcrete($!archetypes) {
            my $generic := $!target_type.HOW.archetypes.generic || $!constraint_type.HOW.archetypes.generic;
            $!archetypes := Perl6::Metamodel::Archetypes.new(
                :coercive, :nominalizable, :$generic,
                definite => $!target_type.HOW.archetypes.definite,
            );
        }
        $!archetypes
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

    method set_target_type($target_type) {
        $!target_type := $target_type;
        $!nominal_target := $!target_type.HOW.archetypes.nominalizable
                                ?? $!target_type.HOW.nominalize($!target_type)
                                !! $!target_type;
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

    method nominal_target($coercion_type) {
        $!nominal_target
    }

    method nominalize($coercion_type) {
        $!target_type.HOW.archetypes.nominalizable
            ?? $!target_type.HOW.nominalize($!target_type)
            !! $!target_type
    }

    method instantiate_generic($coercion_type, $type_env) {
        return $coercion_type unless $!archetypes.generic;
        my $ins_target :=
            $!target_type.HOW.archetypes.generic
                ?? $!target_type.HOW.instantiate_generic($!target_type, $type_env)
                !! $!target_type;
        my $ins_constraint :=
            $!constraint_type.HOW.archetypes.generic
                ?? $!constraint_type.HOW.instantiate_generic($!constraint_type, $type_env)
                !! $!constraint_type;
        self.new_type($ins_target, $ins_constraint);
    }

    method find_method($coercion_type, $name, *%c) {
        $!target_type.HOW.find_method($!target_type, $name, |%c)
    }

    method find_method_qualified($coercion_type, $qtype, $name) {
        $!target_type.HOW.find_method_qualified($!target_type, $qtype, $name)
    }

    method isa($obj, $type) {
        $!nominal_target.HOW.isa($obj, $type)
    }

    method does($obj, $type) {
        $!nominal_target.HOW.does($obj, $type)
    }

    method type_check($coercion_type, $checkee) {
        if $coercion_type =:= $checkee {
            return 1;
        }
        $!target_type.HOW.type_check($!target_type, $checkee);
    }

    method accepts_type($coercion_type, $checkee) {
        nqp::istype($checkee, $!target_type) || nqp::istype($checkee, $!constraint_type);
    }

    # Coercion protocol method.
    method coerce($obj, $value) {
        if nqp::istype($value, $!target_type) {
            return $value
        }

        # Support nested coercions
        if nqp::can($!constraint_type.HOW.archetypes, 'coercive')
            && $!constraint_type.HOW.archetypes.coercive
        {
            $value := $!constraint_type.HOW.coerce($!constraint_type, $value);
        }

        my $hint;
        my $coerced_value := nqp::null();
        my $value_type := nqp::what($value);
        my $coercion_method;

        if nqp::istype($value, $!constraint_type) {
            my $method;

            # First we try $value.TargetType() approach
            $coercion_method := $!nominal_target.HOW.name($!nominal_target);
            $method := nqp::tryfindmethod($value_type, $coercion_method);
            if nqp::defined($method) {
                $coerced_value := $method($value);
            }

            my $nominal_target := $!nominal_target.HOW.archetypes.composable
                                    ?? $!nominal_target.HOW.pun($!nominal_target)
                                    !! $!nominal_target;

            # Then try TargetType.COERCE($value).
            if nqp::isnull($coerced_value) {
                $method := nqp::tryfindmethod($nominal_target, $coercion_method := 'COERCE');
                if nqp::defined($method) && nqp::can($method, 'cando') && $method.cando($nominal_target, $value) {
                    $coerced_value := $method($nominal_target, $value);
                }
            }

            # And eventually fall back to new.
            if nqp::isnull($coerced_value) {
                $method := nqp::tryfindmethod($nominal_target, $coercion_method := 'new');
                if nqp::defined($method) && nqp::can($method, 'cando') && $method.cando($nominal_target, $value) {
                    # There should be no signifacnt performance penalty on this path because if method call ever throws
                    # then this is gonna result in an exception one way or another.
                    my $exception;
                    try {
                        my $*COERCION-TYPE := $obj; # Provide context information to the method 'new'
                        $coerced_value := $method($nominal_target, $value);
                        CATCH {
                            my $exception_obj := nqp::getpayload($!);

                            unless $exception_obj.HOW.name($exception_obj) eq 'X::Constructor::Positional' {
                                $exception := $!;
                            }
                        }
                    }
                    if nqp::defined($exception) {
                        nqp::rethrow($exception);
                    }
                }
            }
        }
        else {
            $hint := "value is of unacceptable type " ~ $value.HOW.name($value);
        }

        my $coerced_decont := nqp::decont($coerced_value);
        # Fail for either no coercion method found or wrong type returned, except for Failure kind of return which we
        # must bypass as it carries some useful information about another error.
        if nqp::isnull($coerced_value)
            || !(nqp::istype($coerced_decont, $!target_type)
                || nqp::istype($coerced_decont, nqp::gethllsym('Raku', 'Failure')))
        {
            my $target_type_name := $!target_type.HOW.name($!target_type);
            my $value_type_name := $value_type.HOW.name($value_type);
            unless $hint {
                if nqp::isnull($coerced_value) {
                    $hint := "no acceptable coercion method found";
                }
                else {
                    my $coerced_name := $coerced_decont.HOW.name($coerced_decont);
                    $hint := "method " ~ $coercion_method ~ " returned "
                                ~ (nqp::defined($coerced_decont) ?? "an instance of" !! "a type object")
                                ~ " " ~ $coerced_name;
                }
            }
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Coerce::Impossible',
                "Impossible coercion from "
                    ~ $value_type_name
                    ~ " into " ~ $target_type_name
                    ~ ": " ~ $hint,
                :target-type($!target_type),
                :from-type($value_type),
                :$hint
            )
        }

        $coerced_value
    }

    # Methods needed by Perl6::Metamodel::Nominalizable
    method nominalizable_kind() { 'coercion' }
    method !wrappee($obj) { $!target_type }
}
BEGIN {
    my $root := nqp::newtype(Perl6::Metamodel::CoercionHOW.new, 'Uninstantiable');
    nqp::settypehll($root, 'Raku');
    nqp::setdebugtypename(nqp::settypehll($root, 'Raku'), 'CoercionHOW root');
    nqp::setparameterizer($root, sub ($type, $params) {
        my $metaclass := $type.HOW.new();
        $metaclass.set_target_type($params[0]);
        $metaclass.set_constraint_type($params[1]);
        my $coercion_type := nqp::settypehll(nqp::newtype($metaclass, 'Uninstantiable'), 'Raku');
        $metaclass.set_language_version($coercion_type, :force);
        nqp::settypecheckmode($coercion_type, 2);
        $coercion_type
    });
    (Perl6::Metamodel::CoercionHOW.WHO)<root> := $root;
}

# vim: expandtab sw=4
