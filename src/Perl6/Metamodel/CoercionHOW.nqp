# Coercion types, of the form TargetType(ConstraintType), are implemented with
# 6model parametrics. We create a single BEGIN-time "root" for the coercion
# type family, and the target and constraint types are stored as parameters.
# This means we get cross-compilation-unit interning "for free", as well as
# avoiding a meta-object instance per coercion type created.
class Perl6::Metamodel::CoercionHOW
    does Perl6::Metamodel::LanguageRevision
{
    has $!composed;
    has $!target_type;
    has $!nominal_target;
    has $!constraint_type;
    has $!archetypes;

    my $archetypes_g := Perl6::Metamodel::Archetypes.new(:coercive, :nominalizable, :generic);
    my $archetypes_ng := Perl6::Metamodel::Archetypes.new(:coercive, :nominalizable);

    method archetypes() {
        unless nqp::isconcrete($!archetypes) {
            $!archetypes := $!target_type.HOW.archetypes.generic || $!constraint_type.HOW.archetypes.generic
                            ?? $archetypes_g
                            !! $archetypes_ng;
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

    method compose($coercion_type) {
        if $!composed {
            return $coercion_type;
        }
        self.set_language_version($coercion_type, :force);
        nqp::settypecheckmode($coercion_type, 2);
        $!composed := 1;
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
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        $target_type.HOW.archetypes.nominalizable
            ?? $target_type.HOW.nominalize($target_type)
            !! $target_type
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
        my $ins := self.new_type($ins_target, $ins_constraint);
        $ins.HOW.compose($ins)
    }

    method find_method($coercion_type, $name, *%c) {
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        $target_type.HOW.find_method($target_type, $name, |%c)
    }

    method find_method_qualified($coercion_type, $qtype, $name) {
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        $target_type.HOW.find_method_qualified($target_type, $qtype, $name)
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
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        my $rc := $target_type.HOW.type_check($target_type, $checkee);
        $rc
    }

    method accepts_type($coercion_type, $checkee) {
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        my $constraint_type := $coercion_type.HOW.constraint_type($coercion_type);
        my $rc := nqp::istype($checkee, $target_type) || nqp::istype($checkee, $constraint_type);
        $rc
    }

    # Coercion protocol method.
    method coerce($obj, $value) {
        if nqp::istype($value, $!target_type) {
            return $value
        }

        my $value_type := nqp::what($value);
        my $coerced_value := nqp::null();
        my $coercion_method;
        my $method;

        # First we try $value.TargetType() approach
        $coercion_method := $!nominal_target.HOW.name($!nominal_target);
        $method := nqp::tryfindmethod($value_type, $coercion_method);
        if nqp::defined($method) {
            $coerced_value := $method($value)
        }

        # Then try TargetType.COERCE($value).
        if nqp::isnull($coerced_value) {
            $method := nqp::tryfindmethod($!nominal_target, $coercion_method := 'COERCE');
            if nqp::defined($method) && nqp::can($method, 'cando') && $method.cando($!nominal_target, $value) {
                $coerced_value := $method($!nominal_target, $value);
            }
        }

        # And eventually fall back to new. Note that it is invoked on the coercion type invokee to let the method know
        # it's context.
        if nqp::isnull($coerced_value) {
            $method := nqp::tryfindmethod($!nominal_target, $coercion_method := 'new');
            if nqp::defined($method) && nqp::can($method, 'cando') && $method.cando($!nominal_target, $value) {
                # There should be no signifacnt performance penalty on this path because if method call ever throws
                # then this is gonna result in an exception one way or another.
                my $exception;
                try {
                    my $*COERCION-TYPE := $obj; # Provide context information to the method 'new'
                    $coerced_value := $method($!nominal_target, $value);
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

        my $coerced_decont := nqp::decont($coerced_value);
        # Fail for either no coercion method found or wrong type returned, except for Failure kind of return which we
        # must bypass as it carries some useful information about another error.
        if nqp::isnull($coerced_value)
            || !(nqp::istype($coerced_decont, $!target_type)
                || nqp::istype($coerced_decont, nqp::gethllsym('Raku', 'Failure')))
        {
            my %ex := nqp::gethllsym('Raku', 'P6EX');
            my $target_type_name := $!target_type.HOW.name($!target_type);
            my $value_type_name := $value_type.HOW.name($value_type);
            my $hint;
            if nqp::isnull($coerced_value) {
                $hint := "no acceptable coercion method found";
            }
            else {
                my $coerced_name := $coerced_decont.HOW.name($coerced_decont);
                $hint := "method " ~ $coercion_method ~ " returned "
                            ~ (nqp::defined($coerced_decont) ?? "an instance of" !! "a type object")
                            ~ " " ~ $coerced_name;
            }
            if %ex {
                %ex<X::Coerce::Impossible>($target_type_name, $value_type_name, $hint)
            }
            nqp::die("Impossible coercion from " ~ $value_type.HOW.name($value_type_name)
                        ~ " into " ~ $target_type_name) ~ ": " ~ $hint;
        }

        $coerced_value
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
