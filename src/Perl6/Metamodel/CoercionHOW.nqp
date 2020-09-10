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
        my $tt := $coercion_type.HOW.target_type($coercion_type);
        my $ct := $coercion_type.HOW.constraint_type($coercion_type);
        nqp::settypecheckmode($coercion_type, 2);
        $!composed := 1;
        $coercion_type
    }

    method set_target_type($target_type) {
        $!target_type := $target_type;
        $!nominal_target := nqp::if($!target_type.HOW.archetypes.nominalizable,
                                    $!target_type.HOW.nominalize($!target_type),
                                    $!target_type);
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
        return self unless self.archetypes.generic;
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
        if nqp::getenvhash<RAKUDO_DEBUG> {
            say("&&& find_method on ", $coercion_type.HOW.name($coercion_type), " for ", $name);
        }
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        $target_type.HOW.find_method($target_type, $name, |%c)
    }

    method find_method_qualified($coercion_type, $qtype, $name) {
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        $target_type.HOW.find_method_qualified($target_type, $qtype, $name)
    }

    method type_check($coercion_type, $checkee) {
        say("??? type_check(", $coercion_type.HOW.name($coercion_type), " vs. ", $checkee.HOW.name($checkee), ")") if nqp::getenvhash<RAKUDO_DEBUG>;
        if $coercion_type =:= $checkee {
            say("??? types are full match") if nqp::getenvhash<RAKUDO_DEBUG>;
            return 1;
        }
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        my $rc := $target_type.HOW.type_check($target_type, $checkee);
        say("??? checks? ", $rc) if nqp::getenvhash<RAKUDO_DEBUG>;
        $rc
    }

    method accepts_type($coercion_type, $checkee) {
        say("??? accepts_type(", $coercion_type.HOW.name($coercion_type), " vs. ", $checkee.HOW.name($checkee), ")") if nqp::getenvhash<RAKUDO_DEBUG>;
        my $target_type := $coercion_type.HOW.target_type($coercion_type);
        my $constraint_type := $coercion_type.HOW.constraint_type($coercion_type);
        my $rc := nqp::istype($checkee, $target_type) || nqp::istype($checkee, $constraint_type);
        say("??? accepts? ", $rc) if nqp::getenvhash<RAKUDO_DEBUG>;
        $rc
    }

    # Coercion protocol method.
    method !coerce($target_type, $value) {
        say("!!! coerce ", $value.HOW.name($value), " into ", $target_type.HOW.name($target_type)) if nqp::getenvhash<RAKUDO_DEBUG>;
        if nqp::istype($value, $target_type) {
            say("!!! ... coerce isn't needed") if nqp::getenvhash<RAKUDO_DEBUG>;
            return $value
        }

        my $value_type := nqp::what($value);

        say("!!! ... try target type method first") if nqp::getenvhash<RAKUDO_DEBUG>;
        # First we try $value.TargetType() approach
        my $method := $value_type.HOW.find_method($value_type, $!nominal_target.HOW.name($!nominal_target), :no_fallback);
        unless nqp::isnull($method) {
            return $method($value)
        }

        say("!!! ... try COERCE-INTO") if nqp::getenvhash<RAKUDO_DEBUG>;
        # Next we try $value.COERCE-INTO(TargetType). This would make possible coercion into types with compound names
        # like MyPackage::TargetType.
        # XXX COERCE-* methods can only be of type Routine now. Does it ever makes sense for them to be of some other
        # base class?
        $method := $value_type.HOW.find_method($value_type, 'COERCE-INTO');
        if nqp::defined($method) && nqp::can($method, 'cando') && $method.cando($value, $target_type) {
            return $method($value, $target_type);
        }

        say("!!! ... try COERCE-FROM") if nqp::getenvhash<RAKUDO_DEBUG>;
        # As the last resort we fallback to TargetType.COERCE-FROM($value). This is the worst possible variant because
        # the best possible coercion may require access to source calss private data. Yet, this may work for many simple
        # cases like TargetType(Str), for example.
        $method := $target_type.HOW.find_method($target_type, 'COERCE-FROM');
        if nqp::defined($method) && nqp::can($method, 'cando') && $method.cando($target_type, $value) {
            return $method($target_type, $value);
        }

        say("!!! ... failed") if nqp::getenvhash<RAKUDO_DEBUG>;
        my %ex := nqp::gethllsym('Raku', 'P6EX');
        my $target_type_name := $target_type.HOW.name($target_type);
        my $value_type_name := $value_type.HOW.name($value_type);
        if %ex {
            %ex<X::Coerce::Impossible>($target_type_name, $value_type_name)
        }
        nqp::die("Impossible coercion from " ~ $value_type.HOW.name($value_type_name)
                    ~ " into " ~ $target_type.HOW.name($target_type_name));
    }

    method coerce($obj, $value) {
        self."!coerce"($!target_type, $value)
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
