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


    method archetypes($obj?) {
        unless nqp::isconcrete($!archetypes) {
            my $generic :=
                $!target_type.HOW.archetypes($!target_type).generic
                || $!constraint_type.HOW.archetypes($!constraint_type).generic;
            $!archetypes := Perl6::Metamodel::Archetypes.new(
                :coercive, :nominalizable, :$generic,
                definite => $!target_type.HOW.archetypes($!target_type).definite );
        }
        $!archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), %named)
    }

    method new_type($target, $constraint) {
        my $coercion_type := nqp::parameterizetype((Perl6::Metamodel::CoercionHOW.WHO)<root>,
            [$target, $constraint]);
        nqp::setdebugtypename($coercion_type, $coercion_type.HOW.name($coercion_type));
        $coercion_type
    }

    method set_target_type($target_type) {
        $!target_type := $target_type;
        $!nominal_target := $!target_type.HOW.archetypes($!target_type).nominalizable
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
        $!target_type.HOW.archetypes($!target_type).nominalizable
            ?? $!target_type.HOW.nominalize($!target_type)
            !! $!target_type
    }

    method instantiate_generic($coercion_type, $type_env) {
        return $coercion_type unless self.archetypes.generic;
        my $ins_target :=
            $!target_type.HOW.archetypes($!target_type).generic
                ?? $!target_type.HOW.instantiate_generic($!target_type, $type_env)
                !! $!target_type;
        my $ins_constraint :=
            $!constraint_type.HOW.archetypes($!constraint_type).generic
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
        $coercion_type =:= $checkee
          || $!target_type.HOW.type_check($!target_type, $checkee);
    }

    method accepts_type($coercion_type, $checkee) {
        nqp::istype($checkee, $!target_type) || nqp::istype($checkee, $!constraint_type);
    }

    # Coercion protocol method.
    method coerce($obj, $value) {
#?if moar
        nqp::dispatch('raku-coercion', nqp::decont($obj), $value)
#?endif
#?if !moar
        nqp::istype($value, $!target_type)
          ?? $value                             # already done
          !! self."!coerce_TargetType"($obj, $value)
#?endif
    }

    # Attempt coercion on TargetType
    method !coerce_TargetType($obj, $value) {
        my $constraintHOW := $!constraint_type.HOW;
        $value := $constraintHOW.coerce($!constraint_type, $value)
          if $constraintHOW.archetypes($!constraint_type).coercive;

        my $nominal_target := $!nominal_target;
        nqp::istype($value, $!constraint_type)
          ?? nqp::defined(
               my $method := nqp::tryfindmethod(
                 nqp::what($value),
                 $nominal_target.HOW.name($nominal_target)))
            ?? (nqp::istype((my $coerced := $method($value)),$!target_type)
                || nqp::istype($coerced, nqp::gethllsym('Raku', 'Failure')))
              ?? $coerced
              !! self."!invalid_coercion"($value, $nominal_target.HOW.name($nominal_target), $coerced)
            !! self."!coerce_COERCE"($obj, $value, $nominal_target)
          !! self."!invalid_type"($value)
    }

    # Handle errors
    method !invalid($value, $hint) {
        my $from-type := nqp::what($value);
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Coerce::Impossible',
          "Impossible coercion from "
            ~ $from-type.HOW.name($from-type)
            ~ " into "
            ~ $!target_type.HOW.name($!target_type)
            ~ ": $hint",
          :target-type($!target_type),
          :$from-type,
          :$hint
        )
    }

    # Handle invalid type on accepting
    method !invalid_type($value) {
        self."!invalid"(
          $value,
          "value is of unacceptable type " ~ $value.HOW.name($value) )
    }

    # Attempt coercion with TargetType.COERCE($value).
    method !coerce_COERCE($obj, $value, $nominal_target) {
        nqp::defined(
          my $method := nqp::tryfindmethod(
            (my $HOW := $nominal_target.HOW).archetypes.composable
              ?? ($nominal_target := $HOW.pun($nominal_target))
              !! $nominal_target,
            'COERCE'
          )
        ) && nqp::can($method, 'cando')
          && $method.cando($nominal_target, $value)
          ?? (
               nqp::istype(
                 (my $coerced_value := $method($nominal_target, $value)),
                 $!target_type
               ) || nqp::istype(
                      $coerced_value,
                      nqp::gethllsym('Raku', 'Failure')
                    )
             )
            ?? $coerced_value
            !! self."!invalid_coercion"($value, 'COERCE', $coerced_value)
          !! self."!coerce_new"($obj, $value, $nominal_target)
    }

    # Handle invalid coercion
    method !invalid_coercion($value, $method_name, $coerced_value) {
        self."!invalid"(
          $value,
          "method $method_name returned "
             ~ (nqp::defined($coerced_value)
                 ?? "an instance of "
                 !! "a type object ")
             ~ $coerced_value.HOW.name($coerced_value)
        )
    }

    # Attempt to coerce via TargetType.new
    method !coerce_new($obj, $value, $nominal_target) {
        if nqp::defined(
            my $method := nqp::tryfindmethod($nominal_target, 'new')
        ) && nqp::can($method, 'cando')
          && $method.cando($nominal_target, $value) {
            # There should be no significant performance penalty on this path
            # because if method call ever throws then this is going to result
            # in an exception one way or another.
            my $exception;
            my $coerced_value := nqp::null();
            try {
                CATCH {
                    my $exception_obj := nqp::getpayload($!);

                    if $exception_obj.HOW.name($exception_obj)
                      ne 'X::Constructor::Positional' {
                        $exception := $!;
                    }
                }

                # Provide context information to the method 'new'
                my $*COERCION-TYPE := $obj;
                $coerced_value := $method($nominal_target, $value);

                return $coerced_value
                  if nqp::istype($coerced_value, $!target_type)
                    || nqp::istype(
                         $coerced_value,
                         nqp::gethllsym('Raku', 'Failure') )
            }
            if nqp::defined($exception) {
                nqp::rethrow($exception);
            }
            elsif !nqp::isnull($coerced_value) {
                self."!invalid_coercion"($value, 'new', $coerced_value)
            }
        }
        self."!invalid"($value, "no acceptable coercion method found")
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
