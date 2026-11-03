#- Metamodel::CoercionHOW ------------------------------------------------------
# Coercion types, of the form TargetType(ConstraintType), are implemented with
# 6model parametrics. We create a single BEGIN-time "root" for the coercion
# type family, and the target and constraint types are stored as parameters.
# This means we get cross-compilation-unit interning "for free", as well as
# avoiding a meta-object instance per coercion type created.
class Perl6::Metamodel::CoercionHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::Nominalizable
{
    has     $!target_type;
    has     $!constraint_type;
    has     $!nominal_target;
    has     $!archetypes;
    has int $!target_type_generic;
    has int $!constraint_type_generic;

    method new(:$target_type!, :$constraint_type!) {
        my $obj := nqp::create(self);

        my int $target_type_generic :=
          $target_type.HOW.archetypes($target_type).generic;
        my int $constraint_type_generic :=
          $constraint_type.HOW.archetypes($constraint_type).generic;
        my str $name := $target_type.HOW.name($target_type)
          ~ '(' ~ $constraint_type.HOW.name($constraint_type) ~ ')';

        nqp::bindattr($obj, Perl6::Metamodel::CoercionHOW, '$!target_type',
          $target_type);
        nqp::bindattr($obj, Perl6::Metamodel::CoercionHOW, '$!constraint_type',
          $constraint_type);
        nqp::bindattr($obj, Perl6::Metamodel::CoercionHOW, '$!nominal_target',
          $target_type.HOW.archetypes($target_type).nominalizable
            ?? $target_type.HOW.nominalize($target_type)
            !! $target_type);
        nqp::bindattr_i($obj, Perl6::Metamodel::CoercionHOW,
          '$!target_type_generic', $target_type_generic
        );
        nqp::bindattr_i($obj, Perl6::Metamodel::CoercionHOW,
          '$!constraint_type_generic', $constraint_type_generic
        );
        nqp::bindattr($obj, Perl6::Metamodel::CoercionHOW, '$!archetypes',
          Perl6::Metamodel::Archetypes.new(
            :coercive,
            :nominalizable,
            :generic($target_type_generic || $constraint_type_generic),
            :definite($target_type.HOW.archetypes($target_type).definite)
          ));

        $obj.set_name(Mu, $name);
        $obj.set_shortname(
          Mu,
          $target_type.HOW.shortname($target_type)
            ~ '(' ~ $constraint_type.HOW.shortname($constraint_type) ~ ')'
        );

        $obj
    }

    method target_type(    $XXX?) { $!target_type     }
    method constraint_type($XXX?) { $!constraint_type }
    method nominal_target( $XXX?) { $!nominal_target  }
    method archetypes(     $XXX?) { $!archetypes      }

    method new_type($target, $constraint) {
        nqp::parameterizetype(
          nqp::atkey(Perl6::Metamodel::CoercionHOW.WHO, 'root'),
          nqp::list($target, $constraint)
        )
    }

    method nominalize($coercion_type) {
        my $target_type := $!target_type;
        $target_type.HOW.archetypes($target_type).nominalizable
          ?? $target_type.HOW.nominalize($target_type)
          !! $target_type
    }

    method instantiate_generic($coercion, $type_env) {
        if $!target_type_generic || $!constraint_type_generic {
            my $target     := $!target_type;
            my $constraint := $!constraint_type;

            $target := $target.HOW.instantiate_generic($target, $type_env)
              if $!target_type_generic;
            $constraint :=
              $constraint.HOW.instantiate_generic($constraint, $type_env)
              if $!constraint_type_generic;

            self.new_type($target, $constraint)
        }
        else {
            $coercion
        }
    }

    method find_method($XXX, $name, *%c) {
        $!target_type.HOW.find_method($!target_type, $name, |%c)
    }

    method find_method_qualified($XXX, $qtype, $name) {
        $!target_type.HOW.find_method_qualified($!target_type, $qtype, $name)
    }

    method isa($target, $type) {
        $!nominal_target.HOW.isa($target, $type)
    }

    method does($target, $type) {
        $!nominal_target.HOW.does($target, $type)
    }

    method type_check($coercion_type, $checkee) {
        nqp::eqaddr($coercion_type, $checkee)
          || $!target_type.HOW.type_check($!target_type, $checkee);
    }

    method accepts_type($coercion_type, $checkee) {
        nqp::istype($checkee, $!target_type)
          || nqp::istype($checkee, $!constraint_type);
    }

    # Coercion protocol method.
    method coerce($target, $value) {
#?if moar
        nqp::dispatch('raku-coercion', nqp::decont($target), $value)
#?endif
#?if !moar
        nqp::istype($value, $!target_type)
          ?? $value                             # already done
          !! self."!coerce_TargetType"($target, $value)
#?endif
    }

    # Attempt coercion on TargetType
    method !coerce_TargetType($target, $value) {
        my $constraint_type := $!constraint_type;
        my $constraintHOW   := $constraint_type.HOW;

        $value := $constraintHOW.coerce($constraint_type, $value)
          if $constraintHOW.archetypes($constraint_type).coercive;

        my $nominal_target := $!nominal_target;
        nqp::istype($value, $constraint_type)
          ?? nqp::defined(
               my $method := nqp::tryfindmethod(
                 nqp::what($value),
                 $nominal_target.HOW.name($nominal_target)
               )
             )
            ?? (nqp::istype((my $coerced := $method($value)), $!target_type)
                || nqp::istype($coerced, nqp::gethllsym('Raku', 'Failure'))
               )
              ?? $coerced
              !! self."!invalid_coercion"(
                   $value, $nominal_target.HOW.name($nominal_target), $coerced
                 )
            !! self."!coerce_COERCE"($target, $value, $nominal_target)
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
    method !coerce_COERCE($target, $value, $nominal_target) {
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
          !! self."!coerce_new"($target, $value, $nominal_target)
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
    method !coerce_new($target, $value, $nominal_target) {
        if nqp::defined(
            my $method := nqp::tryfindmethod($nominal_target, 'new')
        ) && nqp::can($method, 'cando')
          && $method.cando($nominal_target, $value) {

            # There should be no significant performance penalty on this path
            # because if method call ever throws then this is going to result
            # in an exception one way or another.
            my $exception;
            my $coerced_value := nqp::null;
            try {
                CATCH {
                    my $exception_obj := nqp::getpayload($!);

                    if $exception_obj.HOW.name($exception_obj)
                      ne 'X::Constructor::Positional' {
                        $exception := $!;
                    }
                }

                # Provide context information to the method 'new'
                my $*COERCION-TYPE := $target;
                $coerced_value := $method($nominal_target, $value);

                return $coerced_value
                  if nqp::istype($coerced_value, $!target_type)
                    || nqp::istype(
                         $coerced_value,
                         nqp::gethllsym('Raku', 'Failure')
                       )
            }

            if nqp::defined($exception) {
                nqp::rethrow($exception);
            }
            elsif nqp::not_i(nqp::isnull($coerced_value)) {
                self."!invalid_coercion"($value, 'new', $coerced_value)
            }
        }

        self."!invalid"($value, "no acceptable coercion method found")
    }

    # Methods needed by Perl6::Metamodel::Nominalizable
    method nominalizable_kind() { 'coercion' }
    method !wrappee($XXX?) { $!target_type }
}

BEGIN {
    my $root := nqp::newtype(Perl6::Metamodel::CoercionHOW, 'Uninstantiable');
    nqp::settypehll($root, 'Raku');
    nqp::setdebugtypename($root, 'CoercionHOW root');
    nqp::setparameterizer(
      $root,
      sub ($type, $params) {
          my $HOW := $type.HOW.new(
            :target_type(    nqp::atpos($params, 0)),
            :constraint_type(nqp::atpos($params, 1))
          );
          my $coercion := nqp::newtype($HOW, 'Uninstantiable');
          nqp::settypehll($coercion, 'Raku');
          $HOW.set_language_version($coercion, :force);
          nqp::setdebugtypename($coercion, $HOW.name($HOW));
          nqp::settypecheckmode(
            $coercion, nqp::const::TYPE_CHECK_NEEDS_ACCEPTS
          );
          $coercion
    });
    nqp::bindkey(Perl6::Metamodel::CoercionHOW.WHO, 'root',  $root);
}

# vim: expandtab sw=4
