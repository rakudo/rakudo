#- Metamodel::SubsetHOW --------------------------------------------------------
# All the logic pertaining to the creation of subsets
class Perl6::Metamodel::SubsetHOW
  does Perl6::Metamodel::Naming
  does Perl6::Metamodel::BUILDALL
  does Perl6::Metamodel::Documenting
  does Perl6::Metamodel::Stashing
  does Perl6::Metamodel::LanguageRevision
  does Perl6::Metamodel::Nominalizable
{
    # The subset type or nominal type that we refine.
    has $!refinee;

    # The block implementing the refinement.
    has $!refinement;

    # Should we preserve pre-6.e behavior?
    has $!pre-e-behavior;

    has $!archetypes;

    method archetypes($XXX?) {
        if nqp::isnull($!archetypes) {
            my $refinee_archetypes := $!refinee.HOW.archetypes($!refinee);
            my $generic := $refinee_archetypes.generic
                            || (nqp::defined($!refinement)
                                && nqp::can($!refinement, 'is_generic')
                                && $!refinement.is_generic);
            $!archetypes := Perl6::Metamodel::Archetypes.new(
                :nominalizable,
                :$generic,
                definite => $refinee_archetypes.definite,
                coercive => $refinee_archetypes.coercive,
            );
        }
        else {
            $!archetypes
        }
    }

    method mro($target, *%_) {
        my @mro := nqp::clone($!refinee.HOW.mro($!refinee, |%_));
        nqp::unshift(@mro, $target);
        @mro
    }

    method BUILD(:$refinee, :$refinement) {
        $!refinee        := $refinee;
        $!refinement     := $refinement;
        $!pre-e-behavior := self.language_revision < 3; # less than 6.e
        $!archetypes     := nqp::null;
    }

    method new_type(:$name = '<anon>', :$refinee!, :$refinement!) {
        my $metasubset := self.new(:$refinee, :$refinement);
        my $type := nqp::settypehll(nqp::newtype($metasubset, 'Uninstantiable'), 'Raku');
        $metasubset.set_name($type, $name);
        $metasubset.set_language_version($metasubset, :force);
        nqp::settypecheckmode($type, nqp::const::TYPE_CHECK_NEEDS_ACCEPTS);
        self.add_stash($type)
    }

    method set_of($XXX, $refinee) {
        $refinee     := nqp::decont($refinee);
        $!archetypes := nqp::null;

        my $archetypes := $refinee.HOW.archetypes($refinee);
        $archetypes.generic
          ?? nqp::die("Use of a generic as 'of' type of a subset is not implemented yet")
          !! nqp::not_i($archetypes.nominalish)
            ?? nqp::die("The 'of' type of a subset must either be a valid nominal type or a type that can provide one")
            !! nqp::objprimspec($refinee)
              ?? Perl6::Metamodel::Configuration.throw_or_die(
                   'X::NYI',
                   "Subsets of native types NYI",
                   :feature('Subsets of native types')
                 )
              !! ($!refinee := $refinee)
    }

    method set_where($XXX, $refinement) {
        $!archetypes := nqp::null;
        $!refinement := nqp::decont($refinement)
    }

    method refinee($XXX?) { $!refinee }

    method refinement($XXX?) { nqp::hllize($!refinement) }

    method isa($XXX, $type) {
        $!refinee.isa($type)
          || nqp::hllboolfor(nqp::eqaddr($type.HOW, self), "Raku")
    }

    method instantiate_generic($target, $type_env) {
        if $!archetypes.generic {
            my $ins_refinee :=
              $!refinee.HOW.instantiate_generic($!refinee, $type_env);

            my $refinement     := $!refinement;
            my $ins_refinement := nqp::isconcrete($refinement)
             && nqp::can($refinement, 'is_generic')
             && $refinement.is_generic
             ?? $refinement.instantiate_generic($type_env)
             !! $refinement;

            self.new_type(
              :name(self.name($target)),
              :refinee($ins_refinee),
              :refinement($ins_refinement)
            )
        }
        else {
            $target
        }
    }

    method nominalize($XXX?) {
        my $refinee := $!refinee;

        $refinee.HOW.archetypes($refinee).nominalizable
          ?? $refinee.HOW.nominalize($refinee)
          !! $refinee
    }

    # Should have the same methods of the (eventually nominal) type
    # that we refine. (For the performance win, work out a way to
    # steal its method cache.)
    method find_method($XXX, $name, *%c) {
        $!refinee.HOW.find_method($!refinee, $name, |%c)
    }

    # Do check when we're on LHS of smartmatch (e.g. Even ~~ Int).
    method type_check($XXX, $checkee) {
        nqp::hllboolfor(
          ($!pre-e-behavior && nqp::eqaddr($checkee.HOW, self))
            || nqp::istype($!refinee, $checkee),
          "Raku"
        )
    }

    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($XXX, $checkee) {
        my $refinee := $!refinee;

        nqp::hllboolfor(
          nqp::istype($checkee, $refinee)
            && (nqp::isnull($!refinement)
                  || nqp::istrue($!refinement.ACCEPTS(
                       $refinee.HOW.archetypes($refinee).coercive
                         ?? $refinee.HOW.coerce($refinee, $checkee)
                         !! $checkee
                     ))
          ),
          "Raku"
        )
    }

    # Methods needed by Perl6::Metamodel::Nominalizable
    method nominalizable_kind() { 'subset' }
    method !wrappee($XXX?) { $!refinee }
}

# vim: expandtab sw=4
