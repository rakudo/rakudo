class Perl6::Metamodel::SubsetHOW
    does Perl6::Metamodel::Naming
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

    method archetypes() {
        unless nqp::isconcrete($!archetypes) {
            my $refinee_archetypes := $!refinee.HOW.archetypes;
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
        $!archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method BUILD(:$refinee, :$refinement) {
        $!refinee := $refinee;
        $!refinement := $refinement;
        $!pre-e-behavior := self.lang-rev-before(self, 'e');
    }

    method new_type(:$name = '<anon>', :$refinee!, :$refinement!) {
        my $metasubset := self.new(:$refinee, :$refinement);
        my $type := nqp::settypehll(nqp::newtype($metasubset, 'Uninstantiable'), 'Raku');
        $metasubset.set_name($type, $name);
        $metasubset.set_language_version($metasubset, :force);
        nqp::settypecheckmode($type, 2);
        self.add_stash($type)
    }

    method set_of($obj, $refinee) {
        my $archetypes := $refinee.HOW.archetypes;
        if $archetypes.generic {
            nqp::die("Use of a generic as 'of' type of a subset is not implemented yet")
        }
        unless $archetypes.nominal || $archetypes.nominalizable {
            nqp::die("The 'of' type of a subset must either be a valid nominal " ~
                "type or a type that can provide one");
        }
        $!refinee := nqp::decont($refinee);
        if nqp::objprimspec($!refinee) {
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::NYI',
                "Subsets of native types NYI",
                :feature(nqp::hllizefor('Subsets of native types', 'Raku'))
            );
        }
    }

    method refinee($obj) {
        $!refinee
    }

    method refinement($obj) {
        nqp::hllize($!refinement)
    }

    method isa($obj, $type) {
        $!refinee.isa($type)
            || nqp::hllboolfor(nqp::istrue($type.HOW =:= self), "Raku")
    }

    method instantiate_generic($obj, $type_env) {
        return $obj unless $!archetypes.generic;
        my $ins_refinee := $!refinee.HOW.instantiate_generic($!refinee, $type_env);
        my $ins_refinement := $!refinement;
        if nqp::isconcrete($!refinement) {
            if nqp::can($!refinement, 'is_generic') && $!refinement.is_generic {
                $ins_refinement := $!refinement.instantiate_generic($type_env);
            }
        }
        self.new_type(:name(self.name($obj)), :refinee($ins_refinee), :refinement($ins_refinement))
    }

    method nominalize($obj) {
        $!refinee.HOW.archetypes.nominalizable
            ?? $!refinee.HOW.nominalize($!refinee)
            !! $!refinee
    }

    # Should have the same methods of the (eventually nominal) type
    # that we refine. (For the performance win, work out a way to
    # steal its method cache.)
    method find_method($obj, $name) {
        $!refinee.HOW.find_method($!refinee, $name)
    }

    # Do check when we're on LHS of smartmatch (e.g. Even ~~ Int).
    method type_check($obj, $checkee) {
        nqp::hllboolfor(
            ($!pre-e-behavior && nqp::istrue($checkee.HOW =:= self))
                || nqp::istype($!refinee, $checkee),
            "Raku"
        )
    }

    # Here we check the value itself (when on RHS on smartmatch).
    method accepts_type($obj, $checkee) {
        nqp::hllboolfor(
            nqp::istype($checkee, $!refinee) &&
            (nqp::isnull($!refinement)
             || ($!refinee.HOW.archetypes.coercive
                    ?? nqp::istrue($!refinement.ACCEPTS($!refinee.HOW.coerce($!refinee, $checkee)))
                    !! nqp::istrue($!refinement.ACCEPTS($checkee)))),
            "Raku")
    }

    # Methods needed by Perl6::Metamodel::Nominalizable
    method nominalizable_kind() { 'subset' }
    method !wrappee($obj) { $!refinee }
}

# vim: expandtab sw=4
