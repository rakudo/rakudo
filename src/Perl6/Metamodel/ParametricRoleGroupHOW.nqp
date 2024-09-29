#- Metamodel::ParametricRoleGroupHOW -------------------------------------------
# This represents a group of parametric roles. For example, given
# we have the declarations:
#
#   role Foo[] { } # (which is same as role Foo { })
#   role Foo[::T] { }
#   role Foo[::T1, ::T2] { }
#
# Each of them results in a type object that has a HOW of type
# Perl6::Metamodel::ParametricRoleHOW. In here, we keep the whole
# group of those, and know how to specialize to a certain parameter
# list by multi-dispatching over the set of candidates to pick
# a particular candidate.

class Perl6::Metamodel::ParametricRoleGroupHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::BoolificationProtocol
#?if !moar
    does Perl6::Metamodel::InvocationProtocol
#?endif
{
    has @!candidates;
    has $!selector;
    has @!role_typecheck_list;
    has @!nonsignatured;

    my $archetypes := Perl6::Metamodel::Archetypes.new(
      :nominal, :composable, :inheritalizable, :parametric
    );
    method archetypes($XXX?) { $archetypes }

    my $selector_creator;
    method set_selector_creator($sc) { $selector_creator := $sc }

    method new_type(:$name!, :$repr) {
        # Build and configure the type's basic details.
        my $HOW  := self.new(:selector($selector_creator()));
        my $type := nqp::newtype($HOW, 'Uninstantiable');
        nqp::settypehll($type, 'Raku');

        $HOW.set_name($type, $name);
        $HOW.set_pun_repr($type, $repr) if $repr;
        $HOW.set_boolification_mode($type, 5);
        $HOW.publish_boolification_spec($type);
        $HOW.publish_type_cache($type);
        self.add_stash($type);

        # We use 6model parametrics to make this a parametric type on the
        # arguments we curry with. This means we'll make the curries unique.
        nqp::setparameterizer($type, sub ($target, @packed) {
            my @_ := nqp::clone(@packed);
            my %_ := nqp::pop(@_) || nqp::hash;
            my $curried :=
              Perl6::Metamodel::CurriedRoleHOW.new_type($target, |@_, |%_);
            $curried.HOW.set_pun_repr($curried, $target.HOW.pun_repr($target));
            $curried
        });

#?if !moar
        $HOW.compose_invocation($type);
#?endif

        $type
    }

    # We only take positional args into account for the parametric key. If
    # there are no nameds, we push this class in place of them so as to make
    # an otherwise equal key always the same, and named args make it always
    # unequal.
    my class NO_NAMEDS { }

    method parameterize($target, *@_, *%_) {
        my int $m := nqp::elems(@_);
        my int $i;
        while $i < $m {
            nqp::bindpos(@_, $i, nqp::decont(nqp::atpos(@_, $i)));
            ++$i;
        }
        nqp::push(@_, %_ || NO_NAMEDS);
        nqp::parameterizetype($target, @_);
    }

    method add_possibility($target, $possible) {
        $possible := nqp::decont($possible);

        self.protect({
            my @candidates := nqp::clone(@!candidates);
            nqp::push(@candidates, $possible);
            @!candidates := @candidates;

            unless $possible.HOW.signatured($possible) {
                my @nonsignatured := nqp::clone(@!nonsignatured);
                nqp::push(@nonsignatured, $possible);
                @!nonsignatured := @nonsignatured;
            }

            $!selector.add_dispatchee($possible.HOW.body_block($possible));
            self.update_role_typecheck_list($target);
        });
    }

    method select_candidate($target, @_, %_) {
        # Use multi-dispatcher to pick the body block of the best role.
        my $selected_body;
        my $error;
        try {
            sub try_select(*@_, *%_) {
                $!selector.find_best_dispatchee(nqp::usecapture, 0)
            }
            $selected_body := try_select(|@_, |%_);
            CATCH { $error := $! }
        }
        self.no_such_candidate($target, $error) if $error;

        # Locate the role that has that body block.
        my @candidates := @!candidates;
        my int $m := nqp::elems(@candidates);
        my int $i;
        while $i < $m {
            my $candidate := nqp::atpos(@candidates, $i);
            nqp::eqaddr($candidate.HOW.body_block($candidate), $selected_body)
              ?? (return $candidate)
              !! ++$i;
        }

        nqp::die("Internal error: could not resolve body block to role candidate");
    }

    method specialize($target, *@_, *%_) {
        my $selected := self.select_candidate($target, @_, %_);

        # Having picked the appropriate one, specialize it.
        $selected.HOW.specialize($selected, |@_, |%_)
    }

    method update_role_typecheck_list($XXX?) {
        my $ns := nqp::atpos(@!nonsignatured, 0);
        @!role_typecheck_list := $ns.HOW.role_typecheck_list($ns)
          unless nqp::isnull($ns);
    }

    method role_typecheck_list($XXX?) { @!role_typecheck_list }

    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);

        nqp::eqaddr($checkee, $target.WHAT)
          || self.checkee_eqaddr_list($checkee, self.pretending_to_be)
          || self.checkee_eqaddr_list($checkee, @!role_typecheck_list)
          || (nqp::isnull(my $ns := nqp::atpos(@!nonsignatured, 0))
               ?? 0
               !! $ns.HOW.type_check_parents($ns, $checkee)
             )
    }

    method candidates($XXX?) { nqp::clone(@!candidates) }

    method lookup($XXX, $name) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::list
          !! $c.HOW.lookup($c, $name)
    }

    method methods($XXX, *@_, *%_) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::list
          !! $c.HOW.methods($c, |@_, |%_)
    }

    method attributes($XXX, *@_, *%_) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::list
          !! $c.HOW.attributes($c, |@_, |%_)
    }

    method parents($XXX, *%_) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::list
          !! $c.HOW.parents($c, |%_)
    }

    method roles($XXX, *%_) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::list
          !! $c.HOW.roles($c, |%_)
    }

    method ver($XXX?) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::null
          !! $c.HOW.ver($c)
    }

    method auth($XXX?) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::null
          !! $c.HOW.auth($c)
    }

    method api($XXX?) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::null
          !! $c.HOW.api($c)
    }

    # See Perl6::Metamodel::LanguageRevision role comments about the
    # difference between these two language revision methods.
    method language-revision($XXX?) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::null
          !! $c.HOW.language-revision($c)
    }

    method language_revision($XXX?) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::null
          !! $c.HOW.language_revision($c)
    }

    method is-implementation-detail($XXX?) {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::null
          !! $c.HOW.is-implementation-detail($c)
    }

    method WHY() {
        nqp::isnull(my $c := nqp::atpos(@!nonsignatured, 0))
          ?? nqp::null
          !! $c.HOW.WHY
    }

    method set_why($why) {
        my str $role-name := self.name;

        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Group::Documenting',
          "Parametric role group cannot be documented, use one of the candidates instead for '"
            ~ $role-name
            ~ "'",
          :$role-name
        );
    }

    method publish_type_cache($target) {

        # We can at least include ourself and the types a role pretends to be.
        my @tc := nqp::clone(self.pretending_to_be);
        nqp::push(@tc, $target.WHAT);

        nqp::settypecache($target, @tc);
        nqp::settypecheckmode(
          $target,
          nqp::const::TYPE_CHECK_CACHE_THEN_METHOD
        );
    }

    # Helper methods for error handling
    method no_such_candidate($role, $error) {
        my $payload := nqp::getpayload($error);
        my $hint    := nqp::getmessage($error)
          || (nqp::defined($payload) ?? $payload.message !! "");

        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Parametric::NoSuchCandidate',
          "Could not find an appropriate parametric role variant for '"
            ~ self.name($role)
            ~ "' using the arguments supplied:\n    "
            ~ $hint,
          :$role, :$hint
        );
    }
}

# vim: expandtab sw=4
