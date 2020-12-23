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
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::BoolificationProtocol
    does Perl6::Metamodel::InvocationProtocol
{
    has @!candidates;
    has $!selector;
    has @!role_typecheck_list;
    has @!nonsignatured;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    my $selector_creator;
    method set_selector_creator($sc) {
        $selector_creator := $sc;
    }

    method new_type(:$name!, :$repr) {
        # Build and configure the type's basic details.
        my $meta := self.new(:selector($selector_creator()));
        my $type_obj := nqp::settypehll(nqp::newtype($meta, 'Uninstantiable'), 'Raku');
        $meta.set_name($type_obj, $name);
        $meta.set_pun_repr($meta, $repr) if $repr;
        $meta.set_boolification_mode($type_obj, 5);
        $meta.publish_boolification_spec($type_obj);
        self.add_stash($type_obj);

        # We use 6model parametrics to make this a parametric type on the
        # arguments we curry with. This means we'll make the curries unique.
        nqp::setparameterizer($type_obj, sub ($type, @packed) {
            $type.HOW.'!produce_parameterization'($type, @packed);
        });

        $meta.compose_invocation($type_obj);

        $type_obj
    }

    # We only take positional args into account for the parametric key. If
    # there are no nameds, we push this class in place of them so as to make
    # an otherwise equal key always the same, and named args make it always
    # unequal.
    my class NO_NAMEDS { }

    method parameterize($obj, *@args, *%named_args) {
        my int $n := nqp::elems(@args);
        my int $i := -1;
        while ++$i < $n {
            @args[$i] := nqp::decont(@args[$i]);
        }
        nqp::push(@args, %named_args || NO_NAMEDS);
        nqp::parameterizetype($obj, @args);
    }

    method !produce_parameterization($obj, @packed) {
        my @args := nqp::clone(@packed);
        my $maybe_nameds := nqp::pop(@args);
        my $curried;
        if $maybe_nameds {
            my %nameds := $maybe_nameds;
            $curried := $currier.new_type($obj, |@args, |%nameds);
        }
        else {
            $curried := $currier.new_type($obj, |@args);
        }
        $curried.HOW.set_pun_repr($curried, self.pun_repr($obj));
        $curried
    }

    method add_possibility($obj, $possible) {
        @!candidates[+@!candidates] := $possible;
        nqp::push(@!nonsignatured, nqp::decont($possible)) unless $possible.HOW.signatured($possible);
        $!selector.add_dispatchee($possible.HOW.body_block($possible));
        self.update_role_typecheck_list($obj);
    }

    method select_candidate($obj, @pos_args, %named_args) {
        # Use multi-dispatcher to pick the body block of the best role.
        my $error;
        my $selected_body;
        try {
            sub try_select(*@pos, *%named) {
                $!selector.find_best_dispatchee(nqp::usecapture(), 0)
            }
            $selected_body := try_select(|@pos_args, |%named_args);
            CATCH { $error := $! }
        }

        if $error {
            my $payload := nqp::getpayload($error);
            my $hint := nqp::getmessage($error) || (nqp::defined($payload) ?? $payload.message !! "");
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Role::Parametric::NoSuchCandidate',
                "Could not find an appropriate parametric role variant for '"
                    ~ $obj.HOW.name($obj) ~ "' using the arguments supplied:\n    "
                    ~ $hint
                    ,
                :role($obj), :$hint
            );
        }

        # Locate the role that has that body block.
        my $selected := NQPMu;
        for @!candidates {
            if $_.HOW.body_block($_) =:= $selected_body {
                $selected := $_;
                last;
            }
        }
        if $selected =:= NQPMu {
            nqp::die("Internal error: could not resolve body block to role candidate");
        }
        $selected
    }

    method specialize($obj, *@pos_args, *%named_args) {
        my $selected := self.select_candidate($obj, @pos_args, %named_args);
        # Having picked the appropriate one, specialize it.
        $selected.HOW.specialize($selected, |@pos_args, |%named_args);
    }

    method update_role_typecheck_list($obj) {
        my $ns := self.'!get_nonsignatured_candidate'($obj);
        @!role_typecheck_list := $ns.HOW.role_typecheck_list($ns) unless nqp::isnull($ns);
    }

    method role_typecheck_list($obj) {
        @!role_typecheck_list
    }

    method type_check($obj, $checkee) {
        my $decont := nqp::decont($checkee);
        if $decont =:= $obj.WHAT {
            return 1;
        }
        for self.pretending_to_be() {
            if $decont =:= nqp::decont($_) {
                return 1;
            }
        }
        for @!role_typecheck_list {
            if $decont =:= nqp::decont($_) {
                return 1;
            }
        }
        my $ns := self.'!get_nonsignatured_candidate'($obj);
        return $ns.HOW.type_check_parents($ns, $decont) unless nqp::isnull($ns);
        0;
    }

    method candidates($obj) { nqp::clone(@!candidates) }

    method lookup($obj, $name) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.lookup($c, $name);
    }

    method methods($obj, *@pos, *%name) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.methods($c, |@pos, |%name);
    }

    method attributes($obj, *@pos, *%name) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.attributes($c, |@pos, |%name);
    }

    method parents($obj, *%named) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.parents($c, |%named)
    }

    method roles($obj, *%named) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.roles($c, |%named)
    }

    method ver($obj) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.ver($c)
    }

    method auth($obj) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.auth($c)
    }

    method language-revision($obj) {
        my $c := self.'!get_default_candidate'($obj);
        nqp::unless(nqp::isnull($c),
                    $c.HOW.language-revision($c),
                    nqp::null())
    }

    method is-implementation-detail($obj) {
        my $c := self.'!get_default_candidate'($obj);
        $c.HOW.is-implementation-detail($c)
    }

    method !get_default_candidate($obj) {
        nqp::ifnull(self.'!get_nonsignatured_candidate'($obj),
                    nqp::if(
                        +@!candidates,
                        @!candidates[0],
                        nqp::null()))
    }

    method !get_nonsignatured_candidate($obj) {
        return nqp::null unless +@!nonsignatured;
        @!nonsignatured[0]
    }
}

# vim: expandtab sw=4
