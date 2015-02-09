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
{
    has @!candidates;
    has @!add_to_selector;
    has $!selector;
    has @!role_typecheck_list;

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
        my $meta := self.new(:selector($selector_creator()));
        my $type_obj := self.add_stash(nqp::settypehll(
            nqp::newtype($meta, 'Uninstantiable'), 'perl6'));
        $meta.set_name($type_obj, $name);
        $meta.set_pun_repr($meta, $repr) if $repr;
        $meta.set_boolification_mode($type_obj, 5);
        $meta.publish_boolification_spec($type_obj);
        $type_obj
    }
    
    method parameterize($obj, *@pos_args, *%named_args) {
        my $curried := $currier.new_type($obj, |@pos_args, |%named_args);
        $curried.HOW.set_pun_repr($curried, self.pun_repr($obj));
        $curried
    }
    
    method add_possibility($obj, $possible) {
        @!candidates[+@!candidates] := $possible;
        @!add_to_selector[+@!add_to_selector] := $possible;
        self.update_role_typecheck_list($obj);
    }
    
    method specialize($obj, *@pos_args, *%named_args) {
        # Use multi-dispatcher to pick the body block of the best role.
        my $error;
        my $selected_body;
        try {
            sub try_select(*@pos, *%named) {
                self.get_selector($obj).find_best_dispatchee(nqp::usecapture(), 0)
            }
            $selected_body := try_select(|@pos_args, |%named_args);
            CATCH { $error := $! }
        }
        if $error {
            my %ex := nqp::gethllsym('perl6', 'P6EX');
            if nqp::existskey(%ex, 'X::Role::Parametric::NoSuchCandidate') {
                %ex{'X::Role::Parametric::NoSuchCandidate'}($obj);
            }
            nqp::die("Could not find an appropriate parametric role variant for '" ~
                self.name($obj) ~ "' using the arguments supplied.\n" ~
                $error);
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

        # Having picked the appropriate one, specialize it.
        $selected.HOW.specialize($selected, |@pos_args, |%named_args);
    }
    
    method get_selector($obj) {
        if @!add_to_selector {
            for @!add_to_selector {
                $!selector.add_dispatchee($_.HOW.body_block($_));
            }
            @!add_to_selector := [];
        }
        $!selector
    }
    
    method update_role_typecheck_list($obj) {
        for @!candidates {
            if !$_.HOW.signatured($_) {
                @!role_typecheck_list := $_.HOW.role_typecheck_list($_);
            }
        }
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

    method !get_default_candidate($obj) {
        @!candidates[0]
    }
}
