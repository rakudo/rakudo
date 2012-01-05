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
# list by multi-dispatching over the set of possibilities to pick
# a particular candidate.
my $currier := Perl6::Metamodel::CurriedRoleHOW;
class Perl6::Metamodel::ParametricRoleGroupHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::BoolificationProtocol
{
    has @!possibilities;
    has @!add_to_selector;
    has $!selector;
    has @!role_typecheck_list;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method archetypes() {
        $archetypes
    }
    
    my $selector_creator;
    method set_selector_creator($sc) {
        $selector_creator := $sc;
    }
    
    method new_type(:$name!, :$repr) {
        my $meta := self.new(:name($name), :selector($selector_creator()));
        my $type_obj := self.add_stash(pir::repr_type_object_for__PPS($meta, 'Uninstantiable'));
        $meta.set_boolification_mode($type_obj, 5);
        $meta.publish_boolification_spec($type_obj);
        $type_obj
    }
    
    method parameterize($obj, *@pos_args, *%named_args) {
        $currier.new_type($obj, |@pos_args, |%named_args)
    }
    
    method add_possibility($obj, $possible) {
        @!possibilities[+@!possibilities] := $possible;
        @!add_to_selector[+@!add_to_selector] := $possible;
        self.update_role_typecheck_list($obj);
    }
    
    method specialize($obj, *@pos_args, *%named_args) {
        # Locate correct parametric role and type environment.
        my $error;
        my @result;
        try {
            @result := (self.get_selector($obj))(|@pos_args, |%named_args);
            CATCH { $error := $! }
        }
        if $error {
            pir::die("None of the parametric role variants for '" ~
                self.name($obj) ~ "' matched the arguments supplied.\n" ~
                $error);
        }
        
        # Having picked the appropraite one, specialize it.
        my $prole := @result[0];
        my $type_env := @result[1];
        $prole.HOW.specialize_with($prole, $type_env, @pos_args)
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
        for @!possibilities {
            if !$_.HOW.signatured($_) {
                @!role_typecheck_list := $_.HOW.role_typecheck_list($_);
            }
        }
    }
    
    method role_typecheck_list($obj) {
        @!role_typecheck_list
    }
    
    method type_check($obj, $checkee) {
        my $decont := pir::perl6_decontainerize__PP($checkee);
        if $decont =:= $obj.WHAT {
            return 1;
        }
        for self.prentending_to_be() {
            if $decont =:= pir::perl6_decontainerize__PP($_) {
                return 1;
            }
        }
        for @!role_typecheck_list {
            if $decont =:= pir::perl6_decontainerize__PP($_) {
                return 1;
            }
        }
        0;
    }
}
