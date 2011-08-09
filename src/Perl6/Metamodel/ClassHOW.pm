class Perl6::Metamodel::ClassHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::PrivateMethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::DefaultParent
    does Perl6::Metamodel::C3MRO
    does Perl6::Metamodel::MROBasedMethodDispatch
    does Perl6::Metamodel::MROBasedTypeChecking
    does Perl6::Metamodel::Trusting
    does Perl6::Metamodel::BUILDPLAN
    does Perl6::Metamodel::Mixins
    does Perl6::Metamodel::ParrotInterop
{
    has @!does_list;
    has @!fallbacks;
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :inheritable(1) );
    method archetypes() {
        $archetypes
    }

    method new_type(:$name = '<anon>', :$repr = 'P6opaque', :$ver, :$auth) {
        my $metaclass := self.new(:name($name), :ver($ver), :auth($auth));
        self.add_stash(pir::repr_type_object_for__PPS($metaclass, $repr));
    }
    
    # Adds a new fallback for method dispatch. Expects the specified
    # condition to have been met (passes it the object and method name),
    # and if it is calls $calculator with the object and method name to
    # calculate an invokable object.
    method add_fallback($obj, $condition, $calculator) {
        my %desc;
        %desc<cond> := $condition;
        %desc<calc> := $calculator;
        @!fallbacks[+@!fallbacks] := %desc;
    }

    method compose($obj) {
        # Instantiate all of the roles we have (need to do this since
        # all roles are generic on ::?CLASS) and pass them to the
        # composer.
        my @roles_to_compose := self.roles_to_compose($obj);
        if @roles_to_compose {
            my @ins_roles;
            while @roles_to_compose {
                my $r := @roles_to_compose.pop();
                @ins_roles.push($r.HOW.specialize($r, $obj))
            }
            @!does_list := RoleToClassApplier.apply($obj, @ins_roles)
        }

        # Some things we only do if we weren't already composed once, like
        # building the MRO.
        unless $!composed {
            if self.parents($obj, :local(1)) == 0 && self.has_default_parent_type && self.name($obj) ne 'Mu' {
                self.add_parent($obj, self.get_default_parent_type);
            }
            self.compute_mro($obj);
            $!composed := 1;
        }

        # Incorporate any new multi candidates (needs MRO built).
        self.incorporate_multi_candidates($obj);

        # Compose attributes.
        self.compose_attributes($obj);

        # Publish type and method caches.
        self.publish_type_cache($obj);
        self.publish_method_cache($obj);
        
        # Install Parrot v-table mappings.
        self.publish_parrot_vtable_mapping($obj);
		self.publish_parrot_vtable_handler_mapping($obj);
        
        # Create BUILDPLAN.
        self.create_BUILDPLAN($obj);

        $obj
    }
    
    method does_list($obj) {
        @!does_list
    }
    
    method is_composed($obj) {
        $!composed
    }
    
    # Maybe this belongs on a role. Also, may be worth memoizing.
    method can($obj, $name) {
        my @meths;
        for self.mro($obj) {
            my %mt := $_.HOW.method_table($_);
            if pir::exists(%mt, $name) {
                @meths.push(%mt{$name})
            }
        }
        @meths
    }
    
    # Stuff for junctiony dispatch fallback.
    my $junction_type;
    my $junction_autothreader;
    method setup_junction_fallback($type, $autothreader) {
        $junction_type := $type;
        $junction_autothreader := $autothreader;
    }
    
    # Handles the various dispatch fallback cases we have.
    method find_method_fallback($obj, $name) {
        # If the object is a junction, need to do a junction dispatch.
        if $obj.WHAT =:= $junction_type && $junction_autothreader {
            my $p6name := pir::perl6ize_type__PP($name);
            return -> *@pos_args, *%named_args {
                $junction_autothreader($p6name, |@pos_args, |%named_args)
            };
        }
        
        # Consider other fallbacks, if we have any.
        for @!fallbacks {
            if ($_<cond>)($obj, $name) {
                return ($_<calc>)($obj, $name);
            }
        }

        # Otherwise, didn't find anything.
        pir::null__P()
    }
}
