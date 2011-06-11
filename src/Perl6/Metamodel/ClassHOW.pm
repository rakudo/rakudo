class Perl6::Metamodel::ClassHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::C3MRO
    does Perl6::Metamodel::NonGeneric
    does Perl6::Metamodel::ParrotInterop
{
    has $!composed;

    method new_type(:$name = '<anon>', :$repr = 'P6opaque', :$ver, :$auth) {
        my $metaclass := self.new(:name($name), :ver($ver), :auth($auth));
        self.add_stash(pir::repr_type_object_for__PPS($metaclass, $repr));
    }
    
    my @default_parent_type;
    method set_default_parent_type($type) {
        @default_parent_type[0] := $type;
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
            RoleToClassApplier.apply($obj, @ins_roles)
        }

        # Some things we only do if we weren't already composed once, like
        # building the MRO.
        unless $!composed {
            if self.parents($obj, :local(1)) == 0 && +@default_parent_type && self.name($obj) ne 'Mu' {
                self.add_parent($obj, @default_parent_type[0]);
            }
            self.compute_mro($obj);
            $!composed := 1;
        }

        # Incorporate any new multi candidates (needs MRO built).
        self.incorporate_multi_candidates($obj);

        # Compose attributes.
        for self.attributes($obj, :local) {
            $_.compose($obj);
        }

        # Publish type and method caches.
        self.publish_type_cache($obj);
        self.publish_method_cache($obj);
        
        # Install Parrot v-table mappings.
        self.publish_parrot_vtable_mapping($obj);
		self.publish_parrot_vtable_handler_mapping($obj);

        $obj
    }
    
    # While we normally end up locating methods through the method cache,
    # this is here as a fallback.
    method find_method($obj, $name) {
        my %methods;
        for self.mro($obj) {
            %methods := $_.HOW.method_table($_);
            if pir::exists(%methods, $name) {
                return %methods{$name}
            }
        }
        my %submethods := $obj.HOW.submethod_table($obj);
        if pir::exists(%submethods, $name) {
            return %submethods{$name}
        }
        pir::null__P();
    }
    
    method publish_type_cache($obj) {
        my @tc;
        for self.mro($obj) {
            @tc.push($_);
            # XXX roles also...
        }
        pir::publish_type_check_cache($obj, @tc)
    }

    method publish_method_cache($obj) {
        # Walk MRO and add methods to cache, unless another method
        # lower in the class hierarchy "shadowed" it.
        my %cache;
        for self.mro($obj) {
            my %methods := $_.HOW.method_table($_);
            for %methods {
                unless %cache{$_.key} {
                    %cache{$_.key} := $_.value;
                }
            }
        }
        
        # Also add submethods.
        my %submethods := $obj.HOW.submethod_table($obj);
        for %submethods {
            %cache{$_.key} := $_.value;
        }
        
        pir::publish_method_cache($obj, %cache)
    }
    
    method is_composed($obj) {
        $!composed
    }
    
    method type_check($obj, $checkee) {
        # The only time we end up in here is if the type check cache was
        # not yet published, which means the class isn't yet fully composed.
        # Just hunt through MRO.
        for self.mro($obj) {
            if $_ =:= $checkee {
                return 1;
            }
        }
        0
    }
}
