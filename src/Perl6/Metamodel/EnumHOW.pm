# This is the meta-object for an enumeration (declared with enum).
# It keeps hold of the enumeration values in an EnumMap, which is
# created at composition time. It supports having roles composed in,
# one or two of which presumably provide the core enum-ish methods.
class Perl6::Metamodel::EnumHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::BaseType
    does Perl6::Metamodel::MROBasedMethodDispatch
    does Perl6::Metamodel::MROBasedTypeChecking
    does Perl6::Metamodel::BUILDPLAN
    does Perl6::Metamodel::NonGeneric
    does Perl6::Metamodel::ParrotInterop
{
    # Hash representing enumeration keys to values.
    has %!values;
    
    # Roles that we do.
    has @!does_list;
    
    method new_type(:$name!, :$base_type!) {
        my $meta := self.new(:name($name));
        my $obj  := pir::repr_type_object_for__PPS($meta, 'P6opaque');
        $meta.set_base_type($meta, $base_type);
        self.add_stash($obj);
    }
    
    method add_enum_value($obj, $key, $value) {
        %!values{$key} := $value;
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
        
        # Create BUILDPLAN.
        self.create_BUILDPLAN($obj);
        
        # Setup the actual enumeration values.
        for %!values {
            # Create value object, and shove key in it too.
            my $key_obj   := pir::perl6_box_str__PS($_.key);
            my $value_obj := pir::repr_box_int__PIP($_.value, $obj);
            nqp::bindattr($value_obj, $obj, '$!key', $key_obj);
            
            # Add to stash.
            ($obj.WHO){$_.key} := $value_obj;
        }

        $obj
    }
    
    method does_list($obj) {
        @!does_list
    }
}
