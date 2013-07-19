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
    does Perl6::Metamodel::BoolificationProtocol
    does Perl6::Metamodel::REPRComposeProtocol
    does Perl6::Metamodel::InvocationProtocol
#?if parrot
    does Perl6::Metamodel::ParrotInterop
#?endif
{
    # Hash representing enumeration keys to values.
    has %!values;
    
    # Reverse mapping hash.
    has %!value_to_enum;
    
    # List of enum values (actual enum objects).
    has @!enum_value_list;
    
    # Roles that we do.
    has @!does_list;
    
    # Role'd version of the enum.
    has $!role;
    has int $!roled;
    
    # Are we composed yet?
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composalizable(1) );
    method archetypes() {
        $archetypes
    }
    
    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }
    
    method new_type(:$name!, :$base_type!) {
        my $meta := self.new();
        my $obj  := nqp::settypehll(nqp::newtype($meta, 'P6opaque'), 'perl6');
        $meta.set_name($obj, $name);
        $meta.set_base_type($meta, $base_type);
        self.add_stash($obj);
    }
    
    method add_enum_value($obj, $value) {
        %!values{nqp::unbox_s($value.key)} := $value.value;
        @!enum_value_list[+@!enum_value_list] := $value;
    }
    
    method enum_values($obj) {
        %!values
    }
    
    method enum_from_value($obj, $value) {
        unless %!value_to_enum {
            for @!enum_value_list {
                %!value_to_enum{$_.value} := $_;
            }
        }
        nqp::existskey(%!value_to_enum, $value)
            ?? %!value_to_enum{$value}
            !! $obj.WHAT;
    }
    
    method enum_value_list($obj) {
        @!enum_value_list
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
        
#?if parrot
        # Install Parrot v-table mappings.
        self.publish_parrot_vtable_mapping($obj);
		self.publish_parrot_vtable_handler_mapping($obj);
#?endif
        
        # Publish boolification spec.
        self.publish_boolification_spec($obj);
        
        # Create BUILDPLAN.
        self.create_BUILDPLAN($obj);
        
        # Compose the representation.
        unless $!composed {
            self.compose_repr($obj);
            $!composed := 1;
        }
        
        # Compose invocation protocol.
        self.compose_invocation($obj);

        $obj
    }
    
    my $composalizer;
    method set_composalizer($c) { $composalizer := $c }
    method composalize($obj) {
        unless $!roled {
            $!role := $composalizer($obj, self.name($obj), %!values);
            $!roled := 1;
        }
        $!role
    }
    
    method does_list($obj) {
        @!does_list
    }
}
