class Perl6::Metamodel::ConcreteRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::NonGeneric
{
    # Any collisions to resolve.
    has @!collisions;
    
    # The parametric role(s) that this concrete one was derived from.
    has @!parametrics;
    
    # Full flat list of "does" roles.
    has @!does_list;
    
    my class Collision {
        has $!name;
        has @!roles;
        method name() { $!name }
        method roles() { @!roles }
    }
    
    method new_type(:@parametrics, :$name = '<anon>', :$ver, :$auth, :$repr) {
        my $metarole := self.new(:parametrics(@parametrics), :name($name), :ver($ver), :auth($auth));
        pir::repr_type_object_for__PPS($metarole, 'Uninstantiable');
    }
    
    method add_collision($obj, $colliding_name, @role_names) {
        @!collisions[+@!collisions] := Collision.new(
            :name($colliding_name), :roles(@role_names)
        );
    }

    method compose($obj) {
        @!does_list := RoleToRoleApplier.apply($obj, self.roles_to_compose($obj));
        for @!parametrics {
            @!does_list.push($_);
        }
        $obj
    }
    
    method collisions($obj) {
        @!collisions
    }
    
    method does_list($obj) {
        @!does_list
    }
}
