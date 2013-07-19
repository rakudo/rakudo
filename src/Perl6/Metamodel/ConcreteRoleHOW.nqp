class Perl6::Metamodel::ConcreteRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::PrivateMethodContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::ArrayType
{
    # Any collisions to resolve.
    has @!collisions;
    
    # The (parametric) role(s) that this concrete one was directly derived
    # from.
    has @!roles;
    
    # Full flat list of done roles.
    has @!role_typecheck_list;
    
    # Are we composed yet?
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1) );
    method archetypes() {
        $archetypes
    }
    
    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }
    
    my class Collision {
        has $!name;
        has @!roles;
        has $!private;
        method name() { $!name }
        method roles() { @!roles }
        method private() { $!private }
    }
    
    method new_type(:@roles, :$name = '<anon>', :$ver, :$auth, :$repr) {
        my $metarole := self.new(:roles(@roles));
        my $obj := nqp::settypehll(nqp::newtype($metarole, 'Uninstantiable'), 'perl6');
        $metarole.set_name($obj, $name);
        $metarole.set_ver($obj, $ver) if $ver;
        $metarole.set_auth($obj, $auth) if $auth;
        $obj;
    }
    
    method add_collision($obj, $colliding_name, @role_names, :$private = 0) {
        @!collisions[+@!collisions] := Collision.new(
            :name($colliding_name), :roles(@role_names), :private($private)
        );
    }

    method compose($obj) {
        RoleToRoleApplier.apply($obj, self.roles_to_compose($obj));
        for self.roles_to_compose($obj) {
            @!role_typecheck_list[+@!role_typecheck_list] := $_;
            for $_.HOW.role_typecheck_list($_) {
                @!role_typecheck_list[+@!role_typecheck_list] := $_;
            }
        }
        for @!roles {
            @!role_typecheck_list[+@!role_typecheck_list] := $_;
            for $_.HOW.role_typecheck_list($_) {
                @!role_typecheck_list[+@!role_typecheck_list] := $_;
            }
        }
        self.publish_type_cache($obj);
        $!composed := 1;
        $obj
    }
    
    method is_composed($obj) {
        $!composed ?? 1 !! 0
    }
    
    method collisions($obj) {
        @!collisions
    }
    
    method roles($obj, :$transitive) {
        if $transitive {
            my @trans;
            for @!roles {
                @trans.push($_);
                for $_.HOW.roles($_) {
                    @trans.push($_);
                }
            }
        }
        else {
            @!roles
        }
    }
    
    method add_to_role_typecheck_list($obj, $type) {
        @!role_typecheck_list[+@!role_typecheck_list] := $type;
    }
    
    method role_typecheck_list($obj) {
        @!role_typecheck_list
    }
    
    method type_check($obj, $checkee) {
        my $decont := nqp::decont($checkee);
        if $decont =:= $obj.WHAT {
            return 1;
        }
        for @!role_typecheck_list {
            if nqp::decont($_) =:= $decont {
                return 1;
            }
        }
        0
    }
    
    method publish_type_cache($obj) {
        my @types := [$obj.WHAT];
        for @!role_typecheck_list { @types.push($_) }
        nqp::settypecache($obj, @types)
    }
    
    method mro($obj) {
        [$obj]
    }
}
