class Perl6::Metamodel::ConcreteRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::PrivateMethodContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::ArrayType
    does Perl6::Metamodel::Concretization
    does Perl6::Metamodel::C3MRO
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
        has $!multi;
        method name() { $!name }
        method roles() { @!roles }
        method private() { $!private }
        method multi() { $!multi }
    }

    method new_type(:@roles, :$name = '<anon>', :$ver, :$auth, :$repr, :$api) {
        my $metarole := self.new(:roles(@roles));
        my $obj := nqp::settypehll(nqp::newtype($metarole, 'Uninstantiable'), 'Raku');
        $metarole.set_name($obj, $name);
        $metarole.set_ver($obj, $ver);
        $metarole.set_auth($obj, $auth) if $auth;
        $metarole.set_api($obj, $api) if $api;
        $obj;
    }

    method add_collision($obj, $colliding_name, @role_names, :$private = 0, :$multi) {
        @!collisions[+@!collisions] := Collision.new(
            :name($colliding_name), :roles(@role_names), :$private, :$multi
        );
    }

    method compose($the-obj) {
        my $obj := nqp::decont($the-obj);

        Perl6::Metamodel::Configuration.role_to_role_applier_type.apply($obj, self.roles_to_compose($obj));
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

    # It makes sense for concretizations to default to MRO order of roles.
    method roles($obj, :$transitive = 1, :$mro = 1) {
        self.roles-ordered($obj, @!roles, :$transitive, :$mro);
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

    method mro($obj, :$roles = 0, :$unhidden = 0) {
        [$obj]
    }

    method find_method_qualified($obj, $qtype, $name) {
        $obj := nqp::decont($obj);
        $qtype := nqp::decont($qtype);
        if $qtype.HOW.archetypes.parametric {
            my $found-role := nqp::null();
            for self.concretizations($obj, :transitive) {
                my $candidate := $_;
                my $role := $_.HOW.roles($_, :!transitive, :!mro)[0];
                if nqp::can($role.HOW, 'group') {
                    $role := $role.HOW.group($role);
                }
                if $qtype =:= $role {
                    # XXX Better be replaced with Exception throwing. The mechanizm could be provided via
                    # Perl6::Metamodel::Configuration where a property could be set pointing to a Raku object.
                    # It could be something like:
                    # Perl6::Metamodel::Configuration.throw("nqp::die message", ['X', 'Method', 'Ambiguous'], |%exception-params);
                    nqp::die("Ambiguous concretization lookup for " ~ $qtype.HOW.name($qtype))
                        unless nqp::isnull($found-role);
                    $found-role := $candidate;
                }
            }
            nqp::isnull($found-role)
              ?? nqp::null()
              !! $found-role.HOW.method_table($found-role){$name}
                   || $found-role.HOW.submethod_table($found-role){$name}
                   || nqp::null()
        }
        elsif nqp::istype($obj, $qtype) {
            # Non-parametric, so just locate it from the already concrete type.
            nqp::findmethod($qtype, $name)
        }
        else {
            nqp::null()
        }
    }

    method is-implementation-detail($obj) {
        @!roles[0].is-implementation-detail($obj)
    }
}

# vim: expandtab sw=4
