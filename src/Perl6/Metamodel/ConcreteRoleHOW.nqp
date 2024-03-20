class Perl6::Metamodel::ConcreteRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
    does Perl6::Metamodel::Composing
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

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1) );
    method archetypes($XXX?) { $archetypes }

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

    method new_type(:@roles, :$repr, *%_) {
        my $HOW    := self.new(:roles(@roles));
        my $target := nqp::settypehll(nqp::newtype($HOW, 'Uninstantiable'), 'Raku');

        $HOW.set_identity($target, %_);
        $target
    }

    method add_collision($XXX, $colliding_name, @role_names, :$private = 0, :$multi) {
        nqp::push(@!collisions, Collision.new(
            :name($colliding_name), :roles(@role_names), :$private, :$multi
        ));
    }

    method compose($target) {
        $target := nqp::decont($target);

        Perl6::Metamodel::Configuration.role_to_role_applier_type.apply($target, self.roles_to_compose);
        for self.roles_to_compose {
            nqp::push(@!role_typecheck_list, $_);
            for $_.HOW.role_typecheck_list($_) {
                nqp::push(@!role_typecheck_list, $_);
            }
        }
        for @!roles {
            nqp::push(@!role_typecheck_list, $_);
            for $_.HOW.role_typecheck_list($_) {
                nqp::push(@!role_typecheck_list, $_);
            }
        }
        self.publish_type_cache($target);
        self.set_composed;
        $target
    }

    method collisions($XXX?) { @!collisions }

    # It makes sense for concretizations to default to MRO order of roles.
    method roles($XXX?, :$transitive = 1, :$mro = 1) {
        $transitive
          ?? self.roles-ordered(@!roles, :transitive, :$mro)
          !! @!roles
    }

    method add_to_role_typecheck_list($XXX, $type) {
        nqp::push(@!role_typecheck_list, $type);
    }

    method role_typecheck_list($XXX?) { @!role_typecheck_list }

    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);
        if $checkee =:= $target.WHAT {
            return 1;
        }
        for @!role_typecheck_list {
            if nqp::decont($_) =:= $checkee {
                return 1;
            }
        }
        0
    }

    method publish_type_cache($target) {
        my @types := [$target.WHAT];
        for @!role_typecheck_list { @types.push($_) }
        nqp::settypecache($target, @types)
    }

    method mro($target, :$roles, :$concretizations, :$unhidden) {
        [$target]
    }

    method find_method_qualified($target, $qtype, $name) {
        $target := nqp::decont($target);
        $qtype := nqp::decont($qtype);
        if $qtype.HOW.archetypes.parametric {
            my $found-role := nqp::null();
            for self.concretizations($target, :transitive) {
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
        elsif nqp::istype($target, $qtype) {
            # Non-parametric, so just locate it from the already concrete type.
            nqp::findmethod($qtype, $name)
        }
        else {
            nqp::null()
        }
    }

    method is-implementation-detail($target) {
        @!roles[0].is-implementation-detail($target)
    }
}

# vim: expandtab sw=4
