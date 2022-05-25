role Perl6::Metamodel::RoleContainer {
    has @!roles_to_compose;

    method add_role($obj, $role) {
        nqp::push(@!roles_to_compose, nqp::decont($role))
    }

    method roles_to_compose($obj) {
        @!roles_to_compose
    }

    method roles-ordered($obj, $roles, :$transitive = 1, :$mro = 0) {
        $transitive
            ?? $mro
                ?? self.visit_roles_in_mro($obj, $roles, nqp::list())
                !! self.visit_roles($obj, $roles, nqp::list())
            !! $roles
    }

    method visit_roles($obj, $roles, @result) {
        for nqp::hllize($roles) -> $role {
            my @role_roles := nqp::hllize($role.HOW.roles($role, :transitive, :!mro));
            nqp::push(@result, $role);
            nqp::splice(@result, @role_roles, nqp::elems(@result), 0);
        }
        @result
    }

    method visit_roles_in_mro($obj, $roles, @result) {
        for nqp::hllize($roles) -> $role {
            my @role_roles := nqp::hllize($role.HOW.roles($role, :transitive, :!mro));
            my @todo := nqp::clone(@role_roles);
            nqp::unshift(@todo, $role);
            nqp::push(@result, @todo);
        }
        self.c3_merge(@result)
    }
}

# vim: expandtab sw=4
