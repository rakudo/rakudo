role Perl6::Metamodel::MROMember
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::C3MRO
{
    method roles-ordered($obj, $roles, :$local = 0, :$transitive = 1, :$mro = 0) {
        $local
            ?? $transitive
                ?? $mro
                    ?? self.visit_roles_in_mro($obj, $roles, nqp::list())
                    !! self.visit_roles($obj, $roles, nqp::list())
                !! $roles
            !! self.visit_roles_by_mro($obj, $roles, nqp::list(), :$transitive, :$mro)
    }

    method visit_roles_in_mro($obj, $roles, @result) {
        for nqp::hllize($roles) -> $role {
            my @role_roles := nqp::hllize($role.HOW.roles($role, :local, :transitive, :!mro));
            my @todo := nqp::clone(@role_roles);
            nqp::unshift(@todo, $role);
            nqp::push(@result, @todo);
        }
        self.c3_merge(@result)
    }

    method visit_roles_by_mro($obj, $roles, @result, :$transitive!, :$mro!) {
        my @mro := self.mro($obj);
        my uint $i := nqp::elems(@mro);
        my uint $n := nqp::elems(@result);
        nqp::splice(@result,
            nqp::hllize(@mro[$i].HOW.roles(@mro[$i], :local, :$transitive, :$mro)),
            $n, 0) while $i--;
        @result
    }
}
