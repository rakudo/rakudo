role Perl6::Metamodel::MROMember
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::C3MRO
{
    method visit_roles_in_mro($obj, $roles) {
        my @result := nqp::list();
        for nqp::hllize($roles) -> $role {
            my @role_roles := nqp::hllize($role.HOW.roles($role, :transitive, :!mro));
            my @todo := nqp::clone(@role_roles);
            nqp::unshift(@todo, $role);
            nqp::push(@result, @todo);
        }
        self.c3_merge(@result)
    }
}
