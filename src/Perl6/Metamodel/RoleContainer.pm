role Perl6::Metamodel::RoleContainer {
    has @!roles_to_compose;
    
    method add_role($obj, $role) {
        @!roles_to_compose[+@!roles_to_compose] := $role
    }
    
    method roles_to_compose($obj) {
        @!roles_to_compose
    }
}
