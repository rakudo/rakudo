role Perl6::Metamodel::RoleContainer {
    has @!roles_to_compose;

    method add_role($obj, $role) {
        nqp::push(@!roles_to_compose, nqp::decont($role))
    }

    method roles_to_compose($obj) {
        @!roles_to_compose
    }

    my &ROLES-TRANSITIVE := nqp::getstaticcode(anon sub ROLES-TRANSITIVE(@self, $obj) {
        @self.accept($obj).veneer($obj.HOW.roles($obj, :transitive, :!mro))
    });

    my &ROLES-MRO := nqp::getstaticcode(anon sub ROLES-MRO(@self, $obj) {
        @self.accept(nqp::splice([$obj], $obj.HOW.roles($obj, :transitive, :!mro), 1, 0))
    });

    method roles-ordered($obj, @roles, :$transitive = 1, :$mro = 0) {
        if $transitive {
            @roles := $monic_machine.new.veneer(@roles);
            @roles := $mro
                ?? @roles.summon(&ROLES-MRO).beckon(nqp::list())
                !! @roles.banish(&ROLES-TRANSITIVE, nqp::list());
        }
        @roles
    }
}

# vim: expandtab sw=4
