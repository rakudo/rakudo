role Perl6::Metamodel::MROMember
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::C3MRO
{
    my &ROLES-REMOTE := nqp::getstaticcode(anon sub ROLES-REMOTE(@self, $obj) {
        @self.veneer($obj.HOW.roles($obj, :local, :!transitive, :!mro))
    });

    my &ROLES-TRANSITIVE := nqp::getstaticcode(anon sub ROLES-TRANSITIVE(@self, $obj) {
        @self.accept($obj).veneer($obj.HOW.roles($obj, :local, :transitive, :!mro))
    });

    my &ROLES-MRO := nqp::getstaticcode(anon sub ROLES-MRO(@self, $obj) {
        @self.accept(nqp::splice([$obj], $obj.HOW.roles($obj, :local, :transitive, :!mro), 1, 0))
    });

    method roles-ordered($obj, @roles, :$local = 1, :$transitive = 1, :$mro = 0) {
        unless $local {
            @roles := nqp::clone(@roles);
            $monic_machine.new.veneer(self.parents($obj, :local)).banish(&ROLES-REMOTE, @roles);
        }
        if $transitive {
            @roles := $monic_machine.new.veneer(@roles);
            @roles := $mro
                ?? @roles.summon(&ROLES-MRO).beckon(nqp::list())
                !! @roles.banish(&ROLES-TRANSITIVE, nqp::list());
        }
        @roles
    }
}
