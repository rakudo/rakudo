role Perl6::Metamodel::RoleContainer {
    has @!roles_to_compose;

    method add_role($obj, $role) {
        @!roles_to_compose[+@!roles_to_compose] := nqp::decont($role)
    }

    method roles_to_compose($obj) {
        @!roles_to_compose
    }

    method roles-ordered($obj, @roles, :$transitive = 1, :$mro = 0) {
        if $transitive {
            my @result;
            $mro := nqp::can(self, 'c3_merge') if $mro;
            for @roles {
                my @r := $mro ?? [] !! @result;
                nqp::push(@r, $_);
                for $_.HOW.roles($_, :transitive) {
                    nqp::push(@r, $_);
                }
                nqp::push(@result, @r) if $mro;
            }
            $mro  ?? self.c3_merge(@result) !! @result;
        }
        else {
            @roles
        }
    }
}

# vim: expandtab sw=4
