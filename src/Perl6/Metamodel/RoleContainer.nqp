#- Metamodel::RoleContainer ----------------------------------------------------
# Logic for meta roles and classes that may contain role information
role Perl6::Metamodel::RoleContainer {
    has @!roles_to_compose;

    method add_role($XXX, $role) {
        self.protect({
            my @roles_to_compose := nqp::clone(@!roles_to_compose);
            nqp::push(@roles_to_compose, nqp::decont($role));
            @!roles_to_compose := @roles_to_compose;
        });
    }

    method roles_to_compose($XXX?) { @!roles_to_compose }

    method pop_role_to_compose() {
        self.protect({
            nqp::elems(@!roles_to_compose)
              ?? nqp::pop(@!roles_to_compose)
              !! nqp::null
        })
    }

    method roles-ordered(@roles, :$transitive = 1, :$mro = 0) {
        if $transitive {

            my int $m := nqp::elems(@roles);
            if $mro && nqp::can(self, 'c3_merge') {
                my @result;

                my int $i;
                while $i < $m {
                    my $role := nqp::atpos(@roles, $i);
                    my @r := nqp::list($role);
                    nqp::splice(
                      @r, $role.HOW.roles($role, :transitive), nqp::elems(@r), 0
                    );
                    nqp::push(@result, @r);
                    ++$i;
                }
                self.c3_merge(@result)
            }
            else {
                my @result;

                my int $i;
                while $i < $m {
                    my $role := nqp::atpos(@roles, $i);
                    nqp::push(@result, $role);
                    nqp::splice(
                      @result,
                      $role.HOW.roles($role, :transitive),
                      nqp::elems(@result),
                      0
                    );
                    ++$i;
                }
                @result
            }
        }
        else {
            @roles
        }
    }

    # Helper method to return 1 if any of the types in the given list of types
    # matches the checkee, else 0
    method list_istype_checkee(@types, $checkee) {
        my int $m   := nqp::elems(@types);
        my int $i;
        while $i < $m {
            nqp::istype(nqp::atpos(@types, $i), $checkee)
              ?? (return 1)
              !! ++$i;
        }
        0
    }

    # Helper method to return 1 if the checkee matches the type of any of
    # the types in the given list of types, else 0
    method checkee_istype_list($checkee, @types) {
        my int $m   := nqp::elems(@types);
        my int $i;
        while $i < $m {
            nqp::istype($checkee, nqp::atpos(@types, $i))
              ?? (return 1)
              !! ++$i;
        }
        0
    }

    # Helper method to return 1 if the checkee is the same as the type of any
    # of the types in the given list of types, else 0
    method checkee_eqaddr_list($checkee, @types) {
        my int $m   := nqp::elems(@types);
        my int $i;
        while $i < $m {
            nqp::eqaddr($checkee, nqp::decont(nqp::atpos(@types, $i)))
              ?? (return 1)
              !! ++$i;
        }
        0
    }
}

# vim: expandtab sw=4
