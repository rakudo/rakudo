#- Metamodel::Finalization -----------------------------------------------------
role Perl6::Metamodel::Finalization {
    has @!destroyers;

    method setup_finalization($target) {
        my @mro := self.mro($target);
        my @destroyers;

        my int $m := nqp::elems(@mro);
        my int $i;
        while $i < $m {
            my $class   := nqp::atpos(@mro, $i);
            my $HOW     := $class.HOW;
            my $DESTROY := $HOW.find_method($class, 'DESTROY', :no_fallback);
            nqp::push(@destroyers, $DESTROY) unless nqp::isnull($DESTROY);

            if self.language_revision >= 3
                && nqp::can($HOW, 'ins_roles')
                && nqp::can($HOW, 'roles')
            {
                my @roles := $HOW.ins_roles($class, :with-submethods-only);

                my int $n := nqp::elems(@roles);
                my int $j;
                while $j < $n {
                    my $role    := nqp::atpos(@roles, $j);
                    my $DESTROY := nqp::atkey(
                      $role.HOW.submethod_table($role), 'DESTROY'
                    );
                    nqp::push(@destroyers, $DESTROY)
                      unless nqp::isnull($DESTROY);

                    ++$j;
                }
            }

            ++$i;
        }

        if nqp::elems(@destroyers) {
            @!destroyers := @destroyers;
            nqp::settypefinalize($target, 1);
        }
    }

    method destroyers($XXX?) { @!destroyers }
}

# vim: expandtab sw=4
