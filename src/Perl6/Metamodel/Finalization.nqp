role Perl6::Metamodel::Finalization {
    has @!destroyers;

    method setup_finalization($obj) {
        my @mro   := self.mro($obj);
        my int $i := -1;
        my int $ocount := nqp::elems(@mro);
        my @destroyers;
        while ++$i < $ocount {
            my $class   := @mro[$i];
            my $classHOW := $class.HOW;
            my $destroy := $classHOW.find_method($class, 'DESTROY', :no_fallback(1));
            if !nqp::isnull($destroy) && $destroy {
                nqp::push(@destroyers, $destroy);
            }
            if !self.lang-rev-before($obj, 'e')
                && nqp::can($classHOW, 'ins_roles')
                && nqp::can($classHOW, 'roles')
            {
                my @ins_roles := $classHOW.ins_roles($class, :with-submethods-only);
                my int $j := -1;
                my int $rcount := nqp::elems(@ins_roles);
                while ++$j < $rcount {
                    my $r := @ins_roles[$j];
                    my $submeth := nqp::atkey(@ins_roles[$j].HOW.submethod_table(@ins_roles[$j]), 'DESTROY');
                    if !nqp::isnull($submeth) && $submeth {
                        nqp::push(@destroyers, $submeth);
                    }
                }
            }
        }
        @!destroyers := @destroyers;
        if @destroyers {
            nqp::settypefinalize($obj, 1);
        }
    }

    method destroyers($obj) {
        @!destroyers
    }
}

# vim: expandtab sw=4
