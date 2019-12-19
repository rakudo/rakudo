role Perl6::Metamodel::Finalization {
    has @!destroyers;

    method setup_finalization($obj) {
        my @mro   := self.mro($obj);
        my int $i := nqp::elems(@mro);
        my @destroyers;
        while --$i >= 0 {
            my $class   := @mro[$i];
            my $classHOW := $class.HOW;
            if !self.lang-rev-before($obj, 'e')
                && nqp::can($classHOW, 'ins_roles')
                && nqp::can($classHOW, 'roles')
            {
                my @ins_roles := $classHOW.ins_roles($class, :with-submethods-only);
                my $i := +@ins_roles;
                while --$i >= 0 {
                    my $submeth := nqp::atkey(@ins_roles[$i].HOW.submethod_table(@ins_roles[$i]), 'DESTROY');
                    if !nqp::isnull($submeth) && $submeth {
                        nqp::push(@destroyers, $submeth);
                    }
                }
            }
            my $destroy := $classHOW.find_method($class, 'DESTROY', :no_fallback(1));
            if !nqp::isnull($destroy) && $destroy {
                nqp::push(@destroyers, $destroy);
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
