role Perl6::Metamodel::Finalization {
    has @!destroyers;

    method setup_finalization($obj) {
        my @mro   := self.mro($obj);
        my int $i := nqp::elems(@mro);
        my @destroyers;
        while --$i >= 0 {
            my $class   := @mro[$i];
            my $destroy := $class.HOW.find_method($class, 'DESTROY', :no_fallback(1));
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
