role Perl6::Metamodel::Finalization {
    has @!destroyers;

    method setup_finalization($obj) {
        my @mro   := self.mro($obj);
        my int $i := nqp::elems(@mro);
        my @destoryers;
        while --$i >= 0 {
            my $class   := @mro[$i];
            my $destroy := $class.HOW.find_method($class, 'DESTROY', :no_fallback(1));
            if !nqp::isnull($destroy) && $destroy {
                nqp::push(@destoryers, $destroy);
            }
        }
        @!destroyers := @destoryers;
        if @destoryers {
            nqp::settypefinalize($obj, 1);
        }
    }

    method destroyers($obj) {
        @!destroyers
    }
}
