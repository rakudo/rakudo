class List {
    # has Mu $!items    # BOOTSTRAP.pm
    # has Mu $!nextiter # BOOTSTRAP.pm

    method gimme($n) {
        pir::defined($!items) or 
            pir::setattribute__3PPsP(self, List, '$!items', pir::new__Ps('ResizablePMCArray'));
        my $count = pir::perl6_box_int__PI(pir::elements($!items));
        while $!nextiter.defined && $count < $n {
            $!nextiter.reify($n - $count);
            pir::setattribute__vPPsP(self, List, '$!nextiter', $!nextiter.nextiter);
            $count = pir::perl6_box_int__PI(pir::elements($!items));
        }
        pir::perl6_box_int__PI(pir::elements($!items))
    }

    method STORE_AT_POS(\$pos, \$v) {
        pir::set__2QiP($!items, pir::repr_unbox_int__IP($pos), $v)
    }
}
