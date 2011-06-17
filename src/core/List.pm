class List {
    # has Mu $!items   # BOOTSTRAP.pm
    # has Mu $!iter    # BOOTSTRAP.pm

    method gimme($n) {
        pir::defined($!items) or 
            pir::setattribute__3PPsP(self, List, '$!items', pir::new__Ps('ResizablePMCArray'));
        my $count = pir::perl6_box_int__PI(pir::elements($!items));
        while $!iter.defined && $count < $n {
            $!iter.reify($n - $count);
            pir::setattribute__vPPsP(self, List, '$!iter', $!iter.nextiter);
            $count = pir::perl6_box_int__PI(pir::elements($!items));
        }
        pir::perl6_box_int__PI(pir::elements($!items))
    }

#    method iterator() {
#        pir::defined($!items) or $!items := pir::new__Ps('ResizablePMCArray');
#        my Mu $rpa := pir::clone__PP($!items);
#        pir::push__vPP($rpa, $!iter) if $!iter.defined;
#        pir__perl6_iter_from_rpa__PPPP($rpa, self, Mu);
#    }

    method STORE_AT_POS(\$pos, \$v) {
        pir::set__2QiP($!items, pir::repr_unbox_int__IP($pos), $v)
    }
}
