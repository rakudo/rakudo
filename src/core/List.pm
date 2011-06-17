class Parcel { ... }

class List {
    # Attributes defined in BOOTSTRAP.pm:
    #   has Mu $!items;        # RPA of our reified elements
    #   has $!nextiter;        # iterator for generating remaining elements

    method at_pos(\$pos) {
        self.exists($pos)
          ?? pir::set__PQi($!items, pir::repr_unbox_int__IP($pos))
          !! Mu
    }

    method eager() {
        self.gimme(100) while $!nextiter.defined;
    }

    method exists(\$pos) {
        self.gimme($pos + 1);
        pir::perl6_booleanize__PI(
            pir::exists__IQI($!items, pir::repr_unbox_int__IP($pos)))
    }

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

    method iterator() {
        # Return a Parcel containing our currently reified elements
        # and any subsequent iterator.
        my Mu $rpa := pir::clone__PP($!items);
        pir::push__vPP($rpa, $!nextiter) if $!nextiter.defined;
        pir::setattribute__0PPsP(
            pir::repr_instance_of__PP(Parcel),
            Parcel, '$!storage', $rpa)
    }

    method shift() {
        self.gimme(1) && pir::shift__PP($!items)
    }

    method STORE_AT_POS(\$pos, \$v) {
        pir::set__2QiP($!items, pir::repr_unbox_int__IP($pos), $v)
    }
}
