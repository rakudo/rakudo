class Parcel { ... }

class List {
    # Attributes defined in BOOTSTRAP.pm:
    #   has Mu $!items;        # RPA of our reified elements
    #   has $!nextiter;        # iterator for generating remaining elements

    method Bool()    { self.gimme(1).Bool }
    method Int()     { self.elems }
    method Numeric() { self.elems }

    method flat() { self.iterator.flat }
    method list() { self }

    method at_pos(\$pos) {
        self.exists($pos)
          ?? pir::set__PQi($!items, pir::repr_unbox_int__IP($pos))
          !! Mu
    }

    method eager() { self.gimme(*); self }

    method elems() {
        # Get as many elements as we can.  If gimme stops before
        # reaching the end of the list, assume the list is infinite.
        my $n = self.gimme(*);
        $!nextiter.defined ?? pir::perl6_box_num__PN('Inf') !! $n
    }

    method exists(\$pos) {
        self.gimme($pos + 1);
        pir::perl6_booleanize__PI(
            pir::exists__IQI($!items, pir::repr_unbox_int__IP($pos)))
    }

    method gimme($n) {
        # create $!items RPA if it doesn't already exist
        pir::defined($!items) or 
            pir::setattribute__3PPsP(self, List, '$!items', pir::new__Ps('ResizablePMCArray'));

        # loop through iterators until we have at least $n elements
        my $count = pir::perl6_box_int__PI(pir::elements($!items));
        my $eager = Whatever.ACCEPTS($n);
        while $!nextiter.defined && ($eager || $count < $n) {
            $!nextiter.reify($eager ?? 100 !! $n - $count);
            pir::setattribute__vPPsP(self, List, '$!nextiter', $!nextiter.nextiter);
            $count = pir::perl6_box_int__PI(pir::elements($!items));
        }

        # return the number of elements we have now
        $count
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
        # make sure we have at least one item, then shift+return it
        self.gimme(1) && pir::shift__PP($!items)
    }

    method STORE_AT_POS(\$pos, \$v) {
        pir::set__2QiP($!items, pir::repr_unbox_int__IP($pos), $v)
    }
}
