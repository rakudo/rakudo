class Parcel { ... }

class List is Iterable {
    has $!items;
    has $!rest;
    has $!flat;

    method exists(\$n) {
        self.gimme($n+1);
        pir::perl6_booleanize__PI(
            pir::exists__IQI($!items, pir::repr_unbox_int__IP($n))
        )
    }

    method gimme(\$n) {
        pir::defined($!items) ||
            pir::setattribute__vPPsP(self, List, '$!items',
                                     pir::new__Ps('ResizablePMCArray'));
        my $i = pir::perl6_box_int__PI(pir::elements($!items));
        my $a;
        while $!rest && $i < $n {
            $a := pir::shift__PP($!rest);
            if Parcel.ACCEPTS($a) {
                pir::splice__vPPii($!rest, $a.rpa, 0, 0);
            }
            else {
                self.BIND_POS($i, $a);
                $i = $i + 1;
            }
        }
        pir::perl6_box_int__PI(pir::elements($!items));
    }

    method at_pos(\$n) {
        self.exists($n)
          ?? pir::set__PQi($!items, pir::repr_unbox_int__IP($n))
          !! Any
    }

    method BIND_POS(\$n, \$x) {
        pir::set__2QiP($!items, pir::repr_unbox_int__IP($n), $x)
    }
}

