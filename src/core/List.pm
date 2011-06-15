class Parcel { ... }

class List {
    # Has attributes and parent Iterable declared in BOOTSTRAP

    method Bool() { self.gimme(1).Bool }

    method flat() { self }

    method elems() {
        my $n = self.gimme(*);
        $!rest
          ?? pir::perl6_box_num__PN('Inf')
          !! $n;
    }

    method exists(\$pos) {
        self.gimme($pos+1);
        pir::perl6_booleanize__PI(
            pir::exists__IQI($!items, pir::repr_unbox_int__IP($pos))
        )
    }

    method gimme(\$n) {
        pir::defined($!items) ||
            pir::setattribute__vPPsP(self, List, '$!items',
                                     pir::new__Ps('ResizablePMCArray'));
        my $i = pir::perl6_box_int__PI(pir::elements($!items));
        my $x;
        my $eager = Whatever.ACCEPTS($n);
        while $!rest && ($eager || $i < $n) {
            $x := pir::shift__PP($!rest);
            if $x.defined && Parcel.ACCEPTS($x) {
                pir::splice__vPPii(
                     $!rest, pir::getattribute__PPPs($x, Parcel, '$!storage'),
                     0, 0);
            }
            else {
                self.BIND_POS($i, $x);
                $i = $i + 1;
            }
        }
        pir::perl6_box_int__PI(pir::elements($!items));
    }

    method list() { self }

    method shift() {
        self.gimme(1) && pir::shift__PP($!items);
    }

    method at_pos(\$pos) {
        self.exists($pos)
          ?? pir::set__PQi($!items, pir::repr_unbox_int__IP($pos))
          !! Any
    }

    multi method Str(List:D:) {
        self.join(' ');
    }

    method BIND_POS(\$pos, \$v) {
        pir::set__2QiP($!items, pir::repr_unbox_int__IP($pos), $v)
    }

    method join($separator = '') {
        # TODO: needs to be .gimme(Inf) or something
        # once .gimme knows about infinite lists
        self.gimme(*);

        my $elems = self.elems;

        # unbox all elements ourselves, because
        # the get_string vtable is slow.
        # See http://irclog.perlgeek.de/perl6/2011-06-12#i_3911474
        # for a discussion.
        my Mu $fsa := pir::new__Ps('FixedStringArray');

        # initialize length of the FSA, must be done
        # before entering the first item
        pir::set__vPI($fsa, pir::repr_unbox_int__IP($elems));

        my $i = 0;
        while $i < $elems {
            pir::set__vQis($fsa, pir::repr_unbox_int__IP($i),
                pir::repr_unbox_str__SP(
                    pir::set__PQi($!items, pir::repr_unbox_int__IP($i)).Stringy
                )
            );
            $i = $i + 1;
        }
        pir::perl6_box_str__PS(
            pir::join(pir::repr_unbox_str__SP($separator.Stringy), $fsa)
        );
    }

    method Int()     { self.elems }
    method Numeric() { self.elems }
}

