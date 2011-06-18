class List {
    # declared in BOOTSTRAP.pm:
    #   is Iterable;           # parent class
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
        # Return a reified ListIter containing our currently reified elements
        # and any subsequent iterator.
        self.gimme(0);
        my Mu $rpa := pir::clone__PP($!items);
        pir::push__vPP($rpa, $!nextiter) if $!nextiter.defined;
        pir::setattribute__0PPsP(
            pir::setattribute__0PPsP(
                pir::repr_instance_of__PP(ListIter),
                ListIter, '$!nextiter', $!nextiter),
            ListIter, '$!reified', pir__perl6_box_rpa__PP($rpa))
    }

    method map($block) is rw { 
        MapIter.new(:list(self), :block($block)).list 
    }

    method munch($n is copy) {
        self.gimme($n);
        my Mu $rpa := pir::new__Ps('ResizablePMCArray');
        pir::push__vPP($rpa, pir::shift__PP($!items))
            while $!items && $n-- > 0;
        pir__perl6_box_rpa__PP($rpa)
    }

    method shift() {
        # make sure we have at least one item, then shift+return it
        self.gimme(1) && pir::shift__PP($!items)
    }

    multi method perl(List:D:) {
        '(' ~ self.map({ $^a.perl }).join(', ') ~ ')'
    }

    method STORE_AT_POS(\$pos, Mu \$v) {
        pir::set__1QiP($!items, pir::repr_unbox_int__IP($pos), $v)
    }

    multi method DUMP(List:D:) {
        self.DUMP-ID() ~ '('
          ~ ':items(' ~ DUMP($!items) ~ '), '
          ~ ':nextiter(' ~ DUMP($!nextiter) ~ ')'
          ~ ')'
    }
}

sub eager(|$) {
    pir__perl6_box_rpa__PP(pir::perl6_current_args_rpa__P()).flat.eager
}

sub flat(|$) {
    pir::perl6_list_from_rpa__PPPP(List, pir::perl6_current_args_rpa__P(), 1.Bool)
}

sub list(|$) {
    pir::perl6_list_from_rpa__PPPP(List, pir::perl6_current_args_rpa__P(), Mu)
}
