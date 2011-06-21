class List {
    # declared in BOOTSTRAP.pm:
    #   is Iterable;           # parent class
    #   has Mu $!items;        # RPA of our reified elements
    #   has $!flattens;        # true if this list flattens its parcels
    #   has $!nextiter;        # iterator for generating remaining elements

    method Bool()       { self.gimme(1).Bool }
    method Int()        { self.elems }
    method Numeric()    { self.elems }
    method Parcel()     { self.gimme(*); pir__perl6_box_rpa__PP(self.RPA) }
    method Str(List:D:) { self.join(' ') }

    method list() { self }
    method flattens() { $!flattens }

    method at_pos(\$pos) {
        self.exists($pos)
          ?? pir::set__PQi($!items, nqp::unbox_i($pos))
          !! Mu
    }

    method eager() { self.gimme(*); self }

    method elems() {
        # Get as many elements as we can.  If gimme stops before
        # reaching the end of the list, assume the list is infinite.
        my $n = self.gimme(*);
        $!nextiter.defined ?? nqp::p6box_n('Inf') !! $n
    }

    method exists(\$pos) {
        self.gimme($pos + 1);
        nqp::p6bool(pir::exists__IQI($!items, nqp::unbox_i($pos)))
    }

    method flat() {
        pir::perl6_list_from_rpa__PPPP(List, self.RPA, 1.Bool);
    }

    method gimme($n) {
        # create $!items RPA if it doesn't already exist
        pir::defined($!items) or 
            pir::setattribute__3PPsP(self, List, '$!items', pir::new__Ps('ResizablePMCArray'));

        # loop through iterators until we have at least $n elements
        my $count = nqp::p6box_i(pir::elements($!items));
        my $eager = Whatever.ACCEPTS($n);
        while $!nextiter.defined && ($eager 
                                       ?? !$!nextiter.infinite 
                                       !! ($count < $n)) {
            $!nextiter.reify($eager ?? Whatever !! $n - $count);
            pir::setattribute__vPPsP(self, List, '$!nextiter', $!nextiter.nextiter);
            $count = nqp::p6box_i(pir::elements($!items));
        }

        # return the number of elements we have now
        $count
    }

    method infinite() { 
        self.gimme(*);
        $!nextiter.defined 
    }

    method iterator() {
        # Return a reified ListIter containing our currently reified elements
        # and any subsequent iterator.
        pir::setattribute__0PPsP(
            pir::setattribute__0PPsP(
                pir::repr_instance_of__PP(ListIter),
                ListIter, '$!nextiter', $!nextiter),
            ListIter, '$!reified', pir__perl6_box_rpa__PP(self.RPA))
    }

    method munch($n is copy) {
        self.gimme($n);
        my Mu $rpa := pir::new__Ps('ResizablePMCArray');
        pir::push__vPP($rpa, pir::shift__PP($!items))
            while $!items && $n-- > 0;
        pir__perl6_box_rpa__PP($rpa)
    }

    method push(*@values) {
        my $pos = self.elems;
        fail '.push on infinite lists NYI' if $!nextiter.defined;
        self.STORE_AT_POS($pos++, @values.shift) while @values;
    }

    method shift() {
        # make sure we have at least one item, then shift+return it
        self.gimme(1) && pir::shift__PP($!items)
    }

    multi method perl(List:D \$self:) {
        self.gimme(*);
        self.Parcel.perl ~ '.list'  
          ~ (pir::is_container__IP($self) ?? '.item' !! '')
    }

    method STORE_AT_POS(\$pos, Mu \$v) {
        pir::set__1QiP($!items, nqp::unbox_i($pos), $v)
    }

    method RPA() {
        pir::defined($!items) or 
            pir::setattribute__3PPsP(self, List, '$!items', pir::new__Ps('ResizablePMCArray'));
        my Mu $rpa := pir::clone__PP($!items);
        pir::push__vPP($rpa, $!nextiter) if $!nextiter.defined;
        $rpa
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

sub infix:<xx>(Mu \$x, $n is copy) {
    $n = $Inf if Whatever.ACCEPTS($n);
    GatherIter.new({ take $x while $n-- > 0; }, :infinite($n == $Inf))
}


