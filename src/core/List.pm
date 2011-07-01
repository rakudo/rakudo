class List does Positional {
    # declared in BOOTSTRAP.pm:
    #   is Iterable;           # parent class
    #   has Mu $!items;        # RPA of our reified elements
    #   has $!flattens;        # true if this list flattens its parcels
    #   has $!nextiter;        # iterator for generating remaining elements

    method new(|$) {
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        nqp::p6list($args, self.WHAT, Mu);
    }

    method Bool()       { self.gimme(1).Bool }
    method Int()        { self.elems }
    method Numeric()    { self.elems }
    method Str(List:D:) { self.join(' ') }

    method flat() { self.flattens 
                    ?? self 
                    !! nqp::p6list(nqp::list(self), List, 1.Bool)
    }
    method list() { self }
    method flattens() { $!flattens }

    method Parcel() {
        pir::defined($!items) or 
            nqp::bindattr(self, List, '$!items', nqp::list());
        my Mu $rpa := nqp::clone($!items);
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6parcel($rpa, Any);
    }

    method at_pos($pos is copy) {
        $pos = $pos.Int;
        self.exists($pos)
          ?? nqp::atpos($!items, nqp::unbox_i($pos))
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
        nqp::p6bool(nqp::existspos($!items, nqp::unbox_i($pos)))
    }

    method gimme($n) {
        # create $!items RPA if it doesn't already exist
        pir::defined($!items) or 
            nqp::bindattr(self, List, '$!items', nqp::list());

        # loop through iterators until we have at least $n elements
        my $count = nqp::p6box_i(nqp::elems($!items));
        my $eager = Whatever.ACCEPTS($n);
        while $!nextiter.defined && ($eager 
                                       ?? !$!nextiter.infinite 
                                       !! ($count < $n)) {
            $!nextiter.reify($eager ?? Whatever !! $n - $count);
            nqp::bindattr(self, List, '$!nextiter', $!nextiter.nextiter);
            $count = nqp::p6box_i(nqp::elems($!items));
        }

        # return the number of elements we have now
        $count
    }

    method infinite() { 
        $!nextiter.defined && $!nextiter.infinite;
    }

    method iterator() {
        # Return a reified ListIter containing our currently reified elements
        # and any subsequent iterator.
        my $iter := nqp::create(ListIter);
        nqp::bindattr($iter, ListIter, '$!nextiter', $!nextiter);
        nqp::bindattr($iter, ListIter, '$!reified', self.Parcel());
        $iter;
    }

    method munch(\$n) {
        self.gimme($n) if nqp::not_i(nqp::istype($n, Int))
                          || nqp::isnull($!items)
                          || nqp::islt_i(nqp::elems($!items), nqp::unbox_i($n));
        nqp::p6parcel(
            pir::perl6_shiftpush__0PPi(nqp::list(), $!items, nqp::unbox_i($n)),
            Any
        )
    }

    method push(*@values) {
        my $pos = self.elems;
        fail '.push on infinite lists NYI' if $!nextiter.defined;
        self.STORE_AT_POS($pos++, @values.shift) while @values;
    }

    method reverse() {
        self.gimme(*);
        fail 'Cannot reverse and infinite list' if self.infinite;
        my Mu $rev  := nqp::list();
        my Mu $orig := nqp::clone($!items);
        nqp::push($rev, nqp::pop($orig)) while $orig;
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $rev);
        $rlist;
    }

    method shift() {
        # make sure we have at least one item, then shift+return it
        self.gimme(1) 
          ?? nqp::shift($!items) 
          !! fail 'Element shifted from empty array';
    }

    method pop() {
        # .elems also reifies
        my $e := self.elems;
        $e == $Inf
            ?? fail('Pop from infinite array')
            !! (
                $e == 0
                    ?? fail('Pop from empty array')
                    !!  nqp::pop($!items)
                );
    }

    multi method perl(List:D \$self:) {
        self.gimme(*);
        self.Parcel.perl ~ '.list'  
          ~ (nqp::iscont($self) ?? '.item' !! '')
    }

    method REIFY(Parcel \$parcel) {
        nqp::splice($!items, nqp::getattr($parcel, Parcel, '$!storage'),
                    nqp::elems($!items), 0);
        $parcel
    }

    method STORE_AT_POS(\$pos, Mu \$v) {
        nqp::bindpos($!items, nqp::unbox_i($pos), $v)
    }

    method ARGLIST_FLATTENABLE() { self.gimme(*); $!items }

    multi method DUMP(List:D:) {
        self.DUMP-ID() ~ '('
          ~ ("\x221e " if self.infinite) ~
          ~ ':items(' ~ DUMP($!items) ~ '), '
          ~ ':nextiter(' ~ DUMP($!nextiter) ~ ')'
          ~ ')'
    }
}

sub eager(|$) {
    nqp::p6parcel(pir::perl6_current_args_rpa__P(), Any).eager
}

sub flat(|$) {
    nqp::p6list(pir::perl6_current_args_rpa__P(), List, 1.Bool)
}

sub list(|$) {
    nqp::p6list(pir::perl6_current_args_rpa__P(), List, Mu)
}

sub infix:<xx>(Mu \$x, $n is copy) {
    $n = $Inf if Whatever.ACCEPTS($n);
    GatherIter.new({ take $x while $n-- > 0; }, :infinite($n == $Inf)).list
}


