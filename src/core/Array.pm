class Array {
    # Has attributes and parent List declared in BOOTSTRAP.    
    
    method at_pos($pos) {
        self.exists($pos)
          ?? pir::find_method__PPs(List, 'at_pos')(self, $pos)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(List, 'STORE_AT_POS')(self, $pos, $v) } )
    }

    method flattens() { 1 }

    multi method perl(Array:D \$self:) {
        pir::is_container__IP($self)
          ?? '[' ~ self.map({.perl}).join(', ') ~ ']'
          !! self.WHAT.perl ~ '.new(' ~ self.map({.perl}).join(', ') ~ ')'
    }

    method REIFY(Parcel \$parcel) {
        my Mu $rpa := nqp::getattr($parcel, Parcel, '$!storage');
        my Mu $iter := nqp::iterator($rpa);
        my $i = 0;
        while $iter {
            nqp::bindpos($rpa, nqp::unbox_i($i++), my $v = nqp::shift($iter));
        }
        pir::find_method__PPs(List, 'REIFY')(self, $parcel)
    }

    method STORE_AT_POS(\$pos, Mu $v is copy) {
        pir::find_method__PPs(List, 'STORE_AT_POS')(self, $pos, $v);
    }

    method STORE(|$) {
        # get arguments, shift off invocant
        my $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        # clear our current items, and create a flattening iterator
        # that will bring in values from $args
        pir::setattribute__vPPsP(self, List, '$!items', Mu);
        pir::setattribute__0PPsP(self, List, '$!nextiter',
            pir::perl6_iter_from_rpa__PPP($args, self));
        self.eager
    }

}


sub circumfix:<[ ]>(*@elems) is rw { my $x = @elems }
