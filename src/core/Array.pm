class Array {
    # Has attributes and parent List declared in BOOTSTRAP.    

    method new(|$) { 
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        nqp::p6list($args, self.WHAT, Bool::True);
    }
    
    multi method at_pos($pos is copy) is rw {
        $pos = $pos.Int;
        self.exists($pos)
          ?? nqp::atpos(nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos))
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { nqp::bindpos(nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos), $v) } )
    }
    multi method at_pos(int $pos ) is rw {
        self.exists($pos)
          ?? nqp::atpos(nqp::getattr(self, List, '$!items'), $pos)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, $v) } )
    }

    method flattens() { 1 }

    multi method perl(Array:D \$self:) {
        nqp::iscont($self)
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

    method STORE_AT_POS(\$pos, Mu $v is copy) is rw {
        pir::find_method__PPs(List, 'STORE_AT_POS')(self, $pos, $v);
    }

    method STORE(|$) {
        # get arguments, shift off invocant
        my $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        # make an array from them (we can't just use ourself for this,
        # or @a = @a will go terribly wrong); make it eager
        my $list := nqp::p6list($args, Array, Mu);
        nqp::bindattr($list, List, '$!flattens', True);
        $list.eager;
        # clear our items and set our next iterator to be one over
        # the array we just created
        nqp::bindattr(self, List, '$!items', Mu);
        nqp::bindattr(self, List, '$!nextiter', nqp::p6listiter(nqp::list($list), self));
        self
    }

    my role TypedArray[::TValue] does Positional[TValue] {
        multi method at_pos($pos is copy, TValue $v? is copy) is rw {
            $pos = $pos.Int;
            self.exists($pos)
              ?? nqp::atpos(nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos))
              !! pir::setattribute__0PPsP($v, Scalar, '$!whence',
                     -> { nqp::bindpos(nqp::getattr(self, List, '$!items'), nqp::unbox_i($pos), $v) } )
        }
        multi method at_pos(int $pos, TValue $v? is copy) is rw {
            self.exists($pos)
              ?? nqp::atpos(nqp::getattr(self, List, '$!items'), $pos)
              !! pir::setattribute__0PPsP($v, Scalar, '$!whence',
                     -> { nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, $v) } )
        }
        # XXX some methods to come here...
    }
    method PARAMETERIZE_TYPE(Mu $t) {
        self but TypedArray[$t.WHAT]
    }
}


sub circumfix:<[ ]>(*@elems) is rw { my $x = @elems }
