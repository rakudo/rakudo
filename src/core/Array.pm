my class X::Item { ... };

class Array {
    # Has attributes and parent List declared in BOOTSTRAP.    

    method new(:$shape = *, |) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        fail "Too many elements for this shaped array" unless nqp::istype($shape, Whatever) or nqp::elems($args) < $shape;
        my $array := nqp::p6list($args, self.WHAT, Bool::True);
        nqp::bindattr($array, Array, '$!shape', $shape);
        $array;
    }
    
    multi method at_pos(Array:D: $pos) is rw {
#?if jvm
        if nqp::istype($pos, Num) && nqp::isnanorinf($pos) {
#?endif
#?if !jvm
        if nqp::isnanorinf($pos) {
#?endif
            X::Item.new(aggregate => self, index => $pos).throw;
        }
        my int $p = self.map_index($pos.Int);
        fail "Index $p is too large for this shaped array" unless nqp::istype($!shape, Whatever) or $p < $!shape;
        my Mu $items := nqp::p6listitems(self);
        # hotpath check for element existence (RT #111848)
        nqp::existspos($items, $p)
              || nqp::getattr(self, List, '$!nextiter').defined
                  && self.exists($p)
          ?? nqp::atpos($items, $p)
          !! nqp::p6bindattrinvres(my $v, Scalar, '$!whence',
                 -> { nqp::bindpos($items, $p, $v) } )
    }
    multi method at_pos(Array:D: int $p) is rw {
        my int $pos = self.map_index($p);
        fail "Index $pos is too large for this shaped array" unless nqp::istype($!shape, Whatever) or $pos < $!shape;
        my Mu $items := nqp::p6listitems(self);
        # hotpath check for element existence (RT #111848)
        nqp::existspos($items, $pos)
              || nqp::getattr(self, List, '$!nextiter').defined
                  && self.exists($pos)
          ?? nqp::atpos($items, $pos)
          !! nqp::p6bindattrinvres(my $v, Scalar, '$!whence',
                 -> { nqp::bindpos($items, $pos, $v) } )
    }

    proto method bind_pos(|) { * }
    multi method bind_pos($p is copy, Mu \bindval) is rw {
        my int $pos = self.map_index($p.Int);
        fail "Index $pos is too large for this shaped array" unless nqp::istype($!shape, Whatever) or $pos < $!shape;
        self.gimme($pos + 1);
        nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval);
    }
    multi method bind_pos(int $p, Mu \bindval) is rw {
        my int $pos = self.map_index($p);
        fail "Index $pos is too large for this shaped array" unless nqp::istype($!shape, Whatever) or $pos < $!shape;
        self.gimme($pos + 1);
        nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
    }
    
    method delete(@array is rw: *@indices) {
        my $elems = @array.elems;
        my @result;
        for @indices -> $index {
            my int $i = self.map_index($index ~~ Callable
                                       ?? $index($elems)
                                       !! +$index);
            @result.push(@array[$i]);
            undefine @array[$i];

            # next seems unnecessary but handles an obscure
            # edge case
            #
            # FIXME: does the shape addition work?
            if $i == (@array - 1) and nqp::istype(@array.shape, Whatever) {
                @array.pop;
            }
        }
        if nqp::istype(@array.shape, Whatever) {
            @array.pop while ?@array && !defined @array[@array.elems - 1];
        }
        return @result;
    }

    method flattens() { 1 }

    method shape() { $!shape }

    # FIXME: this should probably be private
    proto method map_index(|) { * }
    multi method map_index(Int $pos --> int) {
        return nqp::unbox_i(nqp::decont($!index_map)($pos).floor.Int)
            if nqp::istype($!index_map, Code);
        nqp::unbox_i($pos)
    }
    multi method map_index(int $pos --> int) {
        return nqp::unbox_i(nqp::decont($!index_map)(nqp::p6box_i($pos)).floor.Int)
            if nqp::istype($!index_map, Code);
        $pos
    }

    method pop() is parcel {
        fail 'Cannot pop from a shaped array' unless nqp::istype($!shape, Whatever);
        nqp::findmethod(List, 'pop')(self)
    }

    multi method push(Array:D: *@values) {
        fail 'Cannot push to a shaped array' unless nqp::istype($!shape, Whatever);
        nqp::findmethod(List, 'push')(self, |@values)
    }

    method shift() is parcel {
        fail 'Cannot shift from a shaped array' unless nqp::istype($!shape, Whatever);
        nqp::findmethod(List, 'shift')(self)
    }

    multi method unshift(Array:D: *@values) {
        fail 'Cannot unshift to a shaped array' unless nqp::istype($!shape, Whatever);
        nqp::findmethod(List, 'unshift')(self, |@values);
    }

    method exists(\pos) {
        nqp::findmethod(List, 'exists')(self, self.map_index(pos))
    }

    multi method perl(Array:D \SELF:) {
        nqp::iscont(SELF)
          ?? '[' ~ self.map({.perl}).join(', ') ~ ']'
          !! self.WHAT.perl ~ '.new(' ~ self.map({.perl}).join(', ') ~ ')'
    }

    method REIFY(Parcel \parcel, Mu \nextiter) {
        my Mu $rpa := nqp::getattr(parcel, Parcel, '$!storage');
        my Mu $iter := nqp::iterator($rpa);
        my int $i = 0;
        while $iter {
            nqp::bindpos($rpa, $i, my $v = nqp::shift($iter));
            $i = $i + 1;
        }
        nqp::findmethod(List, 'REIFY')(self, parcel, nextiter)
    }

    method STORE_AT_POS(Int \p, Mu $v is copy) is rw {
        my int $pos = self.map_index(p.Int);
        fail "Index $pos is too large for this shaped array"
            unless nqp::istype($!shape, Whatever) or $pos < $!shape;
        nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, $v)
    }

    method STORE(|) {
        # get arguments, shift off invocant
        my $args := nqp::p6argvmarray();
        nqp::shift($args);
        # make an array from them (we can't just use ourself for this,
        # or @a = @a will go terribly wrong); make it eager
        my $list := nqp::p6list($args, Array, Mu);
        nqp::bindattr($list, Array, '$!shape', $!shape);
        nqp::bindattr($list, List, '$!flattens', True);
        $list.eager;
        # clear our items and set our next iterator to be one over
        # the array we just created
        nqp::bindattr(self, List, '$!items', Mu);
        nqp::bindattr(self, List, '$!nextiter', nqp::p6listiter(nqp::list($list), self));
        self = self.splice(0, $!shape, self) if self.infinite;
        self
    }

    my role TypedArray[::TValue] does Positional[TValue] {
        multi method at_pos($p is copy, TValue $v? is copy) is rw {
            my int $pos = self.map_index($p.Int);
            fail "Index $pos is too large for this shaped array" unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
            self.exists($pos)
              ?? nqp::atpos(nqp::getattr(self, List, '$!items'), $pos)
              !! nqp::p6bindattrinvres($v, Scalar, '$!whence',
                     -> { nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, $v) } )
        }
        multi method at_pos(int $p, TValue $v? is copy) is rw {
            my int $pos = self.map_index($p);
            fail "Index $pos is too large for this shaped array" unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
            self.exists($pos)
              ?? nqp::atpos(nqp::getattr(self, List, '$!items'), $pos)
              !! nqp::p6bindattrinvres($v, Scalar, '$!whence',
                     -> { nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, $v) } )
        }
        multi method bind_pos($p is copy, TValue \bindval) is rw {
            my int $pos = self.map_index($p.Int);
            fail "Index $pos is too large for this shaped array" unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
            self.gimme($pos + 1);
            nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
        }
        multi method bind_pos(int $p, TValue \bindval) is rw {
            my int $pos = self.map_index($p);
            fail "Index $pos is too large for this shaped array" unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
            self.gimme($pos + 1);
            nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
        }
        multi method perl(::?CLASS:D \SELF:) {
            'Array['
              ~ TValue.perl
              ~ '].new('
              ~ self.map({.perl}).join(', ')
              ~ ')';
        }
        # XXX some methods to come here...
    }
    method PARAMETERIZE_TYPE(Mu $t, |c) {
        if c.elems == 0 {
            self but TypedArray[$t.WHAT]
        }
        else {
            die "Can only type-constraint Array with [ValueType]"
        }
    }
}


sub circumfix:<[ ]>(*@elems) is rw { my $x = @elems.eager }
