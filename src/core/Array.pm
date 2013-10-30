my class X::Item { ... };
my class X::TypeCheck { ... };

class Array { # declared in BOOTSTRAP
    # class Array is List {
    #     has Mu $!shape;
    #     has Mu $!index_map;
    #     has Mu $!descriptor;

    method new(:$shape = *, |) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        fail "Too many elements for this shaped array"
          unless nqp::istype($shape, Whatever) or nqp::elems($args) < $shape;
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
        fail "Index $p is too large for this shaped array"
          unless nqp::istype($!shape, Whatever) or $p < $!shape;
        my Mu $items := nqp::p6listitems(self);
        # hotpath check for element existence (RT #111848)
        if nqp::existspos($items, $p)
          || nqp::getattr(self, List, '$!nextiter').defined
          && self.exists_pos($p) {
            nqp::atpos($items, $p);
        }
        else {
            nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc($!descriptor)),
                Scalar,
                '$!whence',
                -> { nqp::bindpos($items, $p, v) }
            );
        }
    }
    multi method at_pos(Array:D: int $p) is rw {
        my int $pos = self.map_index($p);
        fail "Index $pos is too large for this shaped array"
          unless nqp::istype($!shape, Whatever) or $pos < $!shape;
        my Mu $items := nqp::p6listitems(self);
        # hotpath check for element existence (RT #111848)
        if nqp::existspos($items, $pos)
          || nqp::getattr(self, List, '$!nextiter').defined
          && self.exists_pos($pos) {
            nqp::atpos($items, $pos);
        }
        else {
            nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc($!descriptor)),
                Scalar,
                '$!whence',
                -> { nqp::bindpos($items, $pos, v) }
            );
        }
    }

    proto method bind_pos(|) { * }
    multi method bind_pos($p is copy, Mu \bindval) is rw {
        my int $pos = self.map_index($p.Int);
        fail "Index $pos is too large for this shaped array"
          unless nqp::istype($!shape, Whatever) or $pos < $!shape;
        self.gimme($pos + 1);
        nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval);
    }
    multi method bind_pos(int $p, Mu \bindval) is rw {
        my int $pos = self.map_index($p);
        fail "Index $pos is too large for this shaped array"
          unless nqp::istype($!shape, Whatever) or $pos < $!shape;
        self.gimme($pos + 1);
        nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
    }
    
    method delete (\pos) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :delete adverb");
        self.delete_pos(pos);
    }
    method delete_pos(\pos) {
        fail "Cannot delete from a shaped array" unless nqp::istype($!shape, Whatever);
        fail "Cannot use negative index {pos} on {self.WHAT.perl}" if pos < 0;

        my $value := self.at_pos(pos); # needed for reification
        my $items := nqp::getattr(self,List,'$!items');
        my $end   := self.end;

        if pos == $end {
            my $pos = pos;
            nqp::pop($items);
            nqp::pop($items)
              while --$pos >= 0 && nqp::isnull(nqp::atpos($items, $pos));
        }
        elsif pos < $end {
            nqp::bindpos($items, pos, nqp::null());
        }
        else {
            return self.default;
        }
        $value;
    }

    method flattens() { 1 }

    method shape() { $!shape }

    # FIXME: this should probably be private
    method map_index($pos --> int) {
        return nqp::unbox_i(nqp::decont($!index_map)($pos).floor.Int)
            if nqp::istype($!index_map, Code);
        return nqp::unbox_i($!index_map) if nqp::istype($!index_map, Any);
        nqp::unbox_i($pos)
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

    # introspection
    method name() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Str !! $d.name()
    }
    method of() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! $d.of;
    }
    method default() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! $d.default;
    }
    method dynamic() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Mu !! so $d.dynamic;
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
            nqp::bindpos($rpa, $i, nqp::p6scalarfromdesc($!descriptor) = nqp::shift($iter));
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
        method new(|) {
            my Mu $args := nqp::p6argvmarray();
            nqp::shift($args);
            
            my $list := nqp::p6list($args, self.WHAT, Bool::True);

            my $of = self.of;
            if ( $of !=:= Mu ) {
                for @$list {
                    if $_ !~~ $of {
                        X::TypeCheck.new(
                          operation => '.new',
                          expected  => $of,
                          got       => $_,
                        ).throw;
                    }
                }
            }

            $list;
        }
        multi method at_pos($p is copy) is rw {
            my int $pos = self.map_index($pos.Int);
            fail "Index $pos is too large for this shaped array"
              unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
            if self.exists_pos($pos) {
                nqp::atpos(
                  nqp::getattr(self, List, '$!items'), $pos
                );
            }
            else {
                nqp::p6bindattrinvres(
                    (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
                    Scalar,
                    '$!whence',
                    -> { nqp::bindpos(
                      nqp::getattr(self,List,'$!items'), $pos, v) }
                );
            }
        }
        multi method at_pos(int $p, TValue $v? is copy) is rw {
            my $pos = self.map_index($p);
            fail "Index $pos is too large for this shaped array"
              unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
            if self.exists_pos($pos) {
                nqp::atpos(nqp::getattr(self, List, '$!items'), $pos);
            }
            else {
                nqp::p6bindattrinvres(
                    (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
                    Scalar,
                    '$!whence',
                    -> { nqp::bindpos(nqp::getattr(self, List,'$!items'), $pos, v)}
                );
            }
        }
        multi method bind_pos($p is copy, TValue \bindval) is rw {
            my int $pos = self.map_index($p.Int);
            fail "Index $pos is too large for this shaped array"
              unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
            self.gimme($pos + 1);
            nqp::bindpos(nqp::getattr(self, List, '$!items'), $pos, bindval)
        }
        multi method bind_pos(int $p, TValue \bindval) is rw {
            my int $pos = self.map_index($p);
            fail "Index $pos is too large for this shaped array"
              unless nqp::istype(self.shape, Whatever) or $pos < self.shape;
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
#            my $what := self but TypedArray[$t.WHAT]; # too early in bootstrap
            my $what := self.HOW.mixin(self.WHAT, TypedArray[$t.WHAT]);
            # needs to be done in COMPOSE phaser when that works
            $what.HOW.set_name(self,"{self.HOW.name(self)}[{$t.HOW.name($t)}]");
            $what;
        }
        else {
            die "Can only type-constraint Array with [ValueType]"
        }
    }
}


sub circumfix:<[ ]>(*@elems) is rw { my $x = @elems.eager }
