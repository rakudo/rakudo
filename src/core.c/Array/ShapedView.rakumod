# A view of the elements of a shaped array that indices for its first
# dimensions take, which takes the indices of the dimensions left. Each of
# its elements is accessed through the array with all of its indices.
my class Array::ShapedView does Positional does Iterable is implementation-detail {
    has $!array;    # the shaped array viewed
    has $!indices;  # the indices given for its first dimensions
    has $!dims;     # the lengths of the dimensions left

    # The elements of a view, or their indices within it, in the order of
    # those indices
    my class Region does Iterator {
        has $!array;
        has $!indices;  # the indices given, then those of the dimensions left
        has $!dims;
        has int $!given;
        has int $!keys;
        has int $!done;

        method !SET-SELF(\array, \indices, \dims, int $keys) {
            $!array   := array;
            $!indices := nqp::clone(nqp::getattr(indices,List,'$!reified'));
            $!dims    := nqp::getattr(dims,List,'$!reified');
            $!given    = nqp::elems($!indices);
            $!keys     = $keys;
            nqp::push($!indices,0) for ^nqp::elems($!dims);
            self
        }
        method new(\array, \indices, \dims, int $keys) {
            nqp::create(self)!SET-SELF(array, indices, dims, $keys)
        }

        method pull-one() is raw {
            if $!done {
                IterationEnd
            }
            else {
                my \result := $!keys
                  ?? nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
                       nqp::slice($!indices,$!given,nqp::sub_i(nqp::elems($!indices),1)))
                  !! $!array.AT-POS(|nqp::p6bindattrinvres(
                       nqp::create(List),List,'$!reified',nqp::clone($!indices)));

                # the next indices, the last dimension changing first
                my int $i = nqp::elems($!dims);
                nqp::while(
                  nqp::isge_i(--$i,0)
                    && nqp::isge_i(
                         nqp::add_i(nqp::atpos($!indices,nqp::add_i($!given,$i)),1),
                         nqp::atpos($!dims,$i)
                       ),
                  nqp::bindpos($!indices,nqp::add_i($!given,$i),0)
                );
                nqp::islt_i($i,0)
                  ?? ($!done = 1)
                  !! nqp::bindpos($!indices,nqp::add_i($!given,$i),
                       nqp::add_i(nqp::atpos($!indices,nqp::add_i($!given,$i)),1));
                result
            }
        }
        method is-deterministic(--> True) { }
    }

    method !SET-SELF(\array, \indices, \dims) {
        $!array   := array;
        $!indices := indices;
        $!dims    := dims;
        self
    }

    # A view of the elements a shaped array has for the indices given for
    # fewer than its dimensions, each checked against its dimension
    method new(\array, @indices) {
        my \shape   := array.shape;
        my \indices := nqp::create(IterationBuffer);
        my int $i    = -1;
        for @indices -> \index {
            my int $length = shape.AT-POS(++$i);
            my $pos := index.Int;
            $pos.throw if nqp::istype($pos,Failure);
            die "Index $pos for dimension {$i + 1} out of range (must be 0..{$length - 1})"
              unless 0 <= $pos < $length;
            nqp::push(indices,$pos);
        }
        my \dims := nqp::create(IterationBuffer);
        nqp::push(dims,shape.AT-POS($_)) for nqp::elems(indices) ..^ shape.elems;
        nqp::create(self)!SET-SELF(array, indices.List, dims.List)
    }

    method !all(@indices) is raw {
        my \all := nqp::clone(nqp::getattr($!indices,List,'$!reified'));
        nqp::push(all,$_) for @indices;
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',all)
    }

    proto method AT-POS(|) is raw {*}
    multi method AT-POS(::?CLASS:D: **@indices) is raw {
        $!array.AT-POS(|self!all(@indices))
    }
    proto method ASSIGN-POS(|) is raw {*}
    # The value is taken from the capture, as a slurpy would make Nil Any
    # and take the value out of a container to bind
    multi method ASSIGN-POS(::?CLASS:D: |c) is raw {
        my \indices := nqp::clone(nqp::getattr(c,Capture,'@!list'));
        my \value   := nqp::pop(indices);
        $!array.ASSIGN-POS(
          |self!all(nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',indices)),
          value
        )
    }
    proto method BIND-POS(|) is raw {*}
    multi method BIND-POS(::?CLASS:D: |c) is raw {
        my \indices := nqp::clone(nqp::getattr(c,Capture,'@!list'));
        my \value   := nqp::pop(indices);
        $!array.BIND-POS(
          |self!all(nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',indices)),
          value
        )
    }
    proto method EXISTS-POS(|) {*}
    multi method EXISTS-POS(::?CLASS:D: **@indices) {
        $!array.EXISTS-POS(|self!all(@indices))
    }
    proto method DELETE-POS(|) is raw {*}
    multi method DELETE-POS(::?CLASS:D: **@indices) is raw {
        $!array.DELETE-POS(|self!all(@indices))
    }

    method shape(::?CLASS:D:) { $!dims }
    method of(::?CLASS:D:)    { $!array.of }
    method elems(::?CLASS:D:) { $!dims.AT-POS(0) }
    method end(::?CLASS:D:)   { $!dims.AT-POS(0) - 1 }

    method iterator(::?CLASS:D:) { Region.new($!array, $!indices, $!dims, 0) }
    multi method list(::?CLASS:D:) { List.from-iterator(self.iterator) }
    multi method List(::?CLASS:D:) { List.from-iterator(self.iterator) }
    multi method Seq(::?CLASS:D:)  { Seq.new(self.iterator) }
    multi method values(::?CLASS:D:) { Seq.new(self.iterator) }
    # A view of one dimension left is keyed by Int, as an array is
    multi method keys(::?CLASS:D:) {
        nqp::iseq_i($!dims.elems,1)
          ?? Seq.new(Rakudo::Iterator.IntRange(0,nqp::sub_i(self.elems,1)))
          !! Seq.new(Region.new($!array, $!indices, $!dims, 1))
    }
    # The values are the containers of the elements, as a slip would give
    # what they contain
    multi method kv(::?CLASS:D:) {
        my \kv := nqp::create(IterationBuffer);
        for self.keys -> \key {
            nqp::push(kv,key);
            nqp::push(kv,self.AT-POS(|key));
        }
        kv.List.Seq
    }
    multi method pairs(::?CLASS:D:) {
        self.keys.map({ Pair.new($_, self.AT-POS(|$_)) })
    }

    # A shaped array of the shape of the view holding its values
    method Array(::?CLASS:D:) {
        my \copy := nqp::istype($!array,array)
          ?? array[$!array.of].new(:shape($!dims))
          !! nqp::istype($!array,Array::Typed)
            ?? Array[$!array.of].new(:shape($!dims))
            !! Array.new(:shape($!dims));
        my \keys := Region.new($!array, $!indices, $!dims, 1);
        nqp::until(
          nqp::eqaddr((my \key := keys.pull-one),IterationEnd),
          nqp::if(
            self.EXISTS-POS(|key),
            copy.ASSIGN-POS(|key, self.AT-POS(|key))
          )
        );
        copy
    }

    # A view keeps its dimensions, as the array it views does
    method !illegal(str $operation) {
        X::IllegalOnFixedDimensionArray.new(:$operation).throw
    }
    method push(|)    { self!illegal('push')    }
    method append(|)  { self!illegal('append')  }
    method unshift(|) { self!illegal('unshift') }
    method prepend(|) { self!illegal('prepend') }
    method pop(|)     { self!illegal('pop')     }
    method shift(|)   { self!illegal('shift')   }
    method splice(|)  { self!illegal('splice')  }
    # A view of one dimension reverses and rotates as an array of one does
    method reverse(|c) {
        nqp::iseq_i($!dims.elems,1)
          ?? Seq.new(self.iterator).reverse(|c)
          !! self!illegal('reverse')
    }
    method rotate(|c) {
        nqp::iseq_i($!dims.elems,1)
          ?? Seq.new(self.iterator).rotate(|c)
          !! self!illegal('rotate')
    }

    # A view of several dimensions matches as the list of its rows
    multi method ACCEPTS(::?CLASS:D: Mu \topic) {
        nqp::eqaddr(self,nqp::decont(topic))
          || (nqp::iseq_i($!dims.elems,1)
               ?? self.List
               !! (^self.elems).map({ self.AT-POS($_) }).List
             ).ACCEPTS(topic)
    }
    method fmt(|c) { self.List.fmt(|c) }

    multi method gist(::?CLASS:D:) { self.Array.gist }
    multi method raku(::?CLASS:D:) { self.Array.raku }
    multi method Str(::?CLASS:D:)  { self.Array.Str }
    multi method Bool(::?CLASS:D:) { True }
    multi method Numeric(::?CLASS:D:) { self.elems }
    multi method Int(::?CLASS:D:)  { self.elems }
    multi method Real(::?CLASS:D:) { self.elems }
    method Capture(::?CLASS:D:) { self.List.Capture }
}

# vim: expandtab shiftwidth=4
