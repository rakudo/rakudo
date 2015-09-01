# all sub postcircumfix [] candidates here please

# Generate up to N positions
sub NPOSITIONS(\pos, \elems) {
    my \indexes = IterationBuffer.CREATE;
    pos.flat.iterator.push-exactly(indexes, elems);
    nqp::p6bindattrinvres(List.CREATE, List, '$!reified', indexes)
}

# Generates list of positions to index into the array at. Takes all those
# before something lazy is encountered and eagerly reifies them. If there
# are any lazy things in the slice, then we lazily consider those, but will
# truncate at the first one that is out of range. We have a special case for
# Range, which will auto-truncate even though not lazy.  The optional
# :$eagerize will be called if Whatever/WhateverCode is encountered or if
# clipping of lazy indices is enacted.  It should return the number of
# elements of the array if called with Whatever, or do something EXISTS-POSish
# if called with an Int.  Before it does so, it may cause the calling code
# to switch to a memoized version of an iterator by modifying variables in
# the caller's scope.
proto sub POSITIONS(|) { * }
multi sub POSITIONS(\SELF, \pos, Callable :$eagerize = -> $idx {
                       $idx ~~ Whatever ?? SELF.elems !! SELF.EXISTS-POS($idx)
                    }) {
    my class IndicesReificationTarget {
        has $!target;
        has $!star;

        method new(\target, \star) {
            my \rt = self.CREATE;
            nqp::bindattr(rt, self, '$!target', target);
            nqp::bindattr(rt, self, '$!star', star);
            rt
        }

        method push(Mu \value) {
            if nqp::istype(value,Callable) {
                if nqp::istype($!star, Callable) {
                    nqp::bindattr(self, IndicesReificationTarget, '$!star', $!star(*))
                }
                # just using value(...) causes stage optimize to die
                my &whatever := value;
                nqp::push($!target, whatever(|(+$!star xx &whatever.count)))
            }
            else {
                nqp::push($!target, value)
            }
        }
    }


    my \pos-iter = pos.iterator;
    my \pos-list = List.CREATE;
    my \eager-indices = IterationBuffer.CREATE;
    my \target = IndicesReificationTarget.new(eager-indices, $eagerize);
    nqp::bindattr(pos-list, List, '$!reified', eager-indices);
    unless pos-iter.push-until-lazy(target) =:= IterationEnd {
        # There are lazy positions to care about too. We truncate at the first
        # one that fails to exists.
        my \rest-seq = Seq.new(pos-iter).flatmap: -> Int() $i {
            last unless $eagerize($i);
            $i
        };
        my \todo := List::Reifier.CREATE;
        nqp::bindattr(todo, List::Reifier, '$!reified', eager-indices);
        nqp::bindattr(todo, List::Reifier, '$!current-iter', rest-seq.iterator);
        nqp::bindattr(todo, List::Reifier, '$!reification-target', eager-indices);
        nqp::bindattr(pos-list, List, '$!todo', todo);
    }
    pos-list
}
multi sub POSITIONS(\SELF, Range \pos) {
    # Can be more clever here and look at range endpoints, as an optimization.
    pos.map(-> Int() $i {
        last unless SELF.EXISTS-POS($i);
        $i
    }).list;
}

proto sub postcircumfix:<[ ]>(|) is nodal { * }

multi sub postcircumfix:<[ ]>( \SELF, Any:U $type, |c ) is rw {
    die "Indexing requires an instance, tried to do: {try SELF.VAR.name}[ {$type.gist} ]";
}

# @a[int 1]
multi sub postcircumfix:<[ ]>( \SELF, int $pos ) is rw {
    SELF.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, Mu \assignee ) is rw {
    SELF.ASSIGN-POS($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND! is parcel) is rw {
    SELF.BIND-POS($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$SINK!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$delete!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$exists!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$kv!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$p!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$k!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$v!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$v, |%other );
}

# @a[Int 1]
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos ) is rw {
    SELF.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Mu \assignee ) is rw {
    SELF.ASSIGN-POS($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D $pos, Mu :$BIND! is parcel) is rw {
    SELF.BIND-POS($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$SINK!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$delete!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$exists!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$kv!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$p!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$k!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$v!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, $pos, :$v, |%other );
}

# @a[$x]
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos ) is rw {
    SELF.AT-POS(pos.Int);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Mu \assignee ) is rw {
    SELF.ASSIGN-POS(pos.Int, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, Mu :$BIND! is parcel) is rw {
    SELF.BIND-POS(pos.Int, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$SINK!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$delete!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$exists!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$kv!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$p!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$k!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$v!, *%other ) is rw {
    SLICE_ONE_LIST( SELF, pos.Int, :$v, |%other );
}

# @a[@i]
multi sub postcircumfix:<[ ]>( \SELF, Iterable:D \pos ) is rw {
    nqp::iscont(pos)
      ?? SELF.AT-POS(pos.Int)
      !! POSITIONS(SELF, pos).map({ SELF[$_] }).eager.List;
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, Mu \val ) is rw {
    # MMD is not behaving itself so we do this by hand.
    if nqp::iscont(pos) {
        return SELF[pos.Int] = val;
    }

    # Prep an iterator that will assign Nils past end of rval
    my \rvlist :=
        do if  nqp::iscont(val)
            or not nqp::istype(val, Iterator)
               and not nqp::istype(val, Iterable) {
            (nqp::decont(val),).Slip
        }
        elsif nqp::istype(val, Iterator) {
            Slip.from-loop({ nqp::decont(val.pull-one) })
        }
        elsif nqp::istype(val, Iterable) {
            val.map({ nqp::decont($_) }).Slip
        }, (Nil xx Inf).Slip;

    if nqp::istype(SELF, Positional) {
        # For Positionals, preserve established/expected evaluation order.
        my @target;
        # We try to reify indices eagerly first, in case doing so
        # manipulates SELF.  If pos is lazy or contains Whatevers/closures,
        # the SELF may start to reify as well.
        my \indices := POSITIONS(SELF, pos);
        indices.iterator.sink-all;
        # Extract the values/containers which will be assigned to, in case
        # reifying the rhs does crazy things like splicing SELF.
        my $p = 0;
        for indices { @target[$p++] := SELF[$_] }

        rvlist.EXISTS-POS($p);
        my \rviter := rvlist.iterator;
        $p = 0;
        for 0..^+@target { @target[$p++] = rviter.pull-one }
        @target[0..^*];
    }
    else {
        die(X::NYI.new(:feature("Slice assignment to {SELF.WHAT.^name}")));
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$SINK!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,:$delete!,*%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,:$exists!,*%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$kv!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$p!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$k!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$v!, *%other) is rw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$v, |%other );
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block ) is rw {
    SELF[$block(|(SELF.elems xx $block.count))];
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, Mu \assignee ) is rw {
    SELF[$block(|(SELF.elems xx $block.count))] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$SINK!, *%other) is rw {
    SLICE_MORE_LIST( SELF, POSITIONS(SELF,$block), :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$delete!,*%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$delete, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$exists!,*%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$exists, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$kv!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$kv, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$p!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$p, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$k!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$k, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$v!, *%other) is rw {
    my $pos := $block(|(SELF.elems xx $block.count));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$v, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$v, |%other );
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D ) is rw {
    SELF[^SELF.elems];
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D, Mu \assignee ) is rw {
    SELF[^SELF.elems] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$BIND!) is rw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$SINK!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$delete!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$exists!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$kv!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$p!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$k!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$v!, *%other) is rw {
    %other
      ?? SLICE_MORE_LIST( SELF, ^SELF.elems, :$v, |%other )
      !! SELF[^SELF.elems];
}

# @a[**]
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, *%adv) is rw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, Mu \assignee) is rw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}

# @a[]
multi sub postcircumfix:<[ ]>( \SELF ) is rw {
    nqp::decont(SELF);
}
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is rw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, :$SINK!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$delete!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$exists!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$kv!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$p!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$k!, *%other) is rw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$v!, *%other) is rw {
    %other
      ?? SLICE_MORE_LIST( SELF, ^SELF.elems, :$v, |%other )
      !! SELF[^SELF.elems];
}


proto sub postcircumfix:<[; ]>(|) is nodal { * }

sub MD-ARRAY-SLICE-ONE-POSITION(\SELF, \indices, \idx, int $dim, \target) is rw {
    my int $next-dim = $dim + 1;
    if $next-dim < indices.elems {
        if nqp::istype(idx, Iterable) && !nqp::iscont(idx) {
            for idx {
                MD-ARRAY-SLICE-ONE-POSITION(SELF, indices, $_, $dim, target)
            }
        }
        elsif nqp::istype(idx, Int) {
            MD-ARRAY-SLICE-ONE-POSITION(SELF.AT-POS(idx), indices, indices.AT-POS($next-dim), $next-dim, target)
        }
        elsif nqp::istype(idx, Whatever) {
            for ^SELF.elems {
                MD-ARRAY-SLICE-ONE-POSITION(SELF.AT-POS($_), indices, indices.AT-POS($next-dim), $next-dim, target)
            }
        }
        elsif nqp::istype(idx, Callable) {
            MD-ARRAY-SLICE-ONE-POSITION(SELF, indices, idx.(|(SELF.elems xx idx.count)), $dim, target);
        }
        else  {
            MD-ARRAY-SLICE-ONE-POSITION(SELF.AT-POS(idx.Int), indices, indices.AT-POS($next-dim), $next-dim, target)
        }
    }
    else {
        if nqp::istype(idx, Iterable) && !nqp::iscont(idx) {
            for idx {
                MD-ARRAY-SLICE-ONE-POSITION(SELF, indices, $_, $dim, target)
            }
        }
        elsif nqp::istype(idx, Int) {
            nqp::push(target, SELF.AT-POS(idx))
        }
        elsif nqp::istype(idx, Whatever) {
            for ^SELF.elems {
                nqp::push(target, SELF.AT-POS($_))
            }
        }
        elsif nqp::istype(idx, Callable) {
            nqp::push(target, SELF.AT-POS(idx.(|(SELF.elems xx idx.count))))
        }
        else {
            nqp::push(target, SELF.AT-POS(idx.Int))
        }
    }
}
sub MD-ARRAY-SLICE(\SELF, @indices) is rw {
    my \target = IterationBuffer.new;
    MD-ARRAY-SLICE-ONE-POSITION(SELF, @indices, @indices.AT-POS(0), 0, target);
    nqp::p6bindattrinvres(List.CREATE, List, '$!reified', target)
}

multi sub postcircumfix:<[; ]>(\SELF, @indices) is rw {
    my int $n = @indices.elems;
    my int $i = 0;
    my $all-ints := True;
    while $i < $n {
        $all-ints := False unless nqp::istype(@indices.AT-POS($i), Int);
        $i = $i + 1;
    }
    $all-ints
        ?? SELF.AT-POS(|@indices)
        !! MD-ARRAY-SLICE(SELF, @indices)
}

# vim: ft=perl6 expandtab sw=4
