# all sub postcircumfix [] candidates here please

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
                       $idx ~~ Whatever ?? SELF.cache.elems !! SELF.EXISTS-POS($idx)
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
                if &whatever.count == Inf {
                    nqp::push($!target, whatever(+$!star))
                }
                else {
                    nqp::push($!target, whatever(|(+$!star xx &whatever.count)))
                }
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
    }).cache;
}

proto sub postcircumfix:<[ ]>(|) is nodal { * }

multi sub postcircumfix:<[ ]>( \SELF, Any:U $type, |c ) is raw {
    die "Indexing requires an instance, tried to do: {try SELF.VAR.name}[ {$type.gist} ]";
}

# @a[int 1]
multi sub postcircumfix:<[ ]>( \SELF, int $pos ) is raw {
    SELF.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, Mu \assignee ) is raw {
    SELF.ASSIGN-POS($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, int $pos, Mu :$BIND! is raw) is raw {
    SELF.BIND-POS($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$SINK!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$delete!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$exists!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$kv!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$p!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$k!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$v!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$v, |%other );
}

# @a[Int 1]
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos ) is raw {
    SELF.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Mu \assignee ) is raw {
    SELF.ASSIGN-POS($pos, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Int:D $pos, Mu :$BIND! is raw) is raw {
    SELF.BIND-POS($pos, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$SINK!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$delete!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$exists!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$kv!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$p!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$k!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$v!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, :$v, |%other );
}

# @a[$x]
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos ) is raw {
    SELF.AT-POS(pos.Int);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Mu \assignee ) is raw {
    SELF.ASSIGN-POS(pos.Int, assignee);
}
multi sub postcircumfix:<[ ]>(\SELF, Any:D \pos, Mu :$BIND! is raw) is raw {
    SELF.BIND-POS(pos.Int, $BIND);
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$SINK!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$delete!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$exists!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$kv!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$p!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, :$p, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$k!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, :$k, |%other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$v!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, :$v, |%other );
}

# @a[@i]
multi sub postcircumfix:<[ ]>( \SELF, Iterable:D \pos ) is raw {
    nqp::iscont(pos)
      ?? SELF.AT-POS(pos.Int)
      !! POSITIONS(SELF, pos).map({ SELF[$_] }).eager.list;
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, Mu \val ) is raw {
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
        my $list   := List.new;
        my $target := nqp::getattr($list,List,'$!reified');

        # We try to reify indices eagerly first, in case doing so
        # manipulates SELF.  If pos is lazy or contains Whatevers/closures,
        # the SELF may start to reify as well.
        my \indices := POSITIONS(SELF, pos);
        indices.iterator.sink-all;

        # Extract the values/containers which will be assigned to, in case
        # reifying the rhs does crazy things like splicing SELF.
        my int $p = 0;
        for indices {
            nqp::bindpos($target,$p,SELF[$_]);
            $p = $p + 1;
        }

        rvlist.EXISTS-POS($p);
        my \rviter := rvlist.iterator;
        $p = 0;
        my $elems = nqp::elems($target);
        while $p < $elems {
            nqp::atpos($target,$p) = rviter.pull-one;
            $p = $p + 1;
        }
        $list
    }
    else { # The assumption for now is this must be Iterable
        # Lazy list assignment.  This is somewhat experimental and
        # semantics may change.
        my $target := SELF.iterator;
        my sub eagerize ($idx) {
            once $target := $target.cache.iterator;
            $idx ~~ Whatever ?? $target.elems !! $target.EXISTS-POS($idx);
        }
        my @poslist := POSITIONS(SELF, pos, :eagerize(&eagerize)).eager;
        my %keep;
        # TODO: we could also use a quanthash and count occurences of an
        # index to let things go to GC sooner.
        %keep{@poslist} = ();
        my $max = -1;
        my \rviter := rvlist.iterator;
        @poslist.map: -> $p {
            my $lv;
            for $max ^.. $p -> $i {
                $max = $i;
                my $lv := $target.pull-one;
                %keep{$i} := $lv if %keep{$i}:exists and $lv !=:= IterationEnd;
            }
            $lv := %keep{$p};
            $lv = rviter.pull-one;
        };
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$SINK!, *%other) is raw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,:$delete!,*%other) is raw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,:$exists!,*%other) is raw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$kv!, *%other) is raw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$p!, *%other) is raw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$k!, *%other) is raw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$v!, *%other) is raw {
   SLICE_MORE_LIST( SELF, POSITIONS(SELF, pos), :$v, |%other );
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block ) is raw {
    SELF[$block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)))];
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, Mu \assignee ) is raw {
    SELF[$block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)))] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$SINK!, *%other) is raw {
    SLICE_MORE_LIST( SELF, POSITIONS(SELF,$block), :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$delete!,*%other) is raw {
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$delete, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$exists!,*%other) is raw {
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$exists, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$kv!, *%other) is raw {
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$kv, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$p!, *%other) is raw {
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$p, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$k!, *%other) is raw {
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$k, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$v!, *%other) is raw {
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, :$v, |%other )
      !! SLICE_MORE_LIST( SELF, @$pos, :$v, |%other );
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D ) is raw {
    SELF[^SELF.cache.elems];
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D, Mu \assignee ) is raw {
    SELF[^SELF.cache.elems] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$SINK!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$delete!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$exists!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$kv!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$p!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$k!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$v!, *%other) is raw {
    %other
      ?? SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$v, |%other )
      !! SELF[^SELF.cache.elems];
}

# @a[**]
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, *%adv) is raw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}
multi sub postcircumfix:<[ ]>(\SELF, HyperWhatever:D $, Mu \assignee) is raw {
    X::NYI.new(feature => 'HyperWhatever in array index').throw;
}

# @a[]
multi sub postcircumfix:<[ ]>(\SELF, :$BIND!) is raw {
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, :$SINK!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$SINK, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$delete!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$delete, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$exists!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$exists, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$kv!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$kv, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$p!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$p, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$k!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$k, |%other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$v!, *%other) is raw {
    %other
      ?? SLICE_MORE_LIST( SELF, ^SELF.cache.elems, :$v, |%other )
      !! SELF[^SELF.cache.elems];
}
multi sub postcircumfix:<[ ]>(\SELF, *%other) is raw {
    SELF.ZEN-POS(|%other);
}

proto sub postcircumfix:<[; ]>(|) is nodal { * }

sub MD-ARRAY-SLICE-ONE-POSITION(\SELF, \indices, \idx, int $dim, \target) is raw {
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
            for ^SELF.cache.elems {
                MD-ARRAY-SLICE-ONE-POSITION(SELF.AT-POS($_), indices, indices.AT-POS($next-dim), $next-dim, target)
            }
        }
        elsif nqp::istype(idx, Callable) {
            MD-ARRAY-SLICE-ONE-POSITION(SELF, indices, idx.(|(SELF.cache.elems xx (idx.count == Inf ?? 1 !! idx.count))), $dim, target);
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
            for ^SELF.cache.elems {
                nqp::push(target, SELF.AT-POS($_))
            }
        }
        elsif nqp::istype(idx, Callable) {
            nqp::push(target, SELF.AT-POS(idx.(|(SELF.cache.elems xx (idx.count == Inf ?? 1 !! idx.count)))))
        }
        else {
            nqp::push(target, SELF.AT-POS(idx.Int))
        }
    }
}
sub MD-ARRAY-SLICE(\SELF, @indices) is raw {
    my \target = IterationBuffer.new;
    MD-ARRAY-SLICE-ONE-POSITION(SELF, @indices, @indices.AT-POS(0), 0, target);
    nqp::p6bindattrinvres(List.CREATE, List, '$!reified', target)
}

multi sub postcircumfix:<[; ]>(\SELF, @indices) is raw {
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

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$exists!) is raw {
    if $exists {
        my int $n = @indices.elems;
        my int $i = 0;
        my $all-ints := True;
        while $i < $n {
            $all-ints := False unless nqp::istype(@indices.AT-POS($i), Int);
            $i = $i + 1;
        }
        $all-ints
            ?? SELF.EXISTS-POS(|@indices)
            !! X::NYI.new(feature => ':exists on multi-dimensional slices')
    }
    else {
        SELF[@indices]
    }
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$delete!) is raw {
    if $delete {
        my int $n = @indices.elems;
        my int $i = 0;
        my $all-ints := True;
        while $i < $n {
            $all-ints := False unless nqp::istype(@indices.AT-POS($i), Int);
            $i = $i + 1;
        }
        $all-ints
            ?? SELF.DELETE-POS(|@indices)
            !! X::NYI.new(feature => ':delete on multi-dimensional slices')
    }
    else {
        SELF[@indices]
    }
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$BIND! is rw) is raw {
    my int $n = @indices.elems;
    my int $i = 0;
    my $all-ints := True;
    while $i < $n {
        $all-ints := False unless nqp::istype(@indices.AT-POS($i), Int);
        $i = $i + 1;
    }
    $all-ints
        ?? SELF.BIND-POS(|@indices, $BIND)
        !! X::Bind::Slice.new(type => SELF.WHAT).throw;
}

# vim: ft=perl6 expandtab sw=4
