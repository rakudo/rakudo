# all sub postcircumfix [] candidates here please

# Generates list of positions to index into the array at. Takes all those
# before something lazy is encountered and eagerly reifies them. If there
# are any lazy things in the slice, then we lazily consider those, but will
# truncate at the first one that is out of range. The optional
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
            my \rt = nqp::create(self);
            nqp::bindattr(rt, self, '$!target', target);
            nqp::bindattr(rt, self, '$!star', star);
            rt
        }

        method push(Mu \value) {
            nqp::if(
              nqp::istype(value,Callable),
              nqp::stmts(
                nqp::if(
                  nqp::istype($!star,Callable),
                  nqp::bindattr(self,IndicesReificationTarget,'$!star',$!star(*))
                ),
                # just using value(...) causes stage optimize to die
                (my &whatever := value),
                nqp::if(
                  &whatever.count == Inf,
                  nqp::push($!target, whatever(+$!star)),
                  nqp::push($!target, whatever(|(+$!star xx &whatever.count)))
                )
              ),
              nqp::push($!target,value)
            )
        }
    }


    my \pos-iter = pos.iterator;
    my \pos-list = nqp::create(List);
    my \eager-indices = nqp::create(IterationBuffer);
    my \target = IndicesReificationTarget.new(eager-indices, $eagerize);
    nqp::bindattr(pos-list, List, '$!reified', eager-indices);
    unless pos-iter.push-until-lazy(target) =:= IterationEnd {
        # There are lazy positions to care about too. We truncate at the first
        # one that fails to exists.
        my \rest-seq = Seq.new(pos-iter).flatmap: -> Int() $i {
            nqp::unless(
              $eagerize($i),
              last,
              $i
            )
        };
        my \todo := nqp::create(List::Reifier);
        nqp::bindattr(todo, List::Reifier, '$!reified', eager-indices);
        nqp::bindattr(todo, List::Reifier, '$!current-iter', rest-seq.iterator);
        nqp::bindattr(todo, List::Reifier, '$!reification-target', eager-indices);
        nqp::bindattr(pos-list, List, '$!todo', todo);
    }
    pos-list
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
    SLICE_ONE_LIST( SELF, $pos, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$delete!, *%other ) is raw {
    $delete && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? SELF.DELETE-POS($pos)
      !! SLICE_ONE_LIST( SELF, $pos, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$exists!, *%other ) is raw {
    $exists && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? SELF.EXISTS-POS($pos)
      !! SLICE_ONE_LIST( SELF, $pos, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$kv!, *%other ) is raw {
    $kv && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? ($pos, SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$p!, *%other ) is raw {
    $p && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? Pair.new($pos,SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$k!, *%other ) is raw {
    $k && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? $pos !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, int $pos, :$v!, *%other ) is raw {
    $v && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? nqp::decont(SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'v', $v, %other );
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
    SLICE_ONE_LIST( SELF, $pos, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$delete!, *%other ) is raw {
    $delete && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? SELF.DELETE-POS($pos)
      !! SLICE_ONE_LIST( SELF, $pos, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$exists!, *%other ) is raw {
    $exists && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? SELF.EXISTS-POS($pos)
      !! SLICE_ONE_LIST( SELF, $pos, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$kv!, *%other ) is raw {
    $kv && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? ($pos, SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$p!, *%other ) is raw {
    $p && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? Pair.new($pos,SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$k!, *%other ) is raw {
    $k && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? $pos !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, :$v!, *%other ) is raw {
    $v && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? nqp::decont(SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'v', $v, %other );
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
    SLICE_ONE_LIST( SELF, pos.Int, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$delete!, *%other ) is raw {
    $delete && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? SELF.DELETE-POS(pos.Int)
      !! SLICE_ONE_LIST( SELF, pos.Int, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$exists!, *%other ) is raw {
    $exists && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? SELF.EXISTS-POS(pos.Int)
      !! SLICE_ONE_LIST( SELF, pos.Int, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$kv!, *%other ) is raw {
    $kv && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS(pos.Int) ?? (pos, SELF.AT-POS(pos.Int)) !! ())
      !! SLICE_ONE_LIST( SELF, pos.Int, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$p!, *%other ) is raw {
    $p && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos, SELF.AT-POS(pos.Int)) !! ())
      !! SLICE_ONE_LIST( SELF, pos.Int, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$k!, *%other ) is raw {
    $k && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS(pos.Int) ?? pos !! ())
      !! SLICE_ONE_LIST( SELF, pos.Int, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, :$v!, *%other ) is raw {
    $v && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS(pos.Int) ?? nqp::decont(SELF.AT-POS(pos.Int)) !! ())
      !! SLICE_ONE_LIST( SELF, pos.Int, 'v', $v, %other );
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
        my int $p = -1;
        nqp::bindpos($target,++$p,SELF[$_]) for indices;

        rvlist.EXISTS-POS($p);
        my \rviter := rvlist.iterator;
        $p = -1;
        my $elems = nqp::elems($target);
        nqp::atpos($target,$p) = rviter.pull-one
          while nqp::islt_i(++$p,$elems);
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
                %keep{$i} := $lv
                  if %keep{$i}:exists and !($lv =:= IterationEnd);
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
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'SINK', $SINK, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'SINK',$SINK,%other )
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,:$delete!,*%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'delete', $delete, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'delete',$delete,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,:$exists!,*%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'exists', $exists, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'exists',$exists,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$kv!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'kv', $kv, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'kv',$kv,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$p!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'p', $p, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'p',$p,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$k!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'k', $k, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'k',$k,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$v!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'v', $v, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'v',$v,%other)
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block ) is raw {
    my $*INDEX = 'Effective index';
    SELF[$block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)))];
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, Mu \assignee ) is raw {
    my $*INDEX = 'Effective index';
    SELF[$block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)))] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$SINK!, *%other) is raw {
    my $*INDEX = 'Effective index';
    SLICE_MORE_LIST( SELF, POSITIONS(SELF,$block), 'SINK', $SINK, %other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$delete!,*%other) is raw {
    my $*INDEX = 'Effective index';
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, 'delete', $delete, %other )
      !! SLICE_MORE_LIST( SELF, @$pos, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,:$exists!,*%other) is raw {
    my $*INDEX = 'Effective index';
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, 'exists', $exists, %other )
      !! SLICE_MORE_LIST( SELF, @$pos, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$kv!, *%other) is raw {
    my $*INDEX = 'Effective index';
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, 'kv', $kv, %other )
      !! SLICE_MORE_LIST( SELF, @$pos, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$p!, *%other) is raw {
    my $*INDEX = 'Effective index';
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, 'p', $p, %other )
      !! SLICE_MORE_LIST( SELF, @$pos, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$k!, *%other) is raw {
    my $*INDEX = 'Effective index';
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, 'k', $k, %other )
      !! SLICE_MORE_LIST( SELF, @$pos, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$v!, *%other) is raw {
    my $*INDEX = 'Effective index';
    my $pos := $block(|(SELF.cache.elems xx ($block.count == Inf ?? 1 !! $block.count)));
    nqp::istype($pos,Int)
      ?? SLICE_ONE_LIST(  SELF,  $pos, 'v', $v, %other )
      !! SLICE_MORE_LIST( SELF, @$pos, 'v', $v, %other );
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
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$delete!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$exists!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$kv!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$p!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$k!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$v!, *%other) is raw {
    nqp::elems(nqp::getattr(%other,Map,'$!storage'))
      ?? SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'v', $v, %other )
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
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'SINK', $SINK, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$delete!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$exists!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$kv!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$p!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$k!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, :$v!, *%other) is raw {
    nqp::elems(nqp::getattr(%other,Map,'$!storage'))
      ?? SLICE_MORE_LIST( SELF, ^SELF.cache.elems, 'v', $v, %other )
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
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', target)
}

multi sub postcircumfix:<[; ]>(\SELF, @indices) is raw {
    my int $elems = @indices.elems;
    my $indices := nqp::getattr(@indices,List,'$!reified');
    my int $i = -1;

    return MD-ARRAY-SLICE(SELF,@indices)
      unless nqp::istype(nqp::atpos($indices,$i), Int)
      while nqp::islt_i(++$i,$elems);

    SELF.AT-POS(|@indices)
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, Mu \assignee) is raw {
    my int $elems = @indices.elems;
    my $indices := nqp::getattr(@indices,List,'$!reified');
    my int $i = -1;

    return MD-ARRAY-SLICE(SELF, @indices) = assignee
      unless nqp::istype(nqp::atpos($indices,$i), Int)
      while nqp::islt_i(++$i,$elems);

    SELF.ASSIGN-POS(|@indices, assignee)
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$exists!) is raw {
    if $exists {
        my int $elems = @indices.elems;
        my $indices := nqp::getattr(@indices,List,'$!reified');
        my int $i = -1;
        fail X::NYI.new(feature => ':exists on multi-dimensional slices')
          unless nqp::istype(nqp::atpos($indices,$i), Int)
          while nqp::islt_i(++$i,$elems);

        SELF.EXISTS-POS(|@indices)
    }
    else {
        SELF[@indices]
    }
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$delete!) is raw {
    if $delete {
        my int $elems = @indices.elems;
        my $indices := nqp::getattr(@indices,List,'$!reified');
        my int $i = -1;
        fail X::NYI.new(feature => ':delete on multi-dimensional slices')
          unless nqp::istype(nqp::atpos($indices,$i), Int)
          while nqp::islt_i(++$i,$elems);

        SELF.DELETE-POS(|@indices)
    }
    else {
        SELF[@indices]
    }
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$BIND!) is raw {
    my int $elems = @indices.elems;
    my $indices := nqp::getattr(@indices,List,'$!reified');
    my int $i = -1;
    X::Bind::Slice.new(type => SELF.WHAT).throw
      unless nqp::istype(nqp::atpos($indices,$i), Int)
      while nqp::islt_i(++$i,$elems);

    SELF.BIND-POS(|@indices, $BIND)
}

# vim: ft=perl6 expandtab sw=4
