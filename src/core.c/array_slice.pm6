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
proto sub POSITIONS(|) is implementation-detail {*}
multi sub POSITIONS(
  \SELF,
  \pos,
  Callable :$eagerize = -> \idx {
      nqp::istype(idx,Whatever)
        ?? nqp::isconcrete(SELF) && SELF.elems  # 0 if not concrete
        !! SELF.EXISTS-POS(idx)
  }
) {
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

    # we can optimize `42..*` Ranges; as long as they're from core, unmodified
    my \is-pos-lazy = pos.is-lazy;
    my \pos-iter    = nqp::eqaddr(pos.WHAT,Range)
        && pos.max === Inf
        && nqp::isfalse(SELF.is-lazy)
          ?? Range.new(pos.min, SELF.elems,
              :excludes-min(pos.excludes-min),
              :excludes-max(pos.excludes-max)
          ).iterator
          !! pos.iterator;

    my \pos-list = nqp::create(List);
    my \eager-indices = nqp::create(IterationBuffer);
    my \target = IndicesReificationTarget.new(eager-indices, $eagerize);
    nqp::bindattr(pos-list, List, '$!reified', eager-indices);

    if is-pos-lazy {
        # With lazy indices, we truncate at the first one that fails to exists.
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
    else {
        pos-iter.push-all: target;
    }
    pos-list
}

proto sub postcircumfix:<[ ]>($, |) is nodal {*}

multi sub postcircumfix:<[ ]>( \SELF, Any:U $type, |c ) is raw {
    die "Unable to call postcircumfix {try SELF.VAR.name}[ $type.gist() ] with a type object\n"
      ~ "Indexing requires a defined object";
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
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$delete! ) is raw {
    $delete ?? SELF.DELETE-POS($pos) !! SELF.AT-POS($pos)
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$delete!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, 'delete', $delete, %other )
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$exists! ) is raw {
    $exists ?? SELF.EXISTS-POS($pos) !! !SELF.EXISTS-POS($pos)
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$exists!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, $pos, 'exists', $exists, %other )
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$kv!, *%other ) is raw {
    $kv && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? ($pos, SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$p!, *%other ) is raw {
    $p && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? Pair.new($pos,SELF.AT-POS($pos)) !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$k!, *%other ) is raw {
    $k && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS($pos) ?? $pos !! ())
      !! SLICE_ONE_LIST( SELF, $pos, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Int:D $pos, Bool() :$v!, *%other ) is raw {
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
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$delete! ) is raw {
    $delete ?? SELF.DELETE-POS(pos.Int) !! SELF.AT-POS(pos.Int)
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$delete!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, 'delete', $delete, %other )
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$exists! ) is raw {
    $exists ?? SELF.EXISTS-POS(pos.Int) !! !SELF.EXISTS-POS(pos.Int)
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$exists!, *%other ) is raw {
    SLICE_ONE_LIST( SELF, pos.Int, 'exists', $exists, %other )
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$kv!, *%other ) is raw {
    $kv && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS(pos.Int) ?? (pos, SELF.AT-POS(pos.Int)) !! ())
      !! SLICE_ONE_LIST( SELF, pos.Int, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$p!, *%other ) is raw {
    $p && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS(pos.Int) ?? Pair.new(pos, SELF.AT-POS(pos.Int)) !! ())
      !! SLICE_ONE_LIST( SELF, pos.Int, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$k!, *%other ) is raw {
    $k && nqp::not_i(nqp::elems(nqp::getattr(%other,Map,'$!storage')))
      ?? (SELF.EXISTS-POS(pos.Int) ?? pos !! ())
      !! SLICE_ONE_LIST( SELF, pos.Int, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>( \SELF, Any:D \pos, Bool() :$v!, *%other ) is raw {
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

multi sub postcircumfix:<[ ]>(
  \SELF, Iterable:D \positions, Mu \values
) is raw {
    # MMD is not behaving itself so we do this by hand.
    if nqp::iscont(positions) {
        SELF.ASSIGN-POS(positions.Int, values);
    }
    else {

        # Set up iterators for positions and values
        my $pos-iter := positions.iterator;
        my $val-iter := Rakudo::Iterator.TailWith(
          nqp::iscont(values)
            ?? Rakudo::Iterator.OneValue(nqp::decont(values))
            !! values.iterator,
          Nil
        );

        # Temporary holding area for containers and values.  In the fast
        # path, the $containers buffer will contain the needed result.
        # However, if one the the positions was in fact a List, then the
        # result will differ from what we put in the $containers buffer.
        # When a List is encountered, the $result buffer will be initialized
        # with whatever was in $containers so far, and then the two will
        # start to differ.  In the end, either the $result buffer is returned
        # (if there is one) or the $containers buffer.
        #
        # Note that all of these buffers are completely agnostic about where
        # they are from: $result and $containers just contain containers that
        # match elements in the Positional that is SELF, and $values just
        # contains values to be put into the containers when all has been
        # figured out.
        my $result     := nqp::null;
        my $containers := nqp::create(IterationBuffer);
        my $values     := nqp::create(IterationBuffer);

        # Make sure we handle lazy positions and Callables if we can
        my $elems := SELF.elems;
        $pos-iter := $pos-iter.is-lazy
          ?? Rakudo::Iterator.PosWithinRange($pos-iter, $elems)
          !! Rakudo::Iterator.PosWithCallables($pos-iter, $elems)
          unless nqp::istype($elems,Failure);

        # Handle Whatever in the generated positions
        my sub handle-whatever() {
            my int $i    = -1;
            my int $todo = $elems;
            nqp::if(
              nqp::isnull($result),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$todo),
                nqp::push($containers,SELF.AT-POS($i)),
                nqp::push($values,$val-iter.pull-one)
              ),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$todo),
                nqp::push($result,nqp::push($containers,SELF.AT-POS($i))),
                nqp::push($values,$val-iter.pull-one)
              )
            );
        }

        # Handle iterator in the generated positions
        my sub handle-iterator(\iterator) {
            my $pos;
            nqp::if(
              nqp::isnull($result),
              nqp::until(               # no embedded List seen yet
                nqp::eqaddr(($pos := iterator.pull-one),IterationEnd),
                nqp::if(
                  nqp::istype($pos,Int),
                  nqp::stmts(
                    nqp::if(
                      nqp::isnull($result),
                      nqp::push($containers,SELF.AT-POS($pos)),
                      nqp::push(
                        $result,
                        nqp::push($containers,SELF.AT-POS($pos))
                      )
                    ),
                    nqp::push($values,$val-iter.pull-one)
                  ),
                  handle-nonInt($pos)   # may change nullness of $result
                )
              ),
              nqp::until(               # already seen an embedded List
                nqp::eqaddr(($pos := iterator.pull-one),IterationEnd),
                nqp::if(
                  nqp::istype($pos,Int),
                  nqp::stmts(
                    nqp::push(
                      $result,
                      nqp::push($containers,SELF.AT-POS($pos))
                    ),
                    nqp::push($values,$val-iter.pull-one)
                  ),
                  handle-nonInt($pos)
                )
              )
            );
        }

        # Handle List in the generated positions
        my sub handle-list(\list) {
            my $iterator := list.iterator;
            my $buffer   := nqp::create(IterationBuffer);

            # Set up alternate result handling
            $result := nqp::clone($containers) if nqp::isnull($result);
            nqp::push($containers,nqp::push($result,my $));
            nqp::push($values,$buffer.List);

            nqp::until(
              nqp::eqaddr((my \pos := $iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype(pos,Int),
                nqp::stmts(
                  nqp::push(
                    $containers,
                    nqp::push($buffer,SELF.AT-POS(pos))
                  ),
                  nqp::push($values,$val-iter.pull-one)
                ),
                handle-nonInt(pos)
              )
            );
        }

        # Handle anything non-integer in the generated positions
        my sub handle-nonInt(\pos) {
            nqp::if(
              nqp::istype(pos,List),
              handle-list(pos),
              nqp::if(
                nqp::istype(pos,Iterable),
                handle-iterator(pos.iterator),
                nqp::if(
                  nqp::istype(pos,Whatever),
                  handle-whatever,
                  nqp::stmts(
                    nqp::if(
                      nqp::isnull($result),
                      nqp::push($containers,SELF.AT-POS(pos.Int)),
                      nqp::push(
                        $result,
                        nqp::push($containers,SELF.AT-POS(pos.Int))
                      )
                    ),
                    nqp::push($values,$val-iter.pull-one)
                  )
                )
              )
            );
        }

        # Set up containers and values to be put into them, to allow
        # referring to SELF in different order (aka @a[1,0] = @a[0,1])
        nqp::until(
          nqp::eqaddr((my \pos := $pos-iter.pull-one),IterationEnd),
          nqp::if(
            nqp::istype(pos,Int),
            nqp::stmts(
              nqp::if(
                nqp::isnull($result),
                nqp::push($containers,SELF.AT-POS(pos)),
                nqp::push(
                  $result,
                  nqp::push($containers,SELF.AT-POS(pos))
                )
              ),
              nqp::push($values,$val-iter.pull-one)
            ),
            handle-nonInt(pos)   # may change nullness of $result
          )
        );

        # Do the actual assignments until there's nothing to assign anymore
        my int $i = -1;
        nqp::while(
          nqp::elems($values),
          nqp::atpos($containers,$i = nqp::add_i($i,1)) = nqp::shift($values)
        );

        nqp::ifnull($result,$containers).List
    }
}
multi sub postcircumfix:<[ ]>(
  \SELF, Iterable:D \positions, :$BIND! is raw
) is raw {
    # MMD is not behaving itself so we do this by hand.
    if nqp::iscont(positions) {
        SELF.BIND-POS(positions.Int, $BIND)
    }
    else {

        # Set up iterators for positions and values
        my $pos-iter := positions.iterator;
        my $val-iter := Rakudo::Iterator.TailWith($BIND.iterator, Nil);

        # Make sure we handle lazy positions and Callables if we can
        my $elems := SELF.elems;
        $pos-iter := $pos-iter.is-lazy
          ?? Rakudo::Iterator.PosWithinRange($pos-iter, $elems)
          !! Rakudo::Iterator.PosWithCallables($pos-iter, $elems)
          unless nqp::istype($elems,Failure);

        # Do the actual binding until there are no positions anymore
        my $values := nqp::create(IterationBuffer);
        nqp::until(
          nqp::eqaddr((my \pos := $pos-iter.pull-one),IterationEnd),
          nqp::push($values, SELF.BIND-POS(pos, $val-iter.pull-one))
        );

        $values.List
    }
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,Bool() :$delete!,*%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'delete', $delete, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'delete',$delete,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos,Bool() :$exists!,*%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'exists', $exists, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'exists',$exists,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, Bool() :$kv!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'kv', $kv, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'kv',$kv,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, Bool() :$p!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'p', $p, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'p',$p,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, Bool() :$k!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'k', $k, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'k',$k,%other)
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, Bool() :$v!, *%other) is raw {
    nqp::iscont(pos)
        ?? SLICE_ONE_LIST( SELF, pos.Int, 'v', $v, %other )
        !! SLICE_MORE_LIST(SELF,POSITIONS(SELF,pos),'v',$v,%other)
}

# @a[->{}]
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block ) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
      ?? $pos
      !! SELF[$pos]
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, Mu \assignee ) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
      ?? $pos
      !! (SELF[$pos] = assignee)
}
multi sub postcircumfix:<[ ]>(\SELF, Callable:D $block, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,Bool() :$delete!,*%other) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
      ?? $pos
      !! nqp::istype($pos,Int)
        ?? SLICE_ONE_LIST( SELF,  $pos, 'delete', $delete, %other)
        !! SLICE_MORE_LIST(SELF, @$pos, 'delete', $delete, %other)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,Bool() :$exists!,*%other) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
      ?? $pos
      !! nqp::istype($pos,Int)
        ?? SLICE_ONE_LIST( SELF,  $pos, 'exists', $exists, %other)
        !! SLICE_MORE_LIST(SELF, @$pos, 'exists', $exists, %other)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,Bool() :$kv!,*%other) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
     ?? $pos
     !! nqp::istype($pos,Int)
       ?? SLICE_ONE_LIST( SELF,  $pos, 'kv', $kv, %other)
       !! SLICE_MORE_LIST(SELF, @$pos, 'kv', $kv, %other)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,Bool() :$p!,*%other) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
      ?? $pos
      !! nqp::istype($pos,Int)
        ?? SLICE_ONE_LIST(  SELF,  $pos, 'p', $p, %other )
        !! SLICE_MORE_LIST( SELF, @$pos, 'p', $p, %other )
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,Bool() :$k!,*%other) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
      ?? $pos
      !! nqp::istype($pos,Int)
        ?? SLICE_ONE_LIST( SELF,  $pos, 'k', $k, %other)
        !! SLICE_MORE_LIST(SELF, @$pos, 'k', $k, %other)
}
multi sub postcircumfix:<[ ]>(\SELF,Callable:D $block,Bool() :$v!,*%other) is raw {
    my $*INDEX := 'Effective index';
    nqp::istype((my $pos := $block.POSITIONS(SELF)),Failure)
      ?? $pos
      !! nqp::istype($pos,Int)
        ?? SLICE_ONE_LIST( SELF,  $pos, 'v', $v, %other)
        !! SLICE_MORE_LIST(SELF, @$pos, 'v', $v, %other)
}

# @a[*]
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D ) is raw {
    SELF[^SELF.elems];
}
multi sub postcircumfix:<[ ]>( \SELF, Whatever:D, Mu \assignee ) is raw {
    SELF[^SELF.elems] = assignee;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, :$BIND!) is raw {
    X::Bind::Slice.new(type => SELF.WHAT).throw;
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, Bool() :$delete!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, Bool() :$exists!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, Bool() :$kv!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, Bool() :$p!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, Bool() :$k!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Whatever:D, Bool() :$v!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'v', $v, %other )
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
multi sub postcircumfix:<[ ]>(\SELF, Bool() :$delete!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'delete', $delete, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Bool() :$exists!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'exists', $exists, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Bool() :$kv!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'kv', $kv, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Bool() :$p!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'p', $p, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Bool() :$k!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'k', $k, %other );
}
multi sub postcircumfix:<[ ]>(\SELF, Bool() :$v!, *%other) is raw {
    SLICE_MORE_LIST( SELF, ^SELF.elems, 'v', $v, %other )
}
multi sub postcircumfix:<[ ]>(\SELF, *%other) is raw {
    SELF.ZEN-POS(|%other);
}

# vim: expandtab shiftwidth=4
