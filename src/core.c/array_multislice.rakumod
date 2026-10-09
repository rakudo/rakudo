# all sub postcircumfix [;] candidates here please
proto sub postcircumfix:<[; ]>($, $, Mu $?, *%) is nodal {*}

sub MD-ARRAY-SLICE-ONE-POSITION(
  \SELF, \indices, \idx, int $dim, \target
) is raw is implementation-detail {
    my int $next-dim = $dim + 1;
    if $next-dim < indices.elems {
        if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
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
            MD-ARRAY-SLICE-ONE-POSITION(SELF, indices, idx.(|(SELF.elems xx (idx.count == Inf ?? 1 !! idx.count))), $dim, target);
        }
        else  {
            MD-ARRAY-SLICE-ONE-POSITION(SELF.AT-POS(idx.Int), indices, indices.AT-POS($next-dim), $next-dim, target)
        }
    }
    else {
        if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
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
            nqp::push(target, SELF.AT-POS(idx.(|(SELF.elems xx (idx.count == Inf ?? 1 !! idx.count)))))
        }
        else {
            nqp::push(target, SELF.AT-POS(idx.Int))
        }
    }
}

# The indices of a subscript that interpolates a list with prefix:<||>,
# refusing a lazy list as their number of dimensions can't be known, and
# throwing a Failure a hyper subscript over no elements would drop
sub MD-INTERPOLATED-INDICES(Mu \indices) is raw is implementation-detail {
    nqp::istype(indices,Failure)
      ?? indices.self
      !! nqp::istype(indices,Junction)
        ?? indices.THREAD(&MD-INTERPOLATED-INDICES)
        !! indices.is-lazy
          ?? X::Cannot::Lazy.new(
               :action('take the dimensions of a subscript from')
             ).throw
          !! nqp::istype(indices,Positional)
               && nqp::isconcrete(indices)
               && nqp::not_i(nqp::istype(indices,List))
            ?? indices.List  # a Range, native array or Blob
            !! indices
}

# The indices a slice of a shaped or jagged array stands for, or Nil for no
# change. Each dimension left out takes *, as does a trailing ** for any
# number of them, and a lazy index takes only those within a set length.
sub MD-SHAPED-INDICES(\SELF, @indices) is implementation-detail {
    return Nil
      unless nqp::istype(SELF,Rakudo::Internals::ShapedArrayCommon)
        || nqp::istype(SELF,Array::ShapedView)
        || nqp::istype(SELF,Array::Jagged);

    my int $elems = @indices.elems;  # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my int $hyper = $elems && nqp::istype(
      nqp::decont(nqp::atpos(indices,nqp::sub_i($elems,1))),HyperWhatever
    );
    my int $given = nqp::sub_i($elems,$hyper);
    my int $slice = $hyper;
    my int $i = -1;
    nqp::while(
      nqp::islt_i(++$i,$given),
      nqp::if(
        nqp::istype((my \index := nqp::atpos(indices,$i)),HyperWhatever),
        die("Only the last dimension of a subscript can take **"),
        nqp::if(
          nqp::istype(index,Whatever)
            || nqp::istype(index,Callable)
            || (nqp::istype(index,Iterable) && nqp::not_i(nqp::iscont(index))),
          ($slice = 1)
        )
      )
    );
    return Nil unless $slice;

    # a lazy index takes the indices within a set length
    my \shape   := SELF.shape;
    my \sliced  := nqp::create(IterationBuffer);
    my int $changed = $hyper || $given < shape.elems;
    $i = -1;
    while ++$i < $given {
        my \index := nqp::atpos(indices,$i);
        if nqp::istype(index,Iterable)
          && nqp::not_i(nqp::iscont(index))
          && index.is-lazy
          && nqp::not_i(nqp::istype(shape.AT-POS($i),Whatever)) {
            $changed = 1;
            my int $length = shape.AT-POS($i);
            my \within  := nqp::create(IterationBuffer);
            my \iterator := index.iterator;
            nqp::until(
              nqp::eqaddr((my \pos := iterator.pull-one),IterationEnd)
                || nqp::isge_i(pos.Int,$length),
              nqp::push(within,pos)
            );
            nqp::push(sliced,within.List);
        }
        else {
            # An index can be a Seq, which iterates when sunk, so the index
            # pushed must not be this block's value
            nqp::push(sliced,index);
            Nil
        }
    }
    nqp::push(sliced,Whatever) while $i++ < shape.elems;
    $changed ?? sliced.List !! Nil
}

# Whether a position is within a shaped array, which refuses one that is
# not, while any other array takes any position
sub MD-POS-WITHIN(\target, \pos) is implementation-detail {
    nqp::istype(target,Rakudo::Internals::ShapedArrayCommon)
      || nqp::istype(target,Array::ShapedView)
      ?? 0 <= pos < target.elems
      !! nqp::istype(target,Array::Jagged)
        ?? target.POS-WITHIN(pos)
        !! True
}

# The result of an adverb for each element a multidimensional slice takes,
# as the adverb gives it for a slice of one dimension, with the list of the
# index of each dimension as its key
sub MD-ARRAY-SLICE-ADVERB(\SELF, @given, str $adverb) is implementation-detail {
    my \indices := MD-SHAPED-INDICES(SELF, @given) // @given;
    my \result  := nqp::create(IterationBuffer);
    my \path    := nqp::create(IterationBuffer);
    my int $last  = nqp::sub_i(indices.elems,1);  # reifies

    my sub walk(\target, int $dim, \idx --> Nil) {
        if nqp::istype(idx,Iterable) && nqp::not_i(nqp::iscont(idx)) {
            if idx.is-lazy {
                # a lazy index stops at the end of the array
                my int $elems = nqp::isconcrete(target) ?? target.elems !! 0;
                for idx -> \pos {
                    last if nqp::istype(pos,Numeric) && pos.Int >= $elems;
                    walk(target, $dim, pos);
                }
            }
            else {
                walk(target, $dim, $_) for idx;
            }
        }
        elsif nqp::istype(idx,Whatever) {
            walk(target, $dim, $_) for ^target.elems;
        }
        elsif nqp::istype(idx,Callable) {
            walk(target, $dim,
              idx.(|(target.elems xx (idx.count == Inf ?? 1 !! idx.count))));
        }
        else {
            my $pos := idx.Int;
            my int $within = MD-POS-WITHIN(target, $pos);
            nqp::push(path,$pos);
            # Within nqp::stmts, as a statement giving the value pushed could
            # sink it, which throws a Failure and iterates a Seq
            nqp::stmts(
              nqp::if(
                nqp::islt_i($dim,$last),
                walk($within ?? target.AT-POS($pos) !! Any,
                  nqp::add_i($dim,1), indices.AT-POS(nqp::add_i($dim,1))),
                nqp::if(
                  nqp::iseq_s($adverb,'exists'),
                  nqp::push(result,$within ?? target.EXISTS-POS($pos) !! False),
                  nqp::if(
                    nqp::iseq_s($adverb,'delete'),
                    nqp::push(result,$within ?? target.DELETE-POS($pos) !! Nil),
                    nqp::if(
                      $within && target.EXISTS-POS($pos),
                      nqp::stmts(
                        (my \key := nqp::p6bindattrinvres(
                          nqp::create(List),List,'$!reified',nqp::clone(path))),
                        nqp::iseq_s($adverb,'k')
                          ?? nqp::push(result,key)
                          !! nqp::iseq_s($adverb,'v')
                            ?? nqp::push(result,nqp::decont(target.AT-POS($pos)))
                            !! nqp::iseq_s($adverb,'kv')
                              ?? nqp::stmts(
                                   nqp::push(result,key),
                                   nqp::push(result,target.AT-POS($pos))
                                 )
                              !! nqp::push(result,Pair.new(key,target.AT-POS($pos)))
                      )
                    )
                  )
                )
              ),
              nqp::pop(path)
            );
        }
    }

    walk(SELF, 0, indices.AT-POS(0));
    result.List
}

sub MD-ARRAY-SLICE(\SELF, @given) is raw is implementation-detail {
    my \indices := MD-SHAPED-INDICES(SELF, @given) // @given;
    my \target = nqp::create(IterationBuffer);
    MD-ARRAY-SLICE-ONE-POSITION(SELF, indices, indices.AT-POS(0), 0, target);
    target.List
}

multi sub postcircumfix:<[; ]>(\SELF, @indices) is raw {
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my int $elems = nqp::elems(indices);
    my int $i = -1;
    my \idxs := nqp::list_i;

    nqp::while(
      nqp::islt_i(++$i,$elems),
      nqp::if(
        nqp::istype((my $index = nqp::atpos(indices,$i)),Int),
        nqp::push_i(idxs,$index),               # it's an Int, use that
        nqp::if(
          nqp::istype($index,Numeric),
          nqp::push_i(idxs,$index.Int),         # can be safely coerced to Int
          nqp::if(
            nqp::istype($index,Str),
            nqp::if(
              nqp::istype((my \coerced := $index.Int),Failure),
              coerced.throw,                   # alas, not numeric, bye!
              nqp::push_i(idxs,coerced)        # could be coerced to Int
            ),
            (return-rw MD-ARRAY-SLICE(SELF,@indices)) # alas, slow path needed
          )
        )
      )
    );

    nqp::if(                                   # we have all Ints
      nqp::iseq_i($elems,2),
      SELF.AT-POS(                             # fast pathing [n;n]
        nqp::atpos_i(idxs,0),
        nqp::atpos_i(idxs,1)
      ),
      nqp::if(
        nqp::iseq_i($elems,3),
        SELF.AT-POS(                           # fast pathing [n;n;n]
          nqp::atpos_i(idxs,0),
          nqp::atpos_i(idxs,1),
          nqp::atpos_i(idxs,2)
        ),
        SELF.AT-POS(|@indices)                 # alas >3 dims, slow path
      )
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, Mu \assignee) is raw {
    my int $elems = @indices.elems;   # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my int $i = -1;

    nqp::while(
      nqp::islt_i(++$i,$elems)
        && nqp::istype(nqp::atpos(indices,$i),Int),
      nqp::null
    );

    nqp::if(
      nqp::islt_i($i,$elems),
      (MD-ARRAY-SLICE(SELF,@indices) = assignee),
      nqp::if(
        nqp::iseq_i($elems,2),
        SELF.ASSIGN-POS(
          nqp::atpos(indices,0),
          nqp::atpos(indices,1),
          assignee
        ),
        nqp::if(
          nqp::iseq_i($elems,3),
          SELF.ASSIGN-POS(
            nqp::atpos(indices,0),
            nqp::atpos(indices,1),
            nqp::atpos(indices,2),
            assignee
          ),
          SELF.ASSIGN-POS(|@indices,assignee)
        )
      )
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, Mu :$BIND! is raw) is raw {
    my int $elems = @indices.elems;   # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my int $i = -1;

    nqp::while(
      nqp::islt_i(++$i,$elems)
        && nqp::istype(nqp::atpos(indices,$i),Int),
      nqp::null
    );

    nqp::if(
      nqp::islt_i($i,$elems),
      X::Bind::Slice.new(type => SELF.WHAT).throw,
      nqp::if(
        nqp::iseq_i($elems,2),
        SELF.BIND-POS(
          nqp::atpos(indices,0),
          nqp::atpos(indices,1),
          $BIND
        ),
        nqp::if(
          nqp::iseq_i($elems,3),
          SELF.BIND-POS(
            nqp::atpos(indices,0),
            nqp::atpos(indices,1),
            nqp::atpos(indices,2),
            $BIND
          ),
          SELF.BIND-POS(|@indices, $BIND)
        )
      )
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$delete!) is raw {
    nqp::if(
      $delete,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my \indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(++$i,$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          MD-ARRAY-SLICE-ADVERB(SELF, @indices, 'delete'),
          nqp::if(
            nqp::iseq_i($elems,2),
            SELF.DELETE-POS(
              nqp::atpos(indices,0),
              nqp::atpos(indices,1)
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              SELF.DELETE-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1),
                nqp::atpos(indices,2)
              ),
              SELF.DELETE-POS(|@indices)
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$exists!) is raw {
    nqp::if(
      $exists,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my \indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(++$i,$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          MD-ARRAY-SLICE-ADVERB(SELF, @indices, 'exists'),
          nqp::if(
            nqp::iseq_i($elems,2),
            SELF.EXISTS-POS(
              nqp::atpos(indices,0),
              nqp::atpos(indices,1)
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              SELF.EXISTS-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1),
                nqp::atpos(indices,2)
              ),
              SELF.EXISTS-POS(|@indices)
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$kv!) is raw {
    nqp::if(
      $kv,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my \indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(++$i,$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          MD-ARRAY-SLICE-ADVERB(SELF, @indices, 'kv'),
          nqp::if(
            nqp::iseq_i($elems,2),
            nqp::if(
              SELF.EXISTS-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1)
              ),
              (@indices, SELF.AT-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1)
              )),
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos(indices,0),
                  nqp::atpos(indices,1),
                  nqp::atpos(indices,2)
                ),
                (@indices, SELF.AT-POS(
                  nqp::atpos(indices,0),
                  nqp::atpos(indices,1),
                  nqp::atpos(indices,2)
                )),
                ()
              ),
              nqp::if(
                SELF.EXISTS-POS(|@indices),
                (@indices, SELF.AT-POS(|@indices)),
                ()
              )
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$p!) is raw {
    nqp::if(
      $p,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my \indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(++$i,$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          MD-ARRAY-SLICE-ADVERB(SELF, @indices, 'p'),
          nqp::if(
            nqp::iseq_i($elems,2),
            nqp::if(
              SELF.EXISTS-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1)
              ),
              Pair.new(@indices, SELF.AT-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1)
              )),
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos(indices,0),
                  nqp::atpos(indices,1),
                  nqp::atpos(indices,2)
                ),
                Pair.new(@indices, SELF.AT-POS(
                  nqp::atpos(indices,0),
                  nqp::atpos(indices,1),
                  nqp::atpos(indices,2)
                )),
                ()
              ),
              nqp::if(
                SELF.EXISTS-POS(|@indices),
                Pair.new(@indices, SELF.AT-POS(|@indices)),
                ()
              )
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$k!) is raw {
    nqp::if(
      $k,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my \indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(++$i,$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          MD-ARRAY-SLICE-ADVERB(SELF, @indices, 'k'),
          nqp::if(
            nqp::iseq_i($elems,2),
            nqp::if(
              SELF.EXISTS-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1)
              ),
              @indices,
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos(indices,0),
                  nqp::atpos(indices,1),
                  nqp::atpos(indices,2)
                ),
                @indices,
                ()
              ),
              nqp::if(
                SELF.EXISTS-POS(|@indices),
                @indices,
                ()
              )
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$v!) is raw {
    nqp::if(
      $v,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my \indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(++$i,$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          MD-ARRAY-SLICE-ADVERB(SELF, @indices, 'v'),
          nqp::if(
            nqp::iseq_i($elems,2),
            nqp::if(
              SELF.EXISTS-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1)
              ),
              nqp::decont(SELF.AT-POS(
                nqp::atpos(indices,0),
                nqp::atpos(indices,1)
              )),
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos(indices,0),
                  nqp::atpos(indices,1),
                  nqp::atpos(indices,2)
                ),
                nqp::decont(SELF.AT-POS(
                  nqp::atpos(indices,0),
                  nqp::atpos(indices,1),
                  nqp::atpos(indices,2)
                )),
                ()
              ),
              nqp::if(
                SELF.EXISTS-POS(|@indices),
                nqp::decont(SELF.AT-POS(|@indices)),
                ()
              )
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}

# vim: expandtab shiftwidth=4
