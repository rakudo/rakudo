# all sub postcircumfix [;] candidates here please
proto sub postcircumfix:<[; ]>($, $, Mu $?, *%) is nodal {*}

sub MD-ARRAY-SLICE-ONE-POSITION(
  \SELF, \indices, \idx, int $dim, \target
) is raw is implementation-detail {
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
            MD-ARRAY-SLICE-ONE-POSITION(SELF, indices, idx.(|(SELF.elems xx (idx.count == Inf ?? 1 !! idx.count))), $dim, target);
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
            nqp::push(target, SELF.AT-POS(idx.(|(SELF.elems xx (idx.count == Inf ?? 1 !! idx.count)))))
        }
        else {
            nqp::push(target, SELF.AT-POS(idx.Int))
        }
    }
}
sub MD-ARRAY-SLICE(\SELF, @indices) is raw is implementation-detail {
    my \target = nqp::create(IterationBuffer);
    MD-ARRAY-SLICE-ONE-POSITION(SELF, @indices, @indices.AT-POS(0), 0, target);
    target.List
}

multi sub postcircumfix:<[; ]>(\SELF, @indices) is raw {
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my int $elems = nqp::elems(indices);
    my int $i = -1;
    my \idxs := nqp::list_i;

    nqp::while(
      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
      nqp::if(
        nqp::istype((my \index := nqp::atpos(indices,$i)),Int),
        nqp::push_i(idxs,index),               # it's an Int, use that
        nqp::if(
          nqp::istype(index,Numeric),
          nqp::push_i(idxs,index.Int),         # can be safely coerced to Int
          nqp::if(
            nqp::istype(index,Str),
            nqp::if(
              nqp::istype((my \coerced := index.Int),Failure),
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
      nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
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

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$BIND!) is raw {
    my int $elems = @indices.elems;   # reifies
    my \indices := nqp::getattr(@indices,List,'$!reified');
    my int $i = -1;

    nqp::while(
      nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
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
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':delete on multi-dimensional slices')),
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
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':exists on multi-dimensional slices')),
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
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':kv on multi-dimensional slices')),
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
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':p on multi-dimensional slices')),
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
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':k on multi-dimensional slices')),
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
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos(indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':v on multi-dimensional slices')),
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
