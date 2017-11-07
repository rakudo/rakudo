# all sub postcircumfix [; ] and sub postcircumfix {; } candidates here please
proto sub postcircumfix:<[; ]>(|) is nodal { * }
proto sub postcircumfix:<{; }>(|) is nodal { * }

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

sub MD-HASH-SLICE-ONE-POSITION(\SELF, \indices, \idx, int $dim, \target) {
    my int $next-dim = $dim + 1;
    if $next-dim < indices.elems {
        if nqp::istype(idx, Iterable) && !nqp::iscont(idx) {
            for idx {
                MD-HASH-SLICE-ONE-POSITION(SELF, indices, $_, $dim, target)
            }
        }
        elsif nqp::istype(idx, Str) {
            MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY(idx), indices, indices.AT-POS($next-dim), $next-dim, target)
        }
        elsif nqp::istype(idx, Whatever) {
            for SELF.keys {
                MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY($_), indices, indices.AT-POS($next-dim), $next-dim, target)
            }
        }
        else  {
            MD-HASH-SLICE-ONE-POSITION(SELF.AT-KEY(idx), indices, indices.AT-POS($next-dim), $next-dim, target)
        }
    }
    else {
        if nqp::istype(idx, Iterable) && !nqp::iscont(idx) {
            for idx {
                MD-HASH-SLICE-ONE-POSITION(SELF, indices, $_, $dim, target)
            }
        }
        elsif nqp::istype(idx, Str) {
            nqp::push(target, SELF.AT-KEY(idx))
        }
        elsif nqp::istype(idx, Whatever) {
            for SELF.keys {
                nqp::push(target, SELF.AT-KEY($_))
            }
        }
        else {
            nqp::push(target, SELF.AT-KEY(idx))
        }
    }
}

sub MD-ARRAY-SLICE(\SELF, @indices) is raw {
    my \target = IterationBuffer.new;
    MD-ARRAY-SLICE-ONE-POSITION(SELF, @indices, @indices.AT-POS(0), 0, target);
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', target)
}

sub MD-HASH-SLICE(\SELF, @indices) is raw {
    my \target = IterationBuffer.new;
    MD-HASH-SLICE-ONE-POSITION(SELF, @indices, @indices.AT-POS(0), 0, target);
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', target)
}

#--- Fetching

multi sub postcircumfix:<[; ]>(\SELF, @indices) is raw {
    nqp::stmts(
      (my $indices := nqp::getattr(@indices,List,'$!reified')),
      (my int $elems = nqp::elems($indices)),

      # Check if a slice was requested at any dimension:
      (my int $i = -1),
      nqp::while(
        nqp::islt_i(($i = nqp::add_i($i,1)),$elems) &&
        !nqp::stmts(
          (my $idx := nqp::atpos($indices,$i)),
             nqp::istype($idx, Iterable)
          || nqp::istype($idx, Whatever)
          || nqp::istype($idx, HyperWhatever),
        ),
        nqp::null
      ),
      nqp::if(
        nqp::islt_i($i,$elems),

        # Return a slice:
        MD-ARRAY-SLICE(SELF,@indices),

        # Return a singular element:
        nqp::if(
          nqp::iseq_i($elems,2),
          SELF.AT-POS(
            nqp::atpos($indices,0),
            nqp::atpos($indices,1)
          ),
          nqp::if(
            nqp::iseq_i($elems,3),
            SELF.AT-POS(
              nqp::atpos($indices,0),
              nqp::atpos($indices,1),
              nqp::atpos($indices,2)
            ),
            SELF.AT-POS(|@indices)
          )
        )
      )
    )
}

multi sub postcircumfix:<{; }>(\SELF, @indices) is raw {
    nqp::stmts(
      (my $indices := nqp::getattr(@indices,List,'$!reified')),
      (my int $elems = nqp::elems($indices)),

      # Check if a slice was requested at any dimension:
      (my int $i = -1),
      nqp::while(
        nqp::islt_i(($i = nqp::add_i($i,1)),$elems) &&
        !nqp::stmts(
          (my $idx := nqp::atpos($indices,$i)),
             nqp::istype($idx, Iterable)
          || nqp::istype($idx, Whatever)
          || nqp::istype($idx, HyperWhatever),
        ),
        nqp::null
      ),
      nqp::if(
        nqp::islt_i($i,$elems),

        # Return a slice:
        MD-HASH-SLICE(SELF,@indices),

        # Return a singular element:
        nqp::if(
          nqp::iseq_i($elems,2),
          SELF.AT-KEY(
            nqp::atpos($indices,0),
            nqp::atpos($indices,1)
          ),
          nqp::if(
            nqp::iseq_i($elems,3),
            SELF.AT-KEY(
              nqp::atpos($indices,0),
              nqp::atpos($indices,1),
              nqp::atpos($indices,2)
            ),
            SELF.AT-KEY(|@indices)
          )
        )
      )
    )
}

#--- Assigning

multi sub postcircumfix:<[; ]>(\SELF, @indices, Mu \assignee) is raw {
    nqp::stmts(
      (my int $elems = @indices.elems),   # reifies
      (my $indices := nqp::getattr(@indices,List,'$!reified')),
      (my int $i = -1),
      nqp::while(
        nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
          && nqp::istype(nqp::atpos($indices,$i),Int),
        nqp::null
      ),
      nqp::if(
        nqp::islt_i($i,$elems),
        (MD-ARRAY-SLICE(SELF,@indices) = assignee),
        nqp::if(
          nqp::iseq_i($elems,2),
          SELF.ASSIGN-POS(
            nqp::atpos($indices,0),
            nqp::atpos($indices,1),
            assignee
          ),
          nqp::if(
            nqp::iseq_i($elems,3),
            SELF.ASSIGN-POS(
              nqp::atpos($indices,0),
              nqp::atpos($indices,1),
              nqp::atpos($indices,2),
              assignee
            ),
            SELF.ASSIGN-POS(|@indices,assignee)
          )
        )
      )
    )
}

#--- Binding

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$BIND!) is raw {
    nqp::stmts(
      (my int $elems = @indices.elems),   # reifies
      (my $indices := nqp::getattr(@indices,List,'$!reified')),
      (my int $i = -1),
      nqp::while(
        nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
          && nqp::istype(nqp::atpos($indices,$i),Int),
        nqp::null
      ),
      nqp::if(
        nqp::islt_i($i,$elems),
        X::Bind::Slice.new(type => SELF.WHAT).throw,
        nqp::if(
          nqp::iseq_i($elems,2),
          SELF.BIND-POS(
            nqp::atpos($indices,0),
            nqp::atpos($indices,1),
            $BIND
          ),
          nqp::if(
            nqp::iseq_i($elems,3),
            SELF.BIND-POS(
              nqp::atpos($indices,0),
              nqp::atpos($indices,1),
              nqp::atpos($indices,2),
              $BIND
            ),
            SELF.BIND-POS(|@indices, $BIND)
          )
        )
      )
    )
}

#--- :delete adverb

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$delete!) is raw {
    nqp::if(
      $delete,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my $indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos($indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':delete on multi-dimensional slices')),
          nqp::if(
            nqp::iseq_i($elems,2),
            SELF.DELETE-POS(
              nqp::atpos($indices,0),
              nqp::atpos($indices,1)
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              SELF.DELETE-POS(
                nqp::atpos($indices,0),
                nqp::atpos($indices,1),
                nqp::atpos($indices,2)
              ),
              SELF.DELETE-POS(|@indices)
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}

#--- :exists adverb

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$exists!) is raw {
    nqp::if(
      $exists,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my $indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos($indices,$i),Int),
          nqp::null
        ),
        nqp::if(
          nqp::islt_i($i,$elems),
          Failure.new(X::NYI.new(
            feature => ':exists on multi-dimensional slices')),
          nqp::if(
            nqp::iseq_i($elems,2),
            SELF.EXISTS-POS(
              nqp::atpos($indices,0),
              nqp::atpos($indices,1)
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              SELF.EXISTS-POS(
                nqp::atpos($indices,0),
                nqp::atpos($indices,1),
                nqp::atpos($indices,2)
              ),
              SELF.EXISTS-POS(|@indices)
            )
          )
        )
      ),
      postcircumfix:<[; ]>(SELF, @indices)
    )
}


multi sub postcircumfix:<{; }>(\SELF, @indices, :$exists!) {
    sub recurse-at-key(\SELF, \indices) {
        my \idx     := indices[0];
        my \exists  := SELF.EXISTS-KEY(idx);
        nqp::if(
            nqp::istype(idx, Iterable),
            idx.map({ |recurse-at-key(SELF, ($_, |indices.skip.cache)) }).List,
            nqp::if(
                nqp::iseq_I(indices.elems, 1),
                exists,
                nqp::if(
                    exists,
                    recurse-at-key(SELF{idx}, indices.skip.cache),
                    nqp::stmts(
                        (my \times := indices.map({ .elems }).reduce(&[*])),
                        nqp::if(
                            nqp::iseq_I(times, 1),
                            False,
                            (False xx times).List
                        )
                    ).head
                )
            )
        );
    }

    recurse-at-key(SELF, @indices)
}

#--- :kv adverb

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$kv!) is raw {
    nqp::if(
      $kv,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my $indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos($indices,$i),Int),
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
                nqp::atpos($indices,0),
                nqp::atpos($indices,1)
              ),
              (@indices, SELF.AT-POS(
                nqp::atpos($indices,0),
                nqp::atpos($indices,1)
              )),
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos($indices,0),
                  nqp::atpos($indices,1),
                  nqp::atpos($indices,2)
                ),
                (@indices, SELF.AT-POS(
                  nqp::atpos($indices,0),
                  nqp::atpos($indices,1),
                  nqp::atpos($indices,2)
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

#--- :p adverb

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$p!) is raw {
    nqp::if(
      $p,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my $indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos($indices,$i),Int),
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
                nqp::atpos($indices,0),
                nqp::atpos($indices,1)
              ),
              Pair.new(@indices, SELF.AT-POS(
                nqp::atpos($indices,0),
                nqp::atpos($indices,1)
              )),
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos($indices,0),
                  nqp::atpos($indices,1),
                  nqp::atpos($indices,2)
                ),
                Pair.new(@indices, SELF.AT-POS(
                  nqp::atpos($indices,0),
                  nqp::atpos($indices,1),
                  nqp::atpos($indices,2)
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

#--- :k adverb

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$k!) is raw {
    nqp::if(
      $k,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my $indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos($indices,$i),Int),
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
                nqp::atpos($indices,0),
                nqp::atpos($indices,1)
              ),
              @indices,
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos($indices,0),
                  nqp::atpos($indices,1),
                  nqp::atpos($indices,2)
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

#--- :v adverb

multi sub postcircumfix:<[; ]>(\SELF, @indices, :$v!) is raw {
    nqp::if(
      $v,
      nqp::stmts(
        (my int $elems = @indices.elems),   # reifies
        (my $indices := nqp::getattr(@indices,List,'$!reified')),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
            && nqp::istype(nqp::atpos($indices,$i),Int),
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
                nqp::atpos($indices,0),
                nqp::atpos($indices,1)
              ),
              nqp::decont(SELF.AT-POS(
                nqp::atpos($indices,0),
                nqp::atpos($indices,1)
              )),
              ()
            ),
            nqp::if(
              nqp::iseq_i($elems,3),
              nqp::if(
                SELF.EXISTS-POS(
                  nqp::atpos($indices,0),
                  nqp::atpos($indices,1),
                  nqp::atpos($indices,2)
                ),
                nqp::decont(SELF.AT-POS(
                  nqp::atpos($indices,0),
                  nqp::atpos($indices,1),
                  nqp::atpos($indices,2)
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

# vim: ft=perl6 expandtab sw=4
