# all 6.e specific sub postcircumfix [;] candidates here please

proto sub postcircumfix:<[; ]>($, $, $?, *%) is nodal {*}

# This candidate must be provided because assignment to a multi-level
# hash is codegenned this way.
multi sub postcircumfix:<[; ]>(\SELF, @indices, Mu \assignee) is raw {
    my int $dims = @indices.elems;   # reifies
    my $indices := nqp::getattr(@indices,List,'$!reified');

    my int $i;
    nqp::while(
      nqp::islt_i($i,$dims) && nqp::istype(nqp::atpos($indices,$i),Int),
      ($i = nqp::add_i($i,1))
    );

    nqp::iseq_i($i,$dims)                   # True if all indices are Int
      ?? nqp::iseq_i($dims,2)
        ?? SELF.ASSIGN-POS(
             nqp::atpos($indices,0),
             nqp::atpos($indices,1),
             assignee
           )
        !! nqp::iseq_i($dims,3)
          ?? SELF.ASSIGN-POS(
               nqp::atpos($indices,0),
               nqp::atpos($indices,1),
               nqp::atpos($indices,2),
               assignee
             )
          !! SELF.ASSIGN-POS(|@indices, assignee)
      # need an extra named here to prevent infilooping because otherwise
      # this will code-gen to a call to this candidate again.
      !! (postcircumfix:<[; ]>(SELF, @indices, :none) = assignee)
}

# This candidate must be provided because binding to a multi-level
# hash is codegenned this way.
multi sub postcircumfix:<[; ]>(\SELF, @indices, :$BIND! is raw) is raw {
    my int $dims = @indices.elems;   # reifies
    my $indices := nqp::getattr(@indices,List,'$!reified');

    my int $i;
    nqp::while(
      nqp::islt_i($i,$dims) && nqp::istype(nqp::atpos($indices,$i),Int),
      ($i = nqp::add_i($i,1))
    );

    nqp::iseq_i($i,$dims)                   # True if all indices are Int
      ?? nqp::iseq_i($dims,2)
        ?? SELF.BIND-POS(
             nqp::atpos($indices,0),
             nqp::atpos($indices,1),
             $BIND
           )
        !! nqp::iseq_i($dims,3)
          ?? SELF.BIND-POS(
               nqp::atpos($indices,0),
               nqp::atpos($indices,1),
               nqp::atpos($indices,2),
               $BIND
             )
          !! SELF.BIND-POS(|@indices, $BIND)
      !! X::Bind::Slice.new(type => SELF.WHAT).throw
}

# This candidate provides all of the multi-level array access, as well
# as providing the slow-path for assignment of a multi-level array.
multi sub postcircumfix:<[; ]>(\initial-SELF, @indices, *%_) is raw {
    my int $topdim = @indices.elems;  # .elems reifies
    my $indices   := nqp::getattr(@indices,List,'$!reified');
    my $nameds    := nqp::getattr(%_,Map,'$!storage');

    my int $i;
    nqp::while(
      nqp::islt_i($i,$topdim) && nqp::istype(nqp::atpos($indices,$i),Int),
      $i = nqp::add_i($i,1)
    );
    my int $all-indices-are-Int = nqp::iseq_i($i,$topdim);

    # Map $topdim to the highest index number, so that it can be easier used
    # in recursion checks.
    --$topdim;

    # Delete "none" named argumement, as it is used to fool dispatch into
    # this candidate only and has no further meaning.
    nqp::deletekey($nameds,"none");

    if $nameds {

        # find out what we actually got
        my str $adverbs;
        $adverbs = (my $exists := nqp::atkey($nameds,'exists'))
          ?? ":exists"
          !! ":!exists"
          if nqp::existskey($nameds,'exists');

        $adverbs = nqp::concat($adverbs,":delete")
          if nqp::atkey($nameds,'delete');
        $adverbs = nqp::concat($adverbs,":k")  if nqp::atkey($nameds,'k');
        $adverbs = nqp::concat($adverbs,":kv") if nqp::atkey($nameds,'kv');
        $adverbs = nqp::concat($adverbs,":p")  if nqp::atkey($nameds,'p');
        $adverbs = nqp::concat($adverbs,":v")  if nqp::atkey($nameds,'v');

        # set up standard lexical info for recursing subs
        my \target = nqp::create(IterationBuffer);
        my int $dim;
        my int $return-list;

        my sub post-process-recursive-result() is raw {
            $return-list
              ?? target.List
              !! nqp::elems(target) ?? nqp::atpos(target,0) !! Nil
        }

        if nqp::iseq_s($adverbs,":exists") || nqp::iseq_s($adverbs,":!exists") {
            my sub EXISTS-POS-recursively(\SELF, \idx --> Nil) {
                if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list  = 1;
                    my $iterator := idx.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      EXISTS-POS-recursively(SELF, pulled)
                    );
                }
                elsif nqp::islt_i($dim,$topdim) {
                    ++$dim;  # going higher
                    if nqp::istype(idx,Whatever) {
                        $return-list  = 1;
                        my \next-idx := nqp::atpos($indices,$dim);
                        my int $i     = -1;
                        my int $elems = SELF.elems;
                        nqp::while(
                          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                          EXISTS-POS-recursively(SELF.AT-POS($i), next-idx)
                        );
                    }
                    elsif nqp::istype(idx,Callable) {
                        EXISTS-POS-recursively(
                          SELF.AT-POS((idx.(SELF.elems)).Int),
                          nqp::atpos($indices,$dim)
                        );
                    }
                    else  {
                        EXISTS-POS-recursively(
                          SELF.AT-POS(idx.Int), nqp::atpos($indices,$dim)
                        );
                    }
                    --$dim;  # done at this level
                }
                # $next-dim == $topdim, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list  = 1;
                    my int $i     = -1;
                    my int $elems = SELF.elems;
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      nqp::push(target,SELF.EXISTS-POS($i))
                    );
                }
                elsif nqp::istype(idx,Callable) {
                    nqp::push(
                      target,
                      SELF.EXISTS-POS((idx.(SELF.elems)).Int)
                    );
                }
                else {
                    nqp::push(target,SELF.EXISTS-POS(idx.Int));
                }
            }

            $all-indices-are-Int
              ?? nqp::iseq_i($topdim,1)
                ?? initial-SELF.EXISTS-POS(
                     nqp::atpos($indices,0),
                     nqp::atpos($indices,1)
                   ) == $exists
                !! nqp::iseq_i($topdim,2)
                  ?? initial-SELF.EXISTS-POS(
                       nqp::atpos($indices,0),
                       nqp::atpos($indices,1),
                       nqp::atpos($indices,2)
                     ) == $exists
                  !! initial-SELF.EXISTS-POS(|@indices) == $exists
              !! EXISTS-POS-recursively(initial-SELF,nqp::atpos($indices,0))
                  // post-process-recursive-result
        }

        elsif nqp::iseq_s($adverbs,":delete") {
            my sub DELETE-POS-recursively(\SELF, \idx --> Nil) {
                if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list  = 1;
                    my $iterator := idx.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      DELETE-POS-recursively(SELF, pulled)
                    );
                }
                elsif nqp::islt_i($dim,$topdim) {
                    ++$dim;  # going higher
                    if nqp::istype(idx,Whatever) {
                        $return-list  = 1;
                        my \next-idx := nqp::atpos($indices,$dim);
                        my int $i     = -1;
                        my int $elems = SELF.elems;
                        nqp::while(
                          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                          DELETE-POS-recursively(SELF.AT-POS($i), next-idx)
                        );
                    }
                    elsif nqp::istype(idx,Callable) {
                        DELETE-POS-recursively(
                          SELF.AT-POS((idx.(SELF.elems)).Int),
                          nqp::atpos($indices,$dim)
                        );
                    }
                    else  {
                        DELETE-POS-recursively(
                          SELF.AT-POS(idx.Int), nqp::atpos($indices,$dim)
                        );
                    }
                    --$dim;  # done at this level
                }
                # $next-dim == $topdim, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list  = 1;
                    my int $i     = -1;
                    my int $elems = SELF.elems;
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      nqp::push(target,SELF.DELETE-POS($i))
                    );
                }
                else {
                    my $index := nqp::istype(idx,Callable)
                      ?? (idx.(SELF.elems)).Int
                      !! idx.Int;
                    nqp::push(
                      target,
                      SELF.EXISTS-POS($index) ?? SELF.DELETE-POS($index) !! Nil
                    );
                }
            }

            $all-indices-are-Int
              ?? nqp::iseq_i($topdim,1)
                ?? initial-SELF.DELETE-POS(
                     nqp::atpos($indices,0),
                     nqp::atpos($indices,1)
                   )
                !! nqp::iseq_i($topdim,2)
                  ?? initial-SELF.DELETE-POS(
                       nqp::atpos($indices,0),
                       nqp::atpos($indices,1),
                       nqp::atpos($indices,2)
                     )
                  !! initial-SELF.DELETE-POS(|@indices)
              !! DELETE-POS-recursively(initial-SELF,nqp::atpos($indices,0))
                   // post-process-recursive-result
        }

        # some other combination of adverbs
        else {

            # helper sub to create multi-level keys
            my $keys := nqp::create(IterationBuffer);  # keys encountered
            sub keys-to-list($index) {
                nqp::push((my $list := nqp::clone($keys)),$index);
                $list.List
            }

            # determine the processor to be used
            my &process =
               nqp::iseq_s($adverbs,":exists:delete")
              ?? -> \SELF, \key {
                     SELF.DELETE-POS(key)
                       if nqp::push(target,SELF.EXISTS-POS(key));
                 }
            !! nqp::iseq_s($adverbs,":exists:delete:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key {
                         if SELF.EXISTS-POS(key) {
                             SELF.DELETE-POS(key);
                             nqp::push(target,keys-to-list(key));
                             nqp::push(target,True);
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":exists:delete:p")
              ?? -> \SELF, \key {
                     if SELF.EXISTS-POS(key) {
                         SELF.DELETE-POS(key);
                         nqp::push(
                           target,
                           Pair.new(keys-to-list(key), True)
                        );
                     }
                 }
            !! nqp::iseq_s($adverbs,":exists:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key {
                         if SELF.EXISTS-POS(key) {
                             nqp::push(target,keys-to-list(key));
                             nqp::push(target,True);
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":exists:p")
              ?? -> \SELF, \key {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(key), True)
                     ) if SELF.EXISTS-POS(key);
                 }
            !! nqp::iseq_s($adverbs,":delete:k")
              ?? -> \SELF, \key {
                     if SELF.EXISTS-POS(key) {
                         SELF.DELETE-POS(key);
                         nqp::push(target,keys-to-list(key));
                     }
                 }
            !! nqp::iseq_s($adverbs,":delete:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key {
                         if SELF.EXISTS-POS(key) {
                             nqp::push(target,keys-to-list(key));
                             nqp::push(target,SELF.DELETE-POS(key));
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":delete:p")
              ?? -> \SELF, \key {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(key), SELF.DELETE-POS(key))
                     ) if SELF.EXISTS-POS(key);
                 }
            !! nqp::iseq_s($adverbs,":delete:v")
              ?? -> \SELF, \key {
                     nqp::push(target,SELF.DELETE-POS(key))
                       if SELF.EXISTS-POS(key);
                 }
            !! nqp::iseq_s($adverbs,":k")
              ?? -> \SELF, \key {
                     nqp::push(target,keys-to-list(key))
                       if SELF.EXISTS-POS(key);
                 }
            !! nqp::iseq_s($adverbs,":kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key {
                         if SELF.EXISTS-POS(key) {
                             nqp::push(target,keys-to-list(key));
                             nqp::push(target,nqp::decont(SELF.AT-POS(key)));
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":p")
              ?? -> \SELF, \key {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(key), SELF.AT-POS(key))
                     ) if SELF.EXISTS-POS(key);
                 }
            !! nqp::iseq_s($adverbs,":v")
              ?? -> \SELF, \key {
                     nqp::push(target,nqp::decont(SELF.AT-POS(key)))
                       if SELF.EXISTS-POS(key);
                 }
            !! return Failure.new(X::Adverb.new(
                 :what<slice>,
                 :source(try { initial-SELF.VAR.name } // initial-SELF.^name),
                 :nogo(nqp::split(':',nqp::substr($adverbs,1)))
               ));

            my sub PROCESS-POS-recursively(\SELF, \idx --> Nil) {
                if nqp::istype(idx,Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list  = 1;
                    my $iterator := idx.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      PROCESS-POS-recursively(SELF, pulled)
                    );
                }
                elsif nqp::islt_i($dim,$topdim) {
                    ++$dim;  # going higher
                    if nqp::istype(idx,Whatever) {
                        $return-list  = 1;
                        my \next-idx := nqp::atpos($indices,$dim);
                        my int $i     = -1;
                        my int $elems = SELF.elems;
                        nqp::while(
                          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                          nqp::stmts(
                            nqp::push($keys,nqp::clone($i)),
                            PROCESS-POS-recursively(
                              SELF.AT-POS($i),
                              next-idx
                            ),
                            nqp::pop($keys)
                          )
                        );
                    }
                    elsif nqp::istype(idx,Callable) {
                        my $index := (idx.(SELF.elems)).Int;
                        nqp::push($keys,$index),
                        PROCESS-POS-recursively(
                          SELF.AT-POS($index),
                          nqp::atpos($indices,$dim)
                        );
                        nqp::pop($keys)
                    }
                    else  {
                        nqp::push($keys,idx.Int);
                        PROCESS-POS-recursively(
                          SELF.AT-POS(idx.Int), nqp::atpos($indices,$dim)
                        );
                        nqp::pop($keys);
                    }
                    --$dim;  # done at this level
                }
                # $next-dim == $topdim, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list  = 1;
                    my int $i     = -1;
                    my int $elems = SELF.elems;
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      process(SELF, $i)
                    );
                }
                else {
                    process(
                      SELF,
                      nqp::istype(idx,Callable) ?? (idx.(SELF.elems)) !! idx.Int
                    );
                }
            }

            PROCESS-POS-recursively(initial-SELF, nqp::atpos($indices,0));
            post-process-recursive-result
        }
    }

    # no adverbs whatsoever
    else {
        my sub AT-POS-slow() is raw {

            # set up standard lexical info for recursing subs
            my \target := nqp::create(IterationBuffer);
            my int $dim;
            my int $return-list;

            my sub AT-POS-recursively(\SELF, \idx --> Nil) {
                if nqp::istype(idx,Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list = 1;
                    my $iterator := idx.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      AT-POS-recursively(SELF, pulled)
                    );
                }
                elsif nqp::islt_i($dim,$topdim) {
                    ++$dim;  # going higher
                    if nqp::istype(idx,Whatever) {
                        $return-list  = 1;
                        my \next-idx := nqp::atpos($indices,$dim);
                        my int $i     = -1;
                        my int $elems = SELF.elems;
                        nqp::while(
                          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                          AT-POS-recursively(SELF.AT-POS($i), next-idx)
                        );
                    }
                    elsif nqp::istype(idx,Callable) {
                        AT-POS-recursively(
                          SELF.AT-POS((idx.(SELF.elems)).Int),
                          nqp::atpos($indices,$dim)
                        );
                    }
                    else  {
                        AT-POS-recursively(
                          SELF.AT-POS(idx.Int), nqp::atpos($indices,$dim)
                        );
                    }
                    --$dim;  # done at this level
                }
                # $next-dim == $topdim, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list  = 1;
                    my int $i     = -1;
                    my int $elems = SELF.elems;
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      nqp::push(target,SELF.AT-POS($i))
                    );
                }
                else {
                    nqp::push(
                      target,
                      SELF.AT-POS(nqp::istype(idx,Callable)
                        ?? (idx.(SELF.elems)).Int
                        !! idx.Int
                      )
                    );
                }
            }

            AT-POS-recursively(initial-SELF, nqp::atpos($indices,0));
            $return-list
              ?? target.List
              !! nqp::elems(target) ?? nqp::atpos(target,0) !! Nil
        }

        $all-indices-are-Int
          ?? nqp::iseq_i($topdim,1)
            ?? initial-SELF.AT-POS(
                 nqp::atpos($indices,0),
                 nqp::atpos($indices,1)
               )
            !! nqp::iseq_i($topdim,2)
              ?? initial-SELF.AT-POS(
                   nqp::atpos($indices,0),
                   nqp::atpos($indices,1),
                   nqp::atpos($indices,2)
                 )
              !! initial-SELF.AT-POS(|@indices)
          !! AT-POS-slow()
    }
}

# vim: expandtab shiftwidth=4
