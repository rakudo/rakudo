# all 6.e specific sub postcircumfix {;} candidates here please

proto sub postcircumfix:<{; }>($, $, *%) is nodal {*}

# handle the case of %h{|| "a"}
multi sub postcircumfix:<{; }>(\initial-SELF, \value, *%_) is raw {
    postcircumfix:<{; }>(initial-SELF, value.List, |%_)
}

multi sub postcircumfix:<{; }>(\initial-SELF, @indices,
  :$exists, :$delete, :$k, :$kv, :$p, :$v
) is raw {

    # find out what we actually got
    my str $adverbs;
    $adverbs = $exists ?? ":exists" !! ":!exists" if nqp::isconcrete($exists);
    $adverbs = nqp::concat($adverbs,":delete") if $delete;
    $adverbs = nqp::concat($adverbs,":k")      if $k;
    $adverbs = nqp::concat($adverbs,":kv")     if $kv;
    $adverbs = nqp::concat($adverbs,":p")      if $p;
    $adverbs = nqp::concat($adverbs,":v")      if $v;

    # set up standard lexical info for recursing subs
    my \target   = nqp::create(IterationBuffer);
    my int $dim;
    my int $dims = nqp::sub_i(@indices.elems,1);  # .elems reifies
    my $indices := nqp::getattr(@indices,List,'$!reified');
    my int $return-list;

    if $adverbs {
        if nqp::iseq_s($adverbs,":exists") || nqp::iseq_s($adverbs,":!exists") {
            sub EXISTS-KEY-recursively(\SELF, \idx --> Nil) {
                if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list = 1;
                    my $iterator := idx.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      EXISTS-KEY-recursively(SELF, pulled)
                    );
                }
                elsif $dim < $dims {
                    ++$dim;  # going higher
                    if nqp::istype(idx,Whatever) {
                        $return-list = 1;
                        my \next-idx := nqp::atpos($indices,$dim);
                        my $iterator := SELF.keys.iterator;
                        nqp::until(
                          nqp::eqaddr(
                            (my \pulled := $iterator.pull-one),
                            IterationEnd
                          ),
                          EXISTS-KEY-recursively(SELF.AT-KEY(pulled), next-idx)
                        );
                    }
                    else  {
                        EXISTS-KEY-recursively(
                          SELF.AT-KEY(idx), nqp::atpos($indices,$dim)
                        );
                    }
                    --$dim;  # done at this level
                }
                # $next-dim == $dims, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    my $iterator := SELF.keys.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      nqp::push(target,SELF.EXISTS-KEY(pulled))
                    );
                }
                else {
                    nqp::push(target,SELF.EXISTS-KEY(idx));
                }
            }

            EXISTS-KEY-recursively(initial-SELF, nqp::atpos($indices,0));

            # negate results if so requested
            unless $exists {
                my int $i     = -1;
                my int $elems = nqp::elems(target);
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos(target,$i,!nqp::atpos(target,$i))
                );
            }
        }

        elsif nqp::iseq_s($adverbs,":delete") {
            sub DELETE-KEY-recursively(\SELF, \idx --> Nil) {
                if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list = 1;
                    my $iterator := idx.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      DELETE-KEY-recursively(SELF, pulled)
                    );
                }
                elsif $dim < $dims {
                    ++$dim;  # going higher
                    if nqp::istype(idx,Whatever) {
                        $return-list = 1;
                        my \next-idx := nqp::atpos($indices,$dim);
                        my $iterator := SELF.keys.iterator;
                        nqp::until(
                          nqp::eqaddr(
                            (my \pulled := $iterator.pull-one),
                            IterationEnd
                          ),
                          DELETE-KEY-recursively(SELF.AT-KEY(pulled), next-idx)
                        );
                    }
                    else  {
                        DELETE-KEY-recursively(
                          SELF.AT-KEY(idx), nqp::atpos($indices,$dim)
                        );
                    }
                    --$dim;  # done at this level
                }
                # $next-dim == $dims, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    my $iterator := SELF.keys.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      nqp::push(target,SELF.DELETE-KEY(pulled))
                    );
                }
                else {
                    nqp::push(
                      target,
                      SELF.EXISTS-KEY(idx) ?? SELF.DELETE-KEY(idx) !! Nil
                    );
                }
            }

            DELETE-KEY-recursively(initial-SELF, nqp::atpos($indices,0));
        }

        # some other combination of adverbs
        else {

            # helper sub to create multi-level keys
            my $keys := nqp::create(IterationBuffer);  # keys encountered
            sub keys-to-list(\key) {
                nqp::push((my $list := nqp::clone($keys)),key);
                $list.List
            }

            # determine the processor to be used
            my &process =
               nqp::iseq_s($adverbs,":exists:delete")
              ?? -> \SELF, \key {
                     SELF.DELETE-KEY(key)
                       if nqp::push(target,SELF.EXISTS-KEY(key));
                 }
            !! nqp::iseq_s($adverbs,":exists:delete:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key {
                         if SELF.EXISTS-KEY(key) {
                             SELF.DELETE-KEY(key);
                             nqp::push(target,keys-to-list(key));
                             nqp::push(target,True);
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":exists:delete:p")
              ?? -> \SELF, \key {
                     if SELF.EXISTS-KEY(key) {
                         SELF.DELETE-KEY(key);
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
                         if SELF.EXISTS-KEY(key) {
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
                     ) if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":delete:k")
              ?? -> \SELF, \key {
                     if SELF.EXISTS-KEY(key) {
                         SELF.DELETE-KEY(key);
                         nqp::push(target,keys-to-list(key));
                     }
                 }
            !! nqp::iseq_s($adverbs,":delete:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key {
                         if SELF.EXISTS-KEY(key) {
                             nqp::push(target,keys-to-list(key));
                             nqp::push(target,SELF.DELETE-KEY(key));
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":delete:p")
              ?? -> \SELF, \key {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(key), SELF.DELETE-KEY(key))
                     ) if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":delete:v")
              ?? -> \SELF, \key {
                     nqp::push(target,SELF.DELETE-KEY(key))
                       if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":k")
              ?? -> \SELF, \key {
                     nqp::push(target,keys-to-list(key))
                       if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key {
                         if SELF.EXISTS-KEY(key) {
                             nqp::push(target,keys-to-list(key));
                             nqp::push(target,nqp::decont(SELF.AT-KEY(key)));
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":p")
              ?? -> \SELF, \key {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(key), SELF.AT-KEY(key))
                     ) if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":v")
              ?? -> \SELF, \key {
                     nqp::push(target,nqp::decont(SELF.AT-KEY(key)))
                       if SELF.EXISTS-KEY(key);
                 }
            !! return Failure.new(X::Adverb.new(
                 :what<slice>,
                 :source(try { initial-SELF.VAR.name } // initial-SELF.^name),
                 :nogo(nqp::split(':',nqp::substr($adverbs,1)))
               ));

            sub PROCESS-KEY-recursively(\SELF, \idx --> Nil) {
                if nqp::istype(idx,Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list = 1;
                    my $iterator := idx.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      PROCESS-KEY-recursively(SELF, pulled)
                    );
                }
                elsif $dim < $dims {
                    ++$dim;  # going higher
                    if nqp::istype(idx,Whatever) {
                        $return-list = 1;
                        my $iterator := SELF.keys.iterator;
                        my \next-idx := nqp::atpos($indices,$dim);
                        nqp::until(
                          nqp::eqaddr(
                            (my \pulled := $iterator.pull-one),
                            IterationEnd
                          ),
                          nqp::stmts(
                            nqp::push($keys,pulled),
                            PROCESS-KEY-recursively(
                              SELF.AT-KEY(pulled),
                              next-idx
                            ),
                            nqp::pop($keys)
                          )
                        );
                    }
                    else  {
                        nqp::push($keys,idx);
                        PROCESS-KEY-recursively(
                          SELF.AT-KEY(idx), nqp::atpos($indices,$dim)
                        );
                        nqp::pop($keys);
                    }
                    --$dim;  # done at this level
                }
                # $next-dim == $dims, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    my $iterator := SELF.keys.iterator;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      process(SELF, pulled)
                    );
                }
                else {
                    process(SELF, idx);
                }
            }

            PROCESS-KEY-recursively( initial-SELF, nqp::atpos($indices,0));
        }
    }

    # no adverbs whatsoever
    else {
        my int $non-deterministic;

        sub AT-KEY-recursively(\SELF, \idx --> Nil) {
            if nqp::istype(idx,Iterable) && nqp::not_i(nqp::iscont(idx)) {
                $return-list = 1;
                my $iterator := idx.iterator;
                nqp::until(
                  nqp::eqaddr((my \pulled := $iterator.pull-one),IterationEnd),
                  AT-KEY-recursively(SELF, pulled)
                );
            }
            elsif $dim < $dims {
                $dim++;  # going higher
                if nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    my \next-idx := nqp::atpos($indices,$dim);
                    my $iterator := SELF.keys.iterator;
                    $non-deterministic = 1 unless $iterator.is-deterministic;
                    nqp::until(
                      nqp::eqaddr(
                        (my \pulled := $iterator.pull-one),
                        IterationEnd
                      ),
                      AT-KEY-recursively(SELF.AT-KEY(pulled), next-idx)
                    );
                }
                else  {
                    AT-KEY-recursively(
                      SELF.AT-KEY(idx), nqp::atpos($indices,$dim)
                    );
                }
                --$dim;  # done at this level
            }
            # $next-dim == $dims, reached leaves
            elsif nqp::istype(idx,Whatever) {
                $return-list = 1;
                my $iterator := SELF.keys.iterator;
                $non-deterministic = 1 unless $iterator.is-deterministic;
                nqp::until(
                  nqp::eqaddr((my \pulled := $iterator.pull-one),IterationEnd),
                  nqp::push(target,SELF.AT-KEY(pulled))
                );
            }
            else {
                nqp::push(target,SELF.AT-KEY(idx));
            }
        }

        AT-KEY-recursively(initial-SELF, nqp::atpos($indices,0));

        # decont all elements if non-deterministic to disallow assignment
        if $non-deterministic {
            my int $i     = -1;
            my int $elems = nqp::elems(target);
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos(target,$i,nqp::decont(nqp::atpos(target,$i)))
            );
        }
    }

    $return-list
      ?? target.List
      !! nqp::elems(target) ?? nqp::atpos(target,0) !! Nil
}

# vim: expandtab shiftwidth=4
