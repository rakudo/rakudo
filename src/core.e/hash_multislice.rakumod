# all 6.e specific sub postcircumfix {; } candidates here please

proto sub postcircumfix:<{; }>($, $, Mu $?, *%) is nodal {*}

# handle the case of %h{|| "a"}, and of an adverb no other candidate takes
multi sub postcircumfix:<{; }>(\initial-SELF, \value, *%adverbs) is raw {
    if nqp::istype(value,List) {
        my @nogo = %adverbs<delete exists kv p k v>:delete:k;
        X::Adverb.new(
          :what<multi-dimensional slice>,
          :source((try initial-SELF.VAR.name) // initial-SELF.^name),
          :unexpected(%adverbs.keys),
          :@nogo,
        ).throw
    }
    else {
        postcircumfix:<{; }>(initial-SELF, value.List, |%adverbs)
    }
}
multi sub postcircumfix:<{; }>(\initial-SELF, \value, Mu \assignee) is raw {
    postcircumfix:<{; }>(initial-SELF, value.List, assignee)
}
multi sub postcircumfix:<{; }>(\SELF, @indices, Mu \assignee) is raw {
    my \target := postcircumfix:<{; }>(SELF, @indices);
    target = assignee
}

# A shaped hash gives the element itself for one key per dimension, as the
# subscript of one key does, and a list of elements for a slice. Any adverb
# goes to that subscript with each list of keys as one key.
multi sub postcircumfix:<{; }>(Hash::Shaped \SELF, @indices, Mu \assignee) is raw {
    # An empty list of keys, as %h{||()} gives, is the zen slice %h{}.
    # SHAPED-WILDCARD reifies them before they are counted.
    my \indices := SHAPED-WILDCARD(SELF, @indices, 0);
    nqp::isconcrete(my \reified := nqp::getattr(@indices,List,'$!reified'))
      && nqp::elems(reified)
      ?? nqp::isconcrete(my \key := MD-SINGLE-KEYS(indices))
        ?? SELF.ASSIGN-KEY(key, assignee)
        !! SHAPED-SLICE(SELF, indices,
             -> \keys { postcircumfix:<{ }>(SELF, keys, assignee) }, 'assign')
      !! (postcircumfix:<{ }>(SELF) = assignee)
}
multi sub postcircumfix:<{; }>(Hash::Shaped \SELF, @indices, Mu :$BIND! is raw) is raw {
    my \indices := SHAPED-WILDCARD(SELF, @indices, 0);
    X::Bind::ZenSlice.new(type => SELF.WHAT).throw
      unless nqp::isconcrete(my \reified := nqp::getattr(@indices,List,'$!reified'))
        && nqp::elems(reified);
    nqp::isconcrete(my \key := MD-SINGLE-KEYS(indices))
      ?? SELF.BIND-KEY(key, $BIND)
      !! SHAPED-SLICE(SELF, indices,
           -> \keys { postcircumfix:<{ }>(SELF, keys, :$BIND) }, 'bind')
}
multi sub postcircumfix:<{; }>(Hash::Shaped \SELF, @indices, *%adverbs) is raw {
    my \indices := SHAPED-WILDCARD(SELF, @indices, 1);
    my \key     := MD-SINGLE-KEYS(indices);
    nqp::isconcrete(my \reified := nqp::getattr(@indices,List,'$!reified'))
      && nqp::elems(reified)
      ?? nqp::elems(nqp::getattr(%adverbs,Map,'$!storage'))
        ?? nqp::isconcrete(key)
          ?? postcircumfix:<{ }>(SELF, key.item, |%adverbs)
          !! SHAPED-SLICE(SELF, indices,
               -> \keys { postcircumfix:<{ }>(SELF, keys, |%adverbs) }, '')
        !! nqp::isconcrete(key)
          ?? SELF.AT-KEY(key)
          !! SHAPED-ANY-KEY(indices)
            ?? SHAPED-SLICE(SELF, indices,
                 -> \keys { MD-VALUES(postcircumfix:<{ }>(SELF, keys)) }, '')
            !! SHAPED-SLICE(SELF, indices,
                 -> \keys { postcircumfix:<{ }>(SELF, keys) }, '')
      !! postcircumfix:<{ }>(SELF, |%adverbs)
}

# Binding through a multidimensional subscript of a hash binds the element
# of its innermost hash.
multi sub postcircumfix:<{; }>(\SELF, @indices, Mu :$BIND! is raw) is raw {
    nqp::isconcrete(my \keys := MD-SINGLE-KEYS(@indices))
      ?? @indices.elems
        ?? MD-HASH-BIND(SELF, keys, 0, $BIND)
        !! X::Bind::ZenSlice.new(type => SELF.WHAT).throw
      !! X::Bind::Slice.new(type => SELF.WHAT).throw
}

# An empty list of keys, as %h{||()} gives, is the zen slice %h{} that
# takes the whole hash with each adverb given
sub MD-HASH-ZEN(
  \SELF,
  $exists,
  $delete,
  $k,
  $kv,
  $p,
  $v
) is raw is implementation-detail {
    my %adverbs;
    %adverbs<exists> := $exists if nqp::isconcrete($exists);
    %adverbs<delete> := $delete if nqp::isconcrete($delete);
    %adverbs<k>      := $k      if nqp::isconcrete($k);
    %adverbs<kv>     := $kv     if nqp::isconcrete($kv);
    %adverbs<p>      := $p      if nqp::isconcrete($p);
    %adverbs<v>      := $v      if nqp::isconcrete($v);
    postcircumfix:<{ }>(SELF, |%adverbs)
}

multi sub postcircumfix:<{; }>(\initial-SELF, @indices,
  :$exists, :$delete, :$k, :$kv, :$p, :$v
) is raw {

    my int $dims = nqp::sub_i(@indices.elems,1);  # .elems reifies
    return-rw MD-HASH-ZEN(initial-SELF, $exists, $delete, $k, $kv, $p, $v)
      if nqp::islt_i($dims,0);

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
                  nqp::islt_i(++$i,$elems),
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
            !! return X::Adverb.new(
                 :what<slice>,
                 :source(try { initial-SELF.VAR.name } // initial-SELF.^name),
                 :nogo(nqp::split(':',nqp::substr($adverbs,1)))
               ).Failure;

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
              nqp::islt_i(++$i,$elems),
              nqp::bindpos(target,$i,nqp::decont(nqp::atpos(target,$i)))
            );
        }
    }

    $return-list
      ?? target.List
      !! nqp::elems(target) ?? nqp::atpos(target,0) !! Nil
}

# Can be REMOVED **AFTER** the Raku grammar has become the default grammar
BEGIN &postcircumfix:<{; }>.set_op_props;

# vim: expandtab shiftwidth=4
