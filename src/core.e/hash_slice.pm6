# all 6.e specific sub postcircumfix {} candidates here please

proto sub postcircumfix:<{; }>($, $, *%) is nodal {*}
multi sub postcircumfix:<{; }>(\initial-SELF, @indices,
  :$exists, :$delete, :$k, :$kv, :$p, :$v
) is raw {

    # find out what we actually got
    my str $adverbs;
    $adverbs =                      ":exists"  if $exists;
    $adverbs = nqp::concat($adverbs,":delete") if $delete;
    $adverbs = nqp::concat($adverbs,":k")      if $k;
    $adverbs = nqp::concat($adverbs,":kv")     if $kv;
    $adverbs = nqp::concat($adverbs,":p")      if $p;
    $adverbs = nqp::concat($adverbs,":v")      if $v;

    # set up standard lexical info for recursing subs
    my \target   = nqp::create(IterationBuffer);
    my int $dims = @indices.elems;  # reifies
    my $indices := nqp::getattr(@indices,List,'$!reified');
    my int $return-list;

    if $adverbs {
        if nqp::iseq_s($adverbs,":exists") {
            sub EXISTS-KEY-recursively(\SELF, \idx, int $dim --> Nil) {
                my int $next-dim = $dim + 1;
                if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list = 1;
                    EXISTS-KEY-recursively(SELF, $_, $dim) for idx;
                }
                elsif $next-dim < $dims {
                    if nqp::istype(idx,Whatever) {
                        $return-list = 1;
                        EXISTS-KEY-recursively(
                          SELF.AT-KEY($_),
                          nqp::atpos($indices,$next-dim),
                          $next-dim
                        ) for SELF.keys;  # NOTE: not reproducible!
                    }
                    else  {
                        EXISTS-KEY-recursively(
                          SELF.AT-KEY(idx), nqp::atpos($indices,$next-dim), $next-dim
                        );
                    }
                }
                # $next-dim == $dims, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    if $exists {
                        nqp::push(target,SELF.EXISTS-KEY($_)) for SELF.keys;
                    }
                    else {
                        nqp::push(target,!SELF.EXISTS-KEY($_)) for SELF.keys;
                    }
                }
                elsif $exists {
                    nqp::push(target,SELF.EXISTS-KEY(idx));
                }
                else {
                    nqp::push(target,!SELF.EXISTS-KEY(idx));
                }
            }

            EXISTS-KEY-recursively(initial-SELF, nqp::atpos($indices,0), 0);
        }

        elsif nqp::iseq_s($adverbs,":delete") {
            sub DELETE-KEY-recursively(\SELF, \idx, int $dim --> Nil) {
                my int $next-dim = $dim + 1;
                if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list = 1;
                    DELETE-KEY-recursively(SELF, $_, $dim) for idx;
                }
                elsif $next-dim < $dims {
                    if nqp::istype(idx,Whatever) {
                        $return-list = 1;
                        DELETE-KEY-recursively(
                          SELF.AT-KEY($_),
                          nqp::atpos($indices,$next-dim),
                          $next-dim
                        ) for SELF.keys;  # NOTE: not reproducible!
                    }
                    else  {
                        DELETE-KEY-recursively(
                          SELF.AT-KEY(idx),
                          nqp::atpos($indices,$next-dim),
                          $next-dim
                        );
                    }
                }
                # $next-dim == $dims, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    nqp::push(target,SELF.DELETE-KEY($_)) for SELF.keys;
                }
                else {
                    nqp::push(target,SELF.DELETE-KEY(idx));
                }
            }

            DELETE-KEY-recursively(initial-SELF, nqp::atpos($indices,0), 0);
        }

        # some other combination of adverbs
        else {

            # helper sub to create multi-level keys
            sub keys-to-list(@other, \key) {
                my $list := nqp::clone(nqp::getattr(@other,List,'$!reified'));
                nqp::push($list,key);
                $list.List
            }

            # determine the processor to be used
            my &process = nqp::iseq_s($adverbs,":exists:delete")
              ?? -> \SELF, \key, @ {
                     SELF.DELETE-KEY(key)
                       if nqp::push(target,SELF.EXISTS-KEY(key));
                 }
            !! nqp::iseq_s($adverbs,":exists:delete:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key, @other {
                         if SELF.EXISTS-KEY(key) {
                             SELF.DELETE-KEY(key);
                             nqp::push(target,keys-to-list(@other, key));
                             nqp::push(target,True);
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":exists:delete:p")
              ?? -> \SELF, \key, @other {
                     if SELF.EXISTS-KEY(key) {
                         SELF.DELETE-KEY(key);
                         nqp::push(
                           target,
                           Pair.new(keys-to-list(@other, key), True)
                        );
                     }
                 }
            !! nqp::iseq_s($adverbs,":exists:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key, @other {
                         if SELF.EXISTS-KEY(key) {
                             nqp::push(target,keys-to-list(@other, key));
                             nqp::push(target,True);
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":exists:p")
              ?? -> \SELF, \key, @other {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(@other, key), True)
                     ) if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":delete:k")
              ?? -> \SELF, \key, @other {
                     if SELF.EXISTS-KEY(key) {
                         SELF.DELETE-KEY(key);
                         nqp::push(target,keys-to-list(@other, key));
                     }
                 }
            !! nqp::iseq_s($adverbs,":delete:kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key, @other {
                         if SELF.EXISTS-KEY(key) {
                             nqp::push(target,keys-to-list(@other, key));
                             nqp::push(target,SELF.DELETE-KEY(key));
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":delete:p")
              ?? -> \SELF, \key, @other {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(@other, key),SELF.DELETE-KEY(key))
                     ) if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":delete:v")
              ?? -> \SELF, \key, @ {
                     nqp::push(target,SELF.DELETE-KEY(key))
                       if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":k")
              ?? -> \SELF, \key, @other {
                     nqp::push(target,keys-to-list(@other, key))
                       if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":kv")
              ?? do {
                     $return-list = 1;
                     -> \SELF, \key, @other {
                         if SELF.EXISTS-KEY(key) {
                             nqp::push(target,keys-to-list(@other, key));
                             nqp::push(target,nqp::decont(SELF.AT-KEY(key)));
                         }
                     }
                 }
            !! nqp::iseq_s($adverbs,":p")
              ?? -> \SELF, \key, @other {
                     nqp::push(
                       target,
                       Pair.new(keys-to-list(@other, key),SELF.AT-KEY(key))
                     ) if SELF.EXISTS-KEY(key);
                 }
            !! nqp::iseq_s($adverbs,":v")
              ?? -> \SELF, \key, @ {
                     nqp::push(target,nqp::decont(SELF.AT-KEY(key)))
                       if SELF.EXISTS-KEY(key);
                 }
            !! return Failure.new(X::Adverb.new(
                 :what<slice>,
                 :source(try { initial-SELF.VAR.name } // initial-SELF.^name),
                 :nogo(nqp::split(':',nqp::substr($adverbs,1)))
               ));

            sub PROCESS-KEY-recursively(\SELF, \idx, int $dim, @keys --> Nil) {
                my int $next-dim = $dim + 1;
                if nqp::istype(idx,Iterable) && nqp::not_i(nqp::iscont(idx)) {
                    $return-list = 1;
                    PROCESS-KEY-recursively(SELF, $_, $dim, @keys) for idx;
                }
                elsif $next-dim < $dims {
                    if nqp::istype(idx,Whatever) {
                        $return-list = 1;
                        for SELF.keys {  # NOTE: not reproducible!
                            nqp::push(nqp::getattr(@keys,List,'$!reified'),$_);
                            PROCESS-KEY-recursively(
                              SELF.AT-KEY($_),
                              nqp::atpos($indices,$next-dim),
                              $next-dim,
                              @keys
                            );
                            nqp::pop(nqp::getattr(@keys,List,'$!reified'));
                        }
                    }
                    else  {
                        nqp::push(nqp::getattr(@keys,List,'$!reified'),idx);
                        PROCESS-KEY-recursively(
                          SELF.AT-KEY(idx),
                          nqp::atpos($indices,$next-dim),
                          $next-dim,
                          @keys
                        );
                        nqp::pop(nqp::getattr(@keys,List,'$!reified'));
                    }
                }
                # $next-dim == $dims, reached leaves
                elsif nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    process(SELF,$_,@keys) for SELF.keys;
                }
                else {
                    process(SELF,idx,@keys);
                }
            }

            PROCESS-KEY-recursively(initial-SELF,nqp::atpos($indices,0),0,[]);
        }
    }

    # no adverbs whatsoever
    else {
        sub AT-KEY-recursively(\SELF, \idx, int $dim --> Nil) {
            my int $next-dim = $dim + 1;
            if nqp::istype(idx, Iterable) && nqp::not_i(nqp::iscont(idx)) {
                $return-list = 1;
                AT-KEY-recursively(SELF, $_, $dim) for idx;
            }
            elsif $next-dim < $dims {
                if nqp::istype(idx,Whatever) {
                    $return-list = 1;
                    AT-KEY-recursively(
                      SELF.AT-KEY($_), nqp::atpos($indices,$next-dim), $next-dim
                    ) for SELF.keys;  # NOTE: not reproducible
                }
                else  {
                    AT-KEY-recursively(
                      SELF.AT-KEY(idx),nqp::atpos($indices,$next-dim),$next-dim
                    );
                }
            }
            # $next-dim == $dims, reached leaves
            elsif nqp::istype(idx,Whatever) {
                $return-list = 1;
                nqp::push(target,SELF.AT-KEY($_)) for SELF.keys;
            }
            else {
                nqp::push(target,SELF.AT-KEY(idx));
            }
        }

        AT-KEY-recursively(initial-SELF, nqp::atpos($indices,0), 0);
    }

    $return-list
      ?? target.List
      !! nqp::elems(target) ?? nqp::atpos(target,0) !! Nil
}

# vim: expandtab shiftwidth=4
