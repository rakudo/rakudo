my class Junction { # declared in BOOTSTRAP
    # class Junction is Mu
    #     has Mu $!eigenstates;              # elements of Junction
    #     has str $!type;                # type of Junction
    # Both of these are also accessed directly inside optimizer when
    # optimizing param typechecks with where clauses

    method !SET-SELF(str $type,\values) {
        if nqp::iseq_s($type,"any")
          || nqp::iseq_s($type,"all")
          || nqp::iseq_s($type,"none")
          || nqp::iseq_s($type,"one") {
            my \iterator := values.iterator;
            my \buffer   := nqp::create(IterationBuffer);
            nqp::until(
              nqp::eqaddr((my \pulled := iterator.pull-one),IterationEnd),
              nqp::push(buffer,nqp::decont(pulled))
            );
            $!eigenstates := buffer;
            $!type         = $type;
            self
        }
        else {
            Failure.new("Junction can only have 'any', 'all', 'none', 'one' type")
        }
    }

    # Swap 2 Junctions in place if they need to be for an infix operation
    # on the two Junctions.  Returns a truthy (0|1)value if the Junctions
    # were of the same type and can be merged.
    method INFIX-TWO(Junction:U:
      Junction:D \a, Junction:D \b
    ) is implementation-detail {
        nqp::if(
          nqp::iseq_s(
            (my \atype := nqp::getattr(nqp::decont(a),Junction,'$!type')),
            (my \btype := nqp::getattr(nqp::decont(b),Junction,'$!type'))
          ),
          nqp::isne_s(atype,"one"),              # same
          nqp::if(                               # not same
            (nqp::iseq_s(btype,"all") || nqp::iseq_s(btype,"none"))
              && (nqp::iseq_s(atype,"any") || nqp::iseq_s(atype,"one")),
            nqp::stmts(                          # need to be swapped
              nqp::bindattr(
                (my \ajunc := nqp::clone(nqp::decont(b))),
                Junction,
                '$!eigenstates',
                nqp::getattr(nqp::decont(a),Junction,'$!eigenstates')
              ),
              nqp::bindattr(
                (my \bjunc := nqp::clone(nqp::decont(a))),
                Junction,
                '$!eigenstates',
                nqp::getattr(nqp::decont(b),Junction,'$!eigenstates')
              ),
              (a = ajunc),
              (b = bjunc),
              0                                  # not same, now swapped
            )
          )
        )
    }

    proto method new(|) {*}
    multi method new(Junction: \values, Str :$type!) {
        nqp::create(Junction)!SET-SELF($type,values)
    }
    multi method new(Junction: Str:D \type, \values) {
        nqp::create(Junction)!SET-SELF(type,values)
    }

    method !defined-any() {
        my \eigenstates := $!eigenstates;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(++$i,nqp::elems(eigenstates))
            && nqp::isfalse(nqp::atpos(eigenstates,$i).defined),
          nqp::null
        );
        nqp::hllbool(nqp::islt_i($i,nqp::elems(eigenstates)))
    }
    method !defined-all() {
        my \eigenstates := $!eigenstates;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(++$i,nqp::elems(eigenstates))
            && nqp::atpos(eigenstates,$i).defined,
          nqp::null
        );
        nqp::hllbool(nqp::iseq_i($i,nqp::elems(eigenstates)))
    }
    method !defined-none() {
        my \eigenstates := $!eigenstates;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(++$i,nqp::elems(eigenstates))
            && nqp::isfalse(nqp::atpos(eigenstates,$i).defined),
          nqp::null
        );
        nqp::hllbool(nqp::iseq_i($i,nqp::elems(eigenstates)))
    }
    method !defined-one() {
        my \eigenstates := $!eigenstates;
        my int $i = -1;
        my int $seen;
        nqp::while(
          nqp::islt_i(++$i,nqp::elems(eigenstates))
            && nqp::isfalse(nqp::atpos(eigenstates,$i).defined)
            && nqp::not_i($seen++),
          nqp::null
        );
        nqp::hllbool(nqp::iseq_i($seen,1))
    }
    multi method defined(Junction:D: --> Bool:D) {
        nqp::iseq_s($!type,'any')
          ?? self!defined-any
          !! nqp::iseq_s($!type,'all')
            ?? self!defined-all
            !! nqp::iseq_s($!type,'none')
              ?? self!defined-none
              !! self!defined-one  # nqp::iseq_s($!type,'one')
    }

    multi method Bool(Junction:D:) {
        nqp::hllbool(
          nqp::stmts(
            (my int $elems = nqp::elems($!eigenstates)),
            (my int $i),
            nqp::if(
              nqp::iseq_s($!type,'any'),
              nqp::stmts(
                nqp::while(
                  nqp::islt_i($i,$elems)
                    && nqp::isfalse(nqp::atpos($!eigenstates,$i)),
                  ($i = nqp::add_i($i,1))
                ),
                nqp::islt_i($i,$elems)
              ),
              nqp::if(
                nqp::iseq_s($!type,'all'),
                nqp::stmts(
                  nqp::while(
                    nqp::islt_i($i,$elems)
                      && nqp::atpos($!eigenstates,$i),
                    ($i = nqp::add_i($i,1))
                  ),
                  nqp::iseq_i($i,$elems)
                ),
                nqp::if(
                  nqp::iseq_s($!type,'none'),
                  nqp::stmts(
                    nqp::while(
                      nqp::islt_i($i,$elems)
                        && nqp::isfalse(nqp::atpos($!eigenstates,$i)),
                      ($i = nqp::add_i($i,1))
                    ),
                    nqp::iseq_i($i,$elems)
                  ),
                  nqp::stmts(    # $!type eq 'one'
                    (my int $seen = 0),
                    ($i = nqp::sub_i($i,1)),  # increment in condition
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                        && nqp::isle_i($seen,1),
                      nqp::if(
                        nqp::atpos($!eigenstates,$i),
                        ($seen = nqp::add_i($seen,1))
                      )
                    ),
                    nqp::iseq_i($seen,1)
                  )
                )
              )
            )
          )
        )
    }

    multi method ACCEPTS(Junction:U: Junction:D --> True) { }
    multi method ACCEPTS(Junction:D: Mu \topic) {
        nqp::hllbool(
          nqp::stmts(
            (my int $elems = nqp::elems($!eigenstates)),
            (my int $i),
            nqp::if(
              nqp::iseq_s($!type,'any'),
              nqp::stmts(
                nqp::while(
                  nqp::islt_i($i,$elems)
                    && nqp::isfalse(nqp::atpos($!eigenstates,$i).ACCEPTS(topic)),
                  ($i = nqp::add_i($i,1))
                ),
                nqp::islt_i($i,$elems)
              ),
              nqp::if(
                nqp::iseq_s($!type,'all'),
                nqp::stmts(
                  nqp::while(
                    nqp::islt_i($i,$elems)
                      && nqp::atpos($!eigenstates,$i).ACCEPTS(topic),
                    ($i = nqp::add_i($i,1))
                  ),
                  nqp::iseq_i($i,$elems)
                ),
                nqp::if(
                  nqp::iseq_s($!type,'none'),
                  nqp::stmts(
                    nqp::while(
                      nqp::islt_i($i,$elems)
                        && nqp::isfalse(
                             nqp::atpos($!eigenstates,$i).ACCEPTS(topic)
                           ),
                      ($i = nqp::add_i($i,1))
                    ),
                    nqp::iseq_i($i,$elems)
                  ),
                  nqp::stmts(    # $!type eq 'one'
                    (my int $seen),
                    ($i = nqp::sub_i($i,1)),  # increment in condition
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                        && nqp::isle_i($seen,1),
                      nqp::if(
                        nqp::atpos($!eigenstates,$i).ACCEPTS(topic),
                        ($seen = nqp::add_i($seen,1))
                      )
                    ),
                    nqp::iseq_i($seen,1)
                  )
                )
              )
            )
          )
        )
    }

    multi method Str(Junction:D:) {
        my \storage := nqp::bindattr(
          (my \junction := nqp::clone(self)),
          Junction,
          '$!eigenstates',
          nqp::clone(nqp::getattr(self,Junction,'$!eigenstates'))
        );
        my int $elems = nqp::elems(storage);
        my int $i = -1;

        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::unless(
            nqp::istype(nqp::atpos(storage,$i),Str),
            nqp::bindpos(storage,$i,nqp::atpos(storage,$i).Str)
          )
        );

        junction
    }

    multi method iterator(Junction:D:) {
        # If we're asked for an iterator, we should really give one rather than
        # auto-thread over the `iterator` method. Otherwise we get decidedly
        # confusing outcomes from things that do `.iterator` and then expect it
        # to follow the iterator API.
        list(self).iterator
    }

    multi method gist(Junction:D:) {
        my int $elems = nqp::elems($!eigenstates);
        my int $i     = -1;
        my $gists    := nqp::setelems(nqp::list_s,$elems);
        nqp::bindpos_s($gists,$i,nqp::atpos($!eigenstates,$i).gist)
          while nqp::islt_i(++$i,$elems);
        $!type ~ '(' ~ nqp::join(', ',$gists) ~ ')'
    }

    multi method raku(Junction:D:) {
        my int $elems = nqp::elems($!eigenstates);
        my int $i     = -1;
        my $rakus    := nqp::setelems(nqp::list_s,$elems);
        nqp::bindpos_s($rakus,$i,nqp::atpos($!eigenstates,$i).raku)
          while nqp::islt_i(++$i,$elems);
        $!type ~ '(' ~ nqp::join(', ',$rakus) ~ ')'
    }

    method CALL-ME(|c) {
        self.AUTOTHREAD(
            -> $obj, |c { $obj(|c) },
            self, |c);
    }

    method sink(Junction:D: --> Nil) {
        my int $elems = nqp::elems($!eigenstates);
        my int $i     = -1;
        nqp::atpos($!eigenstates,$i).sink while nqp::islt_i(++$i,$elems);
    }

    # Helper method for handling those cases where auto-threading doesn't cut it.
    # Call the given Callable with each of the Junction values, and return a
    # Junction with the results of the calls.
    method THREAD(&call) is implementation-detail {
        my \storage := nqp::getattr(self,Junction,'$!eigenstates');
        my int $i = -1;
        my int $elems = nqp::elems(storage);
        my \result := nqp::setelems(nqp::list,$elems);
        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::bindpos(result,$i,call(nqp::atpos(storage,$i)))
        );
        nqp::p6bindattrinvres(nqp::clone(self),Junction,'$!eigenstates',result)
    }

    method AUTOTHREAD(&call, |args) is implementation-detail {
        my \positionals := nqp::getattr(nqp::decont(args),Capture,'@!list');

        sub thread_junction(int $pos) {
            my \junction := nqp::decont(nqp::atpos(positionals, $pos));
            my \storage := nqp::getattr(junction,Junction,'$!eigenstates');
            my int $elems = nqp::elems(storage);
            my \result   := nqp::setelems(nqp::list,$elems);
            my int $i     = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              # Next line is Officially Naughty, since captures are
              # meant to be immutable. But hey, it's our capture to
              # be naughty with...
              nqp::stmts(
                nqp::bindpos(positionals,$pos,nqp::atpos(storage,$i)),
                nqp::bindpos(result,$i,call(|args))
              )
            );
            nqp::p6bindattrinvres(
              nqp::clone(junction),Junction,'$!eigenstates',result)
        }

        # Look for a junctional arg in the positionals.
        # we have to autothread the first all or none junction before
        # doing any one or any junctions.
        my int $first_any_one = -1;
        my int $elems = nqp::elems(positionals);
        my int $i     = -1;
        while nqp::islt_i(++$i,$elems) {
            # Junctional positional argument?
            my Mu $arg := nqp::atpos(positionals, $i);
            if nqp::istype($arg,Junction) {
                my str $type = nqp::getattr_s(nqp::decont($arg),Junction,'$!type');
                nqp::iseq_s($type,'any') || nqp::iseq_s($type,'one')
                  ?? $first_any_one == -1
                    ?? ($first_any_one = $i)
                    !! Nil
                  !! return thread_junction($i);
            }
        }
        return thread_junction($first_any_one) if $first_any_one >= 0;

        # Otherwise, look for one in the nameds.
        my \nameds := nqp::getattr(nqp::decont(args), Capture, '%!hash');
        my \iter := nqp::iterator(nameds);
        while iter {
            if nqp::istype(nqp::iterval(nqp::shift(iter)),Junction) {
                my \junction := nqp::decont(nqp::iterval(iter));
                my \storage  := nqp::getattr(junction,Junction,'$!eigenstates');
                my int $elems = nqp::elems(storage);
                my \result   := nqp::setelems(nqp::list,$elems);
                my int $i     = -1;

                while nqp::islt_i(++$i,$elems) {
                    # also naughty, like above
                    nqp::bindkey(nameds,
                      nqp::iterkey_s(iter),nqp::atpos(storage,$i));
                    nqp::bindpos(result,$i,call(|args));
                }

                my \threaded := nqp::clone(nqp::decont(junction));
                nqp::bindattr(threaded,Junction,'$!eigenstates',result);
                return threaded;
            }
        }

        # If we get here, wasn't actually anything to autothread.
        call(|args);
    }
}

proto sub any(|) is pure {*}
#multi sub any(@values) { @values.any }  # this breaks S02-literals/radix.t
multi sub any(+values) {  values.any }

proto sub all(|) is pure {*}
multi sub all(@values) { @values.all }
multi sub all(+values) {  values.all }

proto sub one(|) is pure {*}
multi sub one(@values) { @values.one }
multi sub one(+values) {  values.one }

proto sub none(|) is pure {*}
multi sub none(@values) { @values.none }
multi sub none(+values) {  values.none }

proto sub infix:<|>(|) is pure {*}
multi sub infix:<|>(+values) { values.any }

proto sub infix:<&>(|) is pure {*}
multi sub infix:<&>(+values) { values.all }

proto sub infix:<^>(|) is pure {*}
multi sub infix:<^>(+values) is pure { values.one }

multi sub infix:<~>(Str:D $a, Junction:D $b) {
    nqp::if(
      $a,
      nqp::stmts(                                # something to concat with
        (my \storage := nqp::bindattr(
          (my \junction := nqp::clone($b)),
          Junction,
          '$!eigenstates',
          nqp::clone(nqp::getattr($b,Junction,'$!eigenstates'))
        )),
        (my int $elems = nqp::elems(storage)),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos(storage,$i,
            nqp::if(
              nqp::istype((my \value := nqp::atpos(storage,$i)),Junction),
              infix:<~>($a,value),
              nqp::concat($a,nqp::if(nqp::istype(value,Str),value,value.Str))
            )
          )
        ),
        junction
      ),
      $b.Str                                     # nothing to concat with
    )
}

multi sub infix:<~>(Junction:D $a, Str:D $b) {
    nqp::if(
      $b,
      nqp::stmts(                                # something to concat with
        (my \storage := nqp::bindattr(
          (my \junction := nqp::clone($a)),
          Junction,
          '$!eigenstates',
          nqp::clone(nqp::getattr($a,Junction,'$!eigenstates'))
        )),
        (my int $elems = nqp::elems(storage)),
        (my int $i = -1),
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos(storage,$i,
            nqp::if(
              nqp::istype((my \value := nqp::atpos(storage,$i)),Junction),
              infix:<~>(value,$b),
              nqp::concat(nqp::if(nqp::istype(value,Str),value,value.Str),$b)
            )
          )
        ),
        junction
      ),
      $a.Str                                     # nothing to concat with
    )
}

multi sub infix:<~>(Junction:D \a, Junction:D \b) {
    my int $mergable = Junction.INFIX-TWO(my $a = a, my $b = b);
    my \astor := nqp::getattr(nqp::decont($a),Junction,'$!eigenstates');
    my \bstor := nqp::getattr(nqp::decont($b),Junction,'$!eigenstates');
    my int $aelems = nqp::elems(astor);
    my int $belems = nqp::elems(bstor);
    my int $i = -1;
    my \seen := nqp::hash;
    my \outer := nqp::bindattr(               # outer eigenstates
      (my \junction := nqp::clone(nqp::decont($a))),
      Junction,
      '$!eigenstates',
      nqp::if(
        $mergable,
        nqp::list,
        nqp::setelems(nqp::list,$aelems)
      )
    );

    nqp::while(                                # outer loop
      nqp::islt_i(($i = nqp::add_i($i,1)),$aelems),
      nqp::stmts(
        (my \aval := nqp::if(
          nqp::istype(nqp::atpos(astor,$i),Str),
          nqp::atpos(astor,$i),
          nqp::atpos(astor,$i).Str
        )),
        (my int $j = -1),
        nqp::if(
          $mergable,
          nqp::while(                          # merge eigenstates
            nqp::islt_i(($j = nqp::add_i($j,1)),$belems),
            nqp::unless(
              nqp::existskey(
                seen,
                (my \concat := nqp::concat(
                  aval,
                  nqp::if(
                    nqp::istype(nqp::atpos(bstor,$j),Str),
                    nqp::atpos(bstor,$j),
                    nqp::atpos(bstor,$j).Str,
                  )
                ))
              ),
              nqp::bindkey(                    # new one, remember
                seen,nqp::push(outer,concat),1)
            )
          ),
          nqp::stmts(                          # cannot merge eigenstates
            (my \inner := nqp::bindattr(
              nqp::bindpos(outer,$i,nqp::clone(nqp::decont($b))),
              Junction,
              '$!eigenstates',
              nqp::setelems(nqp::list,$belems)
            )),
            nqp::while(
              nqp::islt_i(($j = nqp::add_i($j,1)),$belems),
              nqp::bindpos(
                inner,
                $j,
                nqp::concat(
                  aval,
                  nqp::if(
                    nqp::istype(nqp::atpos(bstor,$j),Str),
                    nqp::atpos(bstor,$j),
                    nqp::atpos(bstor,$j).Str,
                  )
                )
              )
            )
          )
        )
      )
    );
    junction
}

nqp::p6setautothreader( -> |c {
    Junction.AUTOTHREAD(|c)
} );
Mu.HOW.setup_junction_fallback(Junction, -> $name, |c {
    Junction.AUTOTHREAD(
        -> \obj, |c { obj."$name"(|c) },
        |c);
} );

# vim: expandtab shiftwidth=4
