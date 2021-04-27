my class SetHash does Setty {

    method ^parameterize(Mu \base, Mu \type) {
        Rakudo::Internals.PARAMETERIZE-KEYOF(base,type)
    }

#--- selector methods

    multi method grab(SetHash:D:) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my $object := nqp::iterval(
              my $iter := Rakudo::QuantHash.ROLL($!elems)
            )),
            nqp::deletekey($!elems,nqp::iterkey_s($iter)),
            $object
          ),
          Nil
        )
    }
    multi method grab(SetHash:D: Callable:D $calculate) {
        self.grab($calculate(self.elems))
    }
    multi method grab(SetHash:D: Whatever) {
        self.grab(Inf)
    }

    my class Grab does Rakudo::QuantHash::Pairs {
        method pull-one() is raw {
            nqp::if(
              nqp::elems($!picked),
              nqp::stmts(
                (my \object := nqp::atkey(
                  $!elems,
                  (my \key := nqp::pop_s($!picked))
                )),
                nqp::deletekey($!elems,key),
                object
              ),
              IterationEnd
            )
        }
    }
    multi method grab(SetHash:D: $count) { Seq.new(Grab.new($!elems,$count)) }

    multi method grabpairs(SetHash:D:) {
        Pair.new(self.grab,True)
    }
    multi method grabpairs(SetHash:D: Callable:D $calculate) {
        self.grabpairs($calculate(self.elems))
    }
    multi method grabpairs(SetHash:D: Whatever) {
        self.grabpairs(Inf)
    }

    my class GrabPairs does Rakudo::QuantHash::Pairs {
        method pull-one() is raw {
            nqp::if(
              nqp::elems($!picked),
              nqp::stmts(
                (my \object := nqp::atkey(
                  $!elems,
                  (my \key := nqp::pop_s($!picked))
                )),
                nqp::deletekey($!elems,key),
                Pair.new(object,True)
              ),
              IterationEnd
            )
        }
    }
    multi method grabpairs(SetHash:D: $count) {
        Seq.new(GrabPairs.new($!elems,$count))
    }

#--- iterator methods

    sub proxy(str $key, Mu \elems) is raw {
        # We are only sure that the key exists when the Proxy
        # is made, but we cannot be sure of its existence when
        # either the FETCH or STORE block is executed.  So we
        # still need to check for existence, and handle the case
        # where we need to (re-create) the key and value.  The
        # logic is therefore basically the same as in AT-KEY,
        # except for tests for allocated storage and .WHICH
        # processing.

        # save object for potential recreation
        my $object := nqp::atkey(elems,$key);

        Proxy.new(
          FETCH => {
              nqp::hllbool(nqp::existskey(elems,$key))
          },
          STORE => -> $, $value {
              $value
                ?? nqp::bindkey(elems,$key,$object)
                !! nqp::deletekey(elems,$key);
              $value.Bool
          }
        )
    }

    my class Iterate does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        method pull-one() {
            nqp::elems($!keys)
              ?? Pair.new(
                   nqp::atkey($!elems,(my $key := nqp::shift_s($!keys))),
                   proxy($key,$!elems)
                 )
              !! IterationEnd
        }
    }
    method iterator(SetHash:D:) { Iterate.new(:$!elems) }

    my class KV does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        has str $!on;
        method pull-one() is raw {
            nqp::if(
              $!on,
              nqp::stmts(
                (my $proxy := proxy($!on,$!elems)),
                ($!on = ""),
                $proxy
              ),
              nqp::if(
                nqp::elems($!keys),
                nqp::atkey($!elems,$!on = nqp::shift_s($!keys)),
                IterationEnd
              )
            )
        }
        method push-all(\target --> IterationEnd) {
            my $elems := $!elems;
            my $keys  := $!keys;
            nqp::while(
              nqp::elems($keys),
              nqp::stmts(  # doesn't sink
                target.push(nqp::atkey($elems,nqp::shift_s($keys))),
                target.push(True)
              )
            );
        }
    }
    multi method kv(SetHash:D:) { Seq.new(KV.new(:$!elems)) }

    my class Values does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        method pull-one() is rw {
            nqp::elems($!keys)
              ?? proxy(nqp::shift_s($!keys),$!elems)
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my $elems := $!elems;
            my $keys  := $!keys;
            nqp::while(
              nqp::elems($keys),
              target.push(proxy(nqp::shift_s($keys),$elems))
            );
        }
    }
    multi method values(SetHash:D:) { Seq.new(Values.new(:$!elems)) }

#--- coercion methods
    multi method Set(SetHash:D: :$view) {  # :view is implementation-detail
        $!elems && nqp::elems($!elems)
          ?? nqp::create(Set).SET-SELF($view ?? $!elems !! nqp::clone($!elems))
          !! nqp::create(Set)
    }
    multi method SetHash(SetHash:D:) { self }
    method clone() {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(self).SET-SELF(nqp::clone($!elems))
          !! nqp::create(self)
    }

    multi method Setty(SetHash:U:) { SetHash }
    multi method Setty(SetHash:D:) { self }
    multi method Baggy(SetHash:U:) { BagHash }
    multi method Baggy(SetHash:D:) { self.BagHash }
    multi method Mixy (SetHash:U:) { MixHash }
    multi method Mixy (SetHash:D:) { self.MixHash }

#--- interface methods
    multi method STORE(SetHash:D: Iterable:D \iterable --> SetHash:D) {
        (my \iterator := iterable.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('initialize')
          !! self.SET-SELF(
               Rakudo::QuantHash.ADD-PAIRS-TO-SET(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
               )
             )
    }
    multi method STORE(SetHash:D: \objects, \bools --> SetHash:D) {
        my \iterobjs  := objects.iterator;
        my \iterbools := bools.iterator;
        nqp::bindattr(
          self,SetHash,'$!elems',nqp::create(Rakudo::Internals::IterationSet)
        );
        nqp::until(
          nqp::eqaddr((my \object := iterobjs.pull-one),IterationEnd),
          nqp::if(
            iterbools.pull-one,
            nqp::bindkey($!elems,object.WHICH,nqp::decont(object))
          )
        );
        self
    }

    multi method AT-KEY(SetHash:D: \k --> Bool:D) is raw {
        Proxy.new(
          FETCH => {
              nqp::hllbool($!elems ?? nqp::existskey($!elems,k.WHICH) !! 0)
          },
          STORE => -> $, $value {
              nqp::stmts(
                nqp::if(
                  $value,
                  nqp::stmts(
                    nqp::unless(
                      $!elems,
                      nqp::bindattr(self,::?CLASS,'$!elems',
                        nqp::create(Rakudo::Internals::IterationSet))
                    ),
                    Rakudo::QuantHash.BIND-TO-TYPED-SET(
                      $!elems, nqp::decont(k), self.keyof
                    )
                  ),
                  $!elems && nqp::deletekey($!elems,k.WHICH)
                ),
                $value.Bool
              )
          }
        )
    }

    multi method DELETE-KEY(SetHash:D: \k --> Bool:D) {
        nqp::hllbool(
          nqp::if(
            $!elems && nqp::existskey($!elems,(my $which := k.WHICH)),
            nqp::stmts(
              nqp::deletekey($!elems,$which),
              1
            ),
            0
          )
        )
    }

#--- convenience methods
    method set(SetHash:D: \to-set --> Nil) {
        nqp::bindattr(
          self,SetHash,'$!elems',nqp::create(Rakudo::Internals::IterationSet)
        ) unless $!elems;
        Rakudo::QuantHash.ADD-ITERATOR-TO-SET(
          $!elems, to-set.iterator, self.keyof
        );
    }

    method unset(SetHash:D: \to-unset --> Nil) {
        my \iterator := to-unset.iterator;
        nqp::until(
          nqp::eqaddr((my \pulled := iterator.pull-one),IterationEnd),
          nqp::deletekey($!elems,pulled.WHICH)
        ) if $!elems
    }
}

# vim: expandtab shiftwidth=4
