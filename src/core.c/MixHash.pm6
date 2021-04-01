my class MixHash does Mixy {

    method ^parameterize(Mu \base, Mu \type) {
        Rakudo::Internals.PARAMETERIZE-KEYOF(base,type)
    }

#--- interface methods
    method total() { Rakudo::QuantHash.MIX-TOTAL($!elems) }
    method !total-positive() { Rakudo::QuantHash.MIX-TOTAL-POSITIVE($!elems) }

    multi method STORE(MixHash:D: Iterable:D \iterable --> MixHash:D) {
        (my \iterator := iterable.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('initialize')
          !! self.SET-SELF(
               Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
               )
             )
    }
    multi method STORE(MixHash:D: \objects, \values --> MixHash:D) {
        self.SET-SELF(
          Rakudo::QuantHash.ADD-OBJECTS-VALUES-TO-MIX(
            nqp::create(Rakudo::Internals::IterationSet),
            objects.iterator,
            values.iterator,
            self.keyof
          )
        )
    }
    multi method AT-KEY(MixHash:D: \k) is raw {
        my \type := self.keyof;
        Proxy.new(
          FETCH => {
              $!elems && nqp::existskey($!elems,(my \which := k.WHICH))
                ?? nqp::getattr(nqp::atkey($!elems,which),Pair,'$!value')
                !! 0
          },
          STORE => -> $, Real() $value {
              nqp::if(
                nqp::istype($value,Failure),   # https://github.com/Raku/old-issue-tracker/issues/5567
                $value.throw,
                nqp::if(
                  $!elems,
                  nqp::if(                      # allocated hash
                    nqp::existskey($!elems,(my $which := k.WHICH)),
                    nqp::if(                    # existing element
                      $value == 0,
                      nqp::stmts(
                        nqp::deletekey($!elems,$which),
                        0
                      ),
                      nqp::bindattr(
                        nqp::atkey($!elems,$which),
                        Pair,
                        '$!value',
                        nqp::decont($value)
                      ),
                    ),
                    nqp::unless(
                      $value == 0,
                      Rakudo::QuantHash.BIND-TO-TYPED-MIX(
                        $!elems, $which, k, nqp::decont($value), type
                      )
                    )
                  ),
                  nqp::unless(                  # no hash allocated yet
                    $value == 0,
                    Rakudo::QuantHash.BIND-TO-TYPED-MIX(
                      nqp::bindattr(self,::?CLASS,'$!elems',
                        nqp::create(Rakudo::Internals::IterationSet)
                      ),
                      k.WHICH, k, nqp::decont($value), type
                    )
                  )
                )
              )
          }
        )
    }

#--- object creation methods
    multi method new(MixHash:_:) { nqp::create(self) }

#--- coercion methods
    multi method Mix(MixHash:D: :$view) {  # :view is implementation-detail
        $!elems && nqp::elems($!elems)
          ?? nqp::p6bindattrinvres(
               nqp::create(Mix),Mix,'$!elems',
               $view ?? $!elems !! $!elems.clone
             )
          !! mix()
    }
    multi method MixHash(MixHash:D:) { self }

    multi method Setty(MixHash:U:) { SetHash      }
    multi method Setty(MixHash:D:) { self.SetHash }
    multi method Baggy(MixHash:U:) { BagHash      }
    multi method Baggy(MixHash:D:) { self.BagHash }
    multi method Mixy (MixHash:U:) { MixHash      }
    multi method Mixy (MixHash:D:) { self         }

    method clone() {
        $!elems && nqp::elems($!elems)
          ?? nqp::create(MixHash).SET-SELF(
               Rakudo::QuantHash.BAGGY-CLONE($!elems)
             )
          !! nqp::create(MixHash)
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

        # save for possible object recreation
        my $pair := nqp::atkey(elems,$key);

        Proxy.new(
          FETCH => {
              nqp::existskey(elems,$key)
                ?? nqp::getattr(nqp::atkey(elems,$key),Pair,'$!value')
                !! 0
          },
          STORE => -> $, Real() \value {
              nqp::if(
                # https://github.com/Raku/old-issue-tracker/issues/5567
                nqp::istype(value,Failure),
                value.throw,
                nqp::if(
                  nqp::existskey(elems,$key),
                  nqp::if(                    # existing element
                    value == 0,
                    nqp::stmts(               # goodbye!
                      nqp::deletekey(elems,$key),
                      0
                    ),
                    nqp::bindattr(            # value ok
                      nqp::atkey(elems,$key),
                      Pair,
                      '$!value',
                      nqp::decont(value)
                    )
                  ),
                  nqp::unless(                # where did it go?
                    value == 0,
                    nqp::bindattr(
                      nqp::bindkey(elems,$key,$pair),
                      Pair,
                      '$!value',
                      nqp::decont(value)
                    )
                  )
                )
              )
          }
        )
    }

    my class Iterate does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        method pull-one() is raw {
            nqp::elems($!keys)
              ?? nqp::p6bindattrinvres(
                   nqp::clone(
                     nqp::atkey($!elems,(my $key := nqp::shift_s($!keys)))
                   ),
                   Pair,
                   '$!value',
                   proxy($key,$!elems)
                 )
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my $elems := $!elems;
            my $keys  := $!keys;
            nqp::while(  # doesn't sink
              nqp::elems($keys),
              target.push(nqp::atkey($elems,nqp::shift_s($keys)))
            )
        }
    }
    multi method iterator(MixHash:D:) { Iterate.new(:$!elems) }  # also .pairs

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
                nqp::getattr(
                  nqp::atkey($!elems,($!on = nqp::shift_s($!keys))),Pair,'$!key'
                ),
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
                (my $pair := nqp::atkey($elems,nqp::shift_s($keys))),
                target.push(nqp::getattr($pair,Pair,'$!key')),
                target.push(nqp::getattr($pair,Pair,'$!value'))
              )
            )
        }
    }
    multi method kv(MixHash:D:) { Seq.new(KV.new(:$!elems)) }

    my class Values does Iterator {
        has $!elems is built(:bind);
        has $!keys  is built(:bind) is built(False) =
          Rakudo::Internals.IterationSet2keys($!elems);
        method pull-one() is raw {
            nqp::elems($!keys)
              ?? proxy(nqp::shift_s($!keys),$!elems)
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            my $elems := $!elems;
            my $keys  := $!keys;
            nqp::while(  # doesn't sink
              nqp::elems($keys),
              target.push(proxy(nqp::shift_s($keys),$elems))
            )
        }
    }
    multi method values(MixHash:D:) { Seq.new(Values.new(:$!elems)) }
}

# vim: expandtab shiftwidth=4
