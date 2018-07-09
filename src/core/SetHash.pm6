my class SetHash does Setty {

#--- selector methods

    multi method grab(SetHash:D:) {
        nqp::if(
          $!elems,
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
    multi method grab(SetHash:D: $count) {
        Seq.new(class :: does Rakudo::QuantHash::Pairs {
            method pull-one() is raw {
                nqp::if(
                  nqp::elems($!picked),
                  nqp::stmts(
                    (my $object := nqp::atkey(
                      $!elems,
                      (my $key := nqp::pop_s($!picked))
                    )),
                    nqp::deletekey($!elems,$key),
                    $object
                  ),
                  IterationEnd
                )
            }
        }.new($!elems, $count))
    }

    multi method grabpairs(SetHash:D:) {
        Pair.new(self.grab,True)
    }
    multi method grabpairs(SetHash:D: Callable:D $calculate) {
        self.grabpairs($calculate(self.elems))
    }
    multi method grabpairs(SetHash:D: Whatever) {
        self.grabpairs(Inf)
    }
    multi method grabpairs(SetHash:D: $count) {
        Seq.new(class :: does Rakudo::QuantHash::Pairs {
            method pull-one() is raw {
                nqp::if(
                  nqp::elems($!picked),
                  nqp::stmts(
                    (my $object := nqp::atkey(
                      $!elems,
                      (my $key := nqp::pop_s($!picked))
                    )),
                    nqp::deletekey($!elems,$key),
                    Pair.new($object,True)
                  ),
                  IterationEnd
                )
            }
        }.new($!elems, $count))
    }

#--- iterator methods

    sub proxy(Mu \iter,Mu \elems) is raw {
        # We are only sure that the key exists when the Proxy
        # is made, but we cannot be sure of its existence when
        # either the FETCH or STORE block is executed.  So we
        # still need to check for existence, and handle the case
        # where we need to (re-create) the key and value.  The
        # logic is therefore basically the same as in AT-KEY,
        # except for tests for allocated storage and .WHICH
        # processing.
        nqp::stmts(
          # save object for potential recreation
          (my $object := nqp::iterval(iter)),

          Proxy.new(
            FETCH => {
                nqp::p6bool(nqp::existskey(elems,nqp::iterkey_s(iter)))
            },
            STORE => -> $, $value {
                nqp::stmts(
                  nqp::if(
                    $value,
                    nqp::bindkey(elems,nqp::iterkey_s(iter),$object),
                    nqp::deletekey(elems,nqp::iterkey_s(iter))
                  ),
                  $value.Bool
                )
            }
          )
        )
    }

    method iterator(SetHash:D:) {
        class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
              nqp::if(
                $!iter,
                Pair.new(
                  nqp::iterval(nqp::shift($!iter)),
                  proxy($!iter,$!hash)
                ),
                IterationEnd
              )
            }
        }.new($!elems)
    }

    multi method kv(SetHash:D:) {
        Seq.new(class :: does Rakudo::QuantHash::Quanty-kv {
            method pull-one() is raw {
                nqp::if(
                  $!on,
                  nqp::stmts(
                    ($!on = 0),
                    proxy($!iter,$!elems)
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!on = 1),
                      nqp::iterval(nqp::shift($!iter))
                    ),
                    IterationEnd
                  )
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                    $target.push(nqp::iterval(nqp::shift($!iter))),
                    $target.push(True)
                  )
                )
            }
        }.new(self))
    }
    multi method values(SetHash:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() is rw {
              nqp::if(
                $!iter,
                proxy(nqp::shift($!iter),$!hash),
                IterationEnd
              )
            }
        }.new($!elems))
    }

#--- coercion methods
    multi method Set(SetHash:D: :$view) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(Set).SET-SELF(nqp::if($view,$!elems,nqp::clone($!elems))),
          nqp::create(Set)
        )
    }
    multi method SetHash(SetHash:D:) { self }
    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(self).SET-SELF(nqp::clone($!elems)),
          nqp::create(self)
        )
    }

#--- interface methods
    method STORE(*@pairs --> SetHash:D) {
        nqp::if(
          (my $iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<initialize>,:what(self.^name))),
          self.SET-SELF(
            Rakudo::QuantHash.ADD-PAIRS-TO-SET(
              nqp::create(Rakudo::Internals::IterationSet), $iterator
            )
          )
        )
    }

    multi method AT-KEY(SetHash:D: \k --> Bool:D) is raw {
        Proxy.new(
          FETCH => {
              nqp::p6bool($!elems && nqp::existskey($!elems,k.WHICH))
          },
          STORE => -> $, $value {
              nqp::stmts(
                nqp::if(
                  $value,
                  nqp::stmts(
                    nqp::unless(
                      $!elems,
# XXX for some reason, $!elems := nqp::create(...) doesn't work
# Type check failed in binding; expected NQPMu but got Rakudo::Internals::IterationSet
                      nqp::bindattr(self,::?CLASS,'$!elems',
                        nqp::create(Rakudo::Internals::IterationSet))
                    ),
                    nqp::bindkey($!elems,k.WHICH,nqp::decont(k))
                  ),
                  $!elems && nqp::deletekey($!elems,k.WHICH)
                ),
                $value.Bool
              )
          }
        )
    }

    multi method DELETE-KEY(SetHash:D: \k --> Bool:D) {
        nqp::p6bool(
          nqp::if(
            $!elems && nqp::existskey($!elems,(my $which := k.WHICH)),
            nqp::stmts(
              nqp::deletekey($!elems,$which),
              1
            )
          )
        )
    }
}

# vim: ft=perl6 expandtab sw=4
