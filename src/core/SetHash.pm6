my class SetHash does Setty {

    method ^parameterize(Mu \base, Mu \type) {
        my \what := base.^mixin(Rakudo::Internals::Constraint[type]);
        what.^set_name(base.^name ~ '[' ~ type.^name ~ ']');
        what
    }

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
                nqp::hllbool(nqp::existskey(elems,nqp::iterkey_s(iter)))
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

    my class Iterate does Rakudo::Iterator::Mappy {
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
    }
    method iterator(SetHash:D:) { Iterate.new($!elems) }

    my class KV does Rakudo::QuantHash::Quanty-kv {
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
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(  # doesn't sink
                target.push(nqp::iterval(nqp::shift($!iter))),
                target.push(True)
              )
            )
        }
    }
    multi method kv(SetHash:D:) { Seq.new(KV.new(self)) }

    my class Values does Rakudo::Iterator::Mappy {
        method pull-one() is rw {
          nqp::if(
            $!iter,
            proxy(nqp::shift($!iter),$!hash),
            IterationEnd
          )
        }
    }
    multi method values(SetHash:D:) { Seq.new(Values.new($!elems)) }

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

    multi method Setty(SetHash:U:) { SetHash }
    multi method Setty(SetHash:D:) { self }
    multi method Baggy(SetHash:U:) { BagHash }
    multi method Baggy(SetHash:D:) { self.BagHash }
    multi method Mixy (SetHash:U:) { MixHash }
    multi method Mixy (SetHash:D:) { self.MixHash }

#--- interface methods
    multi method STORE(SetHash:D: *@pairs --> SetHash:D) {
        nqp::if(
          (my \iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<initialize>,:what(self.^name))),
          self.SET-SELF(
            Rakudo::QuantHash.ADD-PAIRS-TO-SET(
              nqp::create(Rakudo::Internals::IterationSet),iterator,self.keyof
            )
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
}

# vim: ft=perl6 expandtab sw=4
