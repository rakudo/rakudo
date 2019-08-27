my class MixHash does Mixy {

    method ^parameterize(Mu \base, Mu \type) {
        Rakudo::Internals.PARAMETERIZE-KEYOF(base,type)
    }

#--- interface methods
    method total() { Rakudo::QuantHash.MIX-TOTAL($!elems) }
    method !total-positive() { Rakudo::QuantHash.MIX-TOTAL-POSITIVE($!elems) }

    multi method STORE(MixHash:D: *@pairs --> MixHash:D) {
        nqp::if(
          (my \iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<initialize>,:what(self.^name))),
          self.SET-SELF(
            Rakudo::QuantHash.ADD-PAIRS-TO-MIX(
              nqp::create(Rakudo::Internals::IterationSet),iterator,self.keyof
            )
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
              nqp::if(
                $!elems && nqp::existskey($!elems,(my \which := k.WHICH)),
                nqp::getattr(nqp::atkey($!elems,which),Pair,'$!value'),
                0
              )
          },
          STORE => -> $, Real() $value {
              nqp::if(
                nqp::istype($value,Failure),   # RT 128927
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
    multi method Mix(MixHash:D: :$view) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::p6bindattrinvres(
            nqp::create(Mix),Mix,'$!elems',
            nqp::if($view,$!elems,$!elems.clone)
          ),
          mix()
        )
    }
    multi method MixHash(MixHash:D:) { self }

    multi method Setty(MixHash:U:) { SetHash      }
    multi method Setty(MixHash:D:) { self.SetHash }
    multi method Baggy(MixHash:U:) { BagHash      }
    multi method Baggy(MixHash:D:) { self.BagHash }
    multi method Mixy (MixHash:U:) { MixHash      }
    multi method Mixy (MixHash:D:) { self         }

    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(MixHash)
        )
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
        nqp::stmts(
          # save for possible object recreation
          (my $pair := nqp::atkey(elems,$key)),

          Proxy.new(
            FETCH => {
                nqp::if(
                  nqp::existskey(elems,$key),
                  nqp::getattr(nqp::atkey(elems,$key),Pair,'$!value'),
                  0
                )
            },
            STORE => -> $, Real() \value {
                nqp::if(
                  nqp::istype(value,Failure),  # RT 128927
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
        )
    }

    my class Iterate does Rakudo::Iterator::Mappy {
        method !SET-SELF(\elems) {
            nqp::bind($!hash,elems);
            nqp::bind($!iter,Rakudo::Internals.ITERATIONSET2LISTITER(elems));
            self
        }
        method pull-one() is raw {
            nqp::if(
              $!iter,
              nqp::p6bindattrinvres(
                nqp::clone(nqp::atkey($!hash,(my $key := nqp::shift($!iter)))),
                Pair,
                '$!value',
                proxy($key,$!hash)
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              target.push(nqp::atkey($!hash,nqp::shift($!iter)))
            )
        }
    }
    multi method iterator(MixHash:D:) { Iterate.new($!elems) }  # also .pairs

    my class KV does Rakudo::Iterator::Mappy-kv-from-pairs {
        method !SET-SELF(Mu \elems) {
            nqp::bind($!hash,elems);
            nqp::bind($!iter,Rakudo::Internals.ITERATIONSET2LISTITER(elems));
            self
        }
        method pull-one() is raw {
            nqp::if(
              $!on,
              nqp::stmts(
                (my $proxy := proxy($!on,$!hash)),
                ($!on = ""),
                $proxy
              ),
              nqp::if(
                $!iter,
                nqp::getattr(
                  nqp::atkey($!hash,($!on= nqp::shift($!iter))),Pair,'$!key'
                ),
                IterationEnd
              )
            )
        }
        method skip-one() {  # the one provided by the role interferes
            nqp::not_i(nqp::eqaddr(self.pull-one,IterationEnd))
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(  # doesn't sink
                (my $pair := nqp::atkey($!hash,nqp::shift($!iter))),
                target.push(nqp::getattr($pair,Pair,'$!key')),
                target.push(nqp::getattr($pair,Pair,'$!value'))
              )
            )
        }
    }
    multi method kv(MixHash:D:) { Seq.new(KV.new($!elems)) }

    my class Values does Rakudo::Iterator::Mappy {
        method !SET-SELF(\elems) {
            nqp::bind($!hash,elems);
            nqp::bind($!iter,Rakudo::Internals.ITERATIONSET2LISTITER(elems));
            self
        }
        method pull-one() is raw {
            nqp::if(
              $!iter,
              proxy(nqp::shift($!iter),$!hash),
              IterationEnd
            )
        }

        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              target.push(proxy(nqp::shift($!iter),$!hash))
            )
        }
    }
    multi method values(MixHash:D:) { Seq.new(Values.new($!elems)) }
}

# vim: ft=perl6 expandtab sw=4
