my class MixHash does Mixy {

#--- interface methods
    method total() { Rakudo::QuantHash.MIX-TOTAL($!elems) }
    method !total-positive() { Rakudo::QuantHash.MIX-TOTAL-POSITIVE($!elems) }

    multi method AT-KEY(MixHash:D: \k) is raw {
        Proxy.new(
          FETCH => {
              nqp::if(
                $!elems && nqp::existskey($!elems,(my $which := k.WHICH)),
                nqp::getattr(nqp::atkey($!elems,$which),Pair,'$!value'),
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
                      nqp::bindkey($!elems,$which,Pair.new(k,nqp::decont($value)))
                    )
                  ),
                  nqp::unless(                  # no hash allocated yet
                    $value == 0,
                    nqp::bindkey(
                      nqp::bindattr(self,::?CLASS,'$!elems',
                        nqp::create(Rakudo::Internals::IterationSet)),
                      k.WHICH,
                      Pair.new(k,nqp::decont($value))
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
    method clone() {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($!elems)),
          nqp::create(MixHash)
        )
    }

#--- iterator methods
    sub proxy(Mu \iter,Mu \storage) is raw {
        # We are only sure that the key exists when the Proxy
        # is made, but we cannot be sure of its existence when
        # either the FETCH or STORE block is executed.  So we
        # still need to check for existence, and handle the case
        # where we need to (re-create) the key and value.  The
        # logic is therefore basically the same as in AT-KEY,
        # except for tests for allocated storage and .WHICH
        # processing.
        nqp::stmts(
          (my $which := nqp::iterkey_s(iter)),
          # save for possible object recreation
          (my $object := nqp::getattr(nqp::iterval(iter),Pair,'$!key')),

          Proxy.new(
            FETCH => {
                nqp::if(
                  nqp::existskey(storage,$which),
                  nqp::getattr(nqp::atkey(storage,$which),Pair,'$!value'),
                  0
                )
            },
            STORE => -> $, Real() $value {
                nqp::if(
                  nqp::istype($value,Failure),  # RT 128927
                  $value.throw,
                  nqp::if(
                    nqp::existskey(storage,$which),
                    nqp::if(                    # existing element
                      $value == 0,
                      nqp::stmts(               # goodbye!
                        nqp::deletekey(storage,$which),
                        0
                      ),
                      nqp::bindattr(            # value ok
                        nqp::atkey(storage,$which),
                        Pair,
                        '$!value',
                        nqp::decont($value)
                      )
                    ),
                    nqp::unless(                # where did it go?
                      $value == 0,
                      nqp::bindkey(
                        storage,
                        $which,
                        Pair.new($object,nqp::decont($value))
                      )
                    )
                  )
                )
            }
          )
        )
    }

    multi method iterator(MixHash:D:) {    # also .pairs
        class :: does Rakudo::Iterator::Mappy {
            method pull-one() is raw {
                nqp::if(
                  $!iter,
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval(nqp::shift($!iter))),
                    Pair,
                    '$!value',
                    proxy($!iter,$!hash)
                  ),
                  IterationEnd
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::iterval(nqp::shift($!iter)))
                )
            }
        }.new($!elems)
    }

    multi method values(MixHash:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() is raw {
                nqp::if(
                  $!iter,
                  proxy(nqp::shift($!iter),$!hash),
                  IterationEnd
                )
            }

            # same as Baggy.values
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::getattr(
                    nqp::iterval(nqp::shift($!iter)),Pair,'$!value'))
                )
            }
        }.new($!elems))
    }

    multi method kv(MixHash:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy-kv-from-pairs {
            method pull-one() is raw {
                nqp::if(
                  $!on,
                  nqp::stmts(
                    ($!on = 0),
                    proxy($!iter,$!hash)
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!on = 1),
                      nqp::getattr(
                        nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
                    ),
                    IterationEnd
                  )
                )
            }
        }.new($!elems))
    }
}

# vim: ft=perl6 expandtab sw=4
