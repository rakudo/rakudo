# This test file tests the following set operators:
#   (^)     set symmetric difference (Texas)
#   ⊖       set symmetric difference

proto sub infix:<(^)>(|) is pure { * }
multi sub infix:<(^)>()               { set()  }
multi sub infix:<(^)>(QuantHash:D $a) { $a     } # Set/Bag/Mix
multi sub infix:<(^)>(SetHash:D $a)   { $a.Set }
multi sub infix:<(^)>(BagHash:D $a)   { $a.Bag }
multi sub infix:<(^)>(MixHash:D $a)   { $a.Mix }
multi sub infix:<(^)>(Any $a)         { $a.Set } # also for Iterable/Map

multi sub infix:<(^)>(Setty:D $a, Setty:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                            # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
            nqp::stmts(                        # $a smallest, iterate over it
              (my $iter  := nqp::iterator($araw)),
              (my $elems := nqp::clone($braw))
            ),
            nqp::stmts(                        # $b smallest, iterate over that
              ($iter  := nqp::iterator($braw)),
              ($elems := nqp::clone($araw))
            )
          ),
          nqp::while(
            $iter,
            nqp::if(                           # remove if in both
              nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
              nqp::deletekey($elems,nqp::iterkey_s($iter)),
              nqp::bindkey($elems,nqp::iterkey_s($iter),nqp::iterval($iter))
            )
          ),
          nqp::create(Set).SET-SELF($elems)
        ),
        nqp::if(nqp::istype($a,Set),$a,$a.Set) # $b empty, so $a
      ),
      nqp::if(nqp::istype($b,Set),$b,$b.Set)   # $a empty, so $b
    )
}
multi sub infix:<(^)>(Setty:D $a, Mixy:D  $b) { $a.Mix (^) $b }
multi sub infix:<(^)>(Setty:D $a, Baggy:D $b) { $a.Bag (^) $b }
multi sub infix:<(^)>(Setty:D $a, Any     $b) { $a (^) $b.Set }

multi sub infix:<(^)>(Mixy:D $a, Mixy:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                            # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
            nqp::stmts(                        # $a smallest, iterate over it
              (my $iter  := nqp::iterator(my $base := $araw)),
              (my $elems := nqp::clone($braw))
            ),
            nqp::stmts(                        # $b smallest, iterate over that
              ($iter  := nqp::iterator($base := $braw)),
              ($elems := nqp::clone($araw))
            )
          ),
          nqp::while(
            $iter,
            nqp::if(
              nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
              nqp::if(
                (my $diff := nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                  - nqp::getattr(
                      nqp::atkey($elems,nqp::iterkey_s($iter)),
                      Pair,
                      '$!value'
                    )
                ),
                nqp::bindkey(
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($iter)),Pair,'$!value',abs($diff)
                  )
                ),
                nqp::deletekey($elems,nqp::iterkey_s($iter))
              ),
              nqp::bindkey(
                $elems,
                nqp::iterkey_s($iter),
                nqp::clone(nqp::iterval($iter))
              )
            )
          ),
          nqp::create(Mix).SET-SELF($elems)
        ),
        nqp::create(Mix).SET-SELF(             # $b empty, so $a
          Rakudo::QuantHash.MIX-CLONE-ALL-POSITIVE($araw)
        )
      ),
      nqp::if(
        ($braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::create(Mix).SET-SELF(             # $a empty, so $b
          Rakudo::QuantHash.MIX-CLONE-ALL-POSITIVE($braw)
        ),
        mix()
      )
    )
}
multi sub infix:<(^)>(Mixy:D $a, Baggy:D $b) { $a (^) $b.Mix }
multi sub infix:<(^)>(Mixy:D $a, Setty:D $b) { $a (^) $b.Mix }
multi sub infix:<(^)>(Mixy:D $a, Any     $b) { $a (^) $b.Mix }

multi sub infix:<(^)>(Baggy:D $a, Mixy:D $b) { $a.Mix (^) $b }
multi sub infix:<(^)>(Baggy:D $a, Baggy:D $b) {
    nqp::if(
      (my $araw := $a.RAW-HASH) && nqp::elems($araw),
      nqp::if(
        (my $braw := $b.RAW-HASH) && nqp::elems($braw),
        nqp::stmts(                            # both are initialized
          nqp::if(
            nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
            nqp::stmts(                        # $a smallest, iterate over it
              (my $iter  := nqp::iterator(my $base := $araw)),
              (my $elems := nqp::clone($braw))
            ),
            nqp::stmts(                        # $b smallest, iterate over that
              ($iter  := nqp::iterator($base := $braw)),
              ($elems := nqp::clone($araw))
            )
          ),
          nqp::while(
            $iter,
            nqp::if(                           # remove if in both
              nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
              nqp::if(
                (my int $diff = nqp::sub_i(
                  nqp::getattr(nqp::iterval($iter),Pair,'$!value'),
                  nqp::getattr(
                    nqp::atkey($elems,nqp::iterkey_s($iter)),
                    Pair,
                    '$!value'
                  )
                )),
                nqp::bindkey(
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($iter)),
                    Pair,
                    '$!value',
                    nqp::abs_i($diff)
                  )
                ),
                nqp::deletekey($elems,nqp::iterkey_s($iter))
              ),
              nqp::bindkey($elems,nqp::iterkey_s($iter),nqp::iterval($iter))
            )
          ),
          nqp::create(Bag).SET-SELF($elems)
        ),
        nqp::if(nqp::istype($a,Bag),$a,$a.Bag) # $b empty, so $a
      ),
      nqp::if(nqp::istype($b,Bag),$b,$b.Bag)   # $a empty, so $b
    )
}
multi sub infix:<(^)>(Baggy:D $a, Setty:D $b) { $a (^) $b.Bag }
multi sub infix:<(^)>(Baggy:D $a, Any     $b) { $a (^) $b.Bag }

multi sub infix:<(^)>(Map:D $a, Map:D $b) {
    nqp::if(
      nqp::elems((my $elems := Rakudo::QuantHash.COERCE-MAP-TO-SET($a))),
      nqp::if(                                    # $a has elems
        (my $raw := nqp::getattr(nqp::decont($b),Map,'$!storage'))
          && (my $iter := nqp::iterator($raw)),
        nqp::stmts(
          nqp::if(                                # both have elems
            nqp::eqaddr($b.keyof,Str(Any)),
            nqp::while(                           # ordinary hash
              $iter,
              nqp::if(
                nqp::iterval(nqp::shift($iter)),
                nqp::if(                          # should be checked
                  nqp::existskey(
                    $elems,
                    (my $which := nqp::iterkey_s($iter).WHICH)
                  ),
                  nqp::deletekey($elems,$which),  # remove existing
                  nqp::bindkey($elems,$which,nqp::iterkey_s($iter)) # add new
                )
              )
            ),
            nqp::while(                           # object hash
              $iter,
              nqp::if(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'),
                nqp::if(                          # should be checked
                  nqp::existskey($elems,nqp::iterkey_s($iter)),
                  nqp::deletekey($elems,nqp::iterkey_s($iter)),# remove existing
                  nqp::bindkey(                   # add new
                    $elems,
                    nqp::iterkey_s($iter),
                    nqp::getattr(nqp::iterval($iter),Pair,'$!key')
                  )
                )
              )
            )
          ),
          nqp::create(Set).SET-SELF($elems)       # done
        ),
        nqp::create(Set).SET-SELF($elems)         # nothing right, so make left
      ),
      $b.Set                                      # nothing left, coerce right
    )
}
multi sub infix:<(^)>(Any $a, Setty:D $b) { $a.Set (^) $b     }
multi sub infix:<(^)>(Any $a, Mixy:D  $b) { $a.Mix (^) $b     }
multi sub infix:<(^)>(Any $a, Baggy:D $b) { $a.Bag (^) $b     }
multi sub infix:<(^)>(Any $a, Any     $b) { $a.Set (^) $b.Set }

multi sub infix:<(^)>(**@p) is pure {

    # positions / size in minmax info
    my constant COUNT   = 0;
    my constant LOWEST  = 1;
    my constant HIGHEST = 2;
    my constant SIZE    = 3;

    # basic minmax for new keys
    my $init-minmax := nqp::setelems(nqp::create(IterationBuffer),SIZE);
    nqp::bindpos($init-minmax,COUNT,1);

    # handle key that has been seen before for given value
    sub handle-existing(Mu \elems, Mu \iter, \value --> Nil) {
        nqp::stmts(
          (my $minmax := nqp::getattr(
            nqp::atkey(elems,nqp::iterkey_s(iter)),Pair,'$!value')
          ),
          nqp::bindpos($minmax,COUNT,nqp::add_i(nqp::atpos($minmax,COUNT),1)),
          nqp::if(
            value > nqp::atpos($minmax,HIGHEST),
            nqp::stmts(
              nqp::bindpos($minmax,LOWEST,nqp::atpos($minmax,HIGHEST)),
              nqp::bindpos($minmax,HIGHEST,value)
            ),
            nqp::if(
              nqp::not_i(nqp::defined(nqp::atpos($minmax,LOWEST)))
                || value > nqp::atpos($minmax,LOWEST),
              nqp::bindpos($minmax,LOWEST,value)
            )
          )
        )
    }

    # handle key that has not yet been seen
    sub handle-new(Mu \elems, Mu \iter, \pair, \value) {
        nqp::stmts(
          (my $minmax := nqp::clone($init-minmax)),
          nqp::bindpos($minmax,HIGHEST,value),
          nqp::bindkey(
            elems,
            nqp::iterkey_s(iter),
            nqp::p6bindattrinvres(pair,Pair,'$!value',$minmax)
          )
        )
    }

    nqp::if(
      (my $params := @p.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action('symmetric diff'))),  # bye bye

      nqp::stmts(                                # fixed list of things to diff
        (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
        (my $type  := Set),
        (my int $pseen = 0),

        nqp::until(
          nqp::eqaddr((my $p := $params.pull-one),IterationEnd),

          nqp::stmts(                            # not done parsing
            ($pseen = nqp::add_i($pseen,1)),
            nqp::if(
              nqp::istype($p,Baggy),

              nqp::stmts(                        # Mixy/Baggy semantics apply
                nqp::unless(
                  nqp::istype($type,Mix),
                  ($type := nqp::if(nqp::istype($p,Mixy),Mix,Bag))
                ),
                nqp::if(
                  (my $raw := $p.RAW-HASH) && (my $iter := nqp::iterator($raw)),
                  nqp::stmts(                    # something to process
                    nqp::while(
                      $iter,
                      nqp::if(
                        nqp::existskey(
                          $elems,
                          nqp::iterkey_s(nqp::shift($iter))
                        ),
                        handle-existing(         # seen this element before
                          $elems,
                          $iter,
                          nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                        ),
                        handle-new(              # new element
                          $elems,
                          $iter,
                          nqp::clone(nqp::iterval($iter)),
                          nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                        )
                      )
                    )
                  )
                )
              ),

              nqp::stmts(                        # not a Baggy/Mixy, assume Set
                ($raw := nqp::if(nqp::istype($p,Setty),$p,$p.Set).RAW-HASH)
                  && ($iter := nqp::iterator($raw)),
                nqp::while(                      # something to process
                  $iter,
                  nqp::if(
                    nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
                    handle-existing(             # seen this element before
                      $elems,
                      $iter,
                      nqp::istrue(nqp::iterval($iter))
                    ),
                    handle-new(                  # new element
                      $elems,
                      $iter,
                      nqp::p6bindattrinvres(
                        nqp::create(Pair),Pair,'$!key',nqp::iterval($iter)),
                      nqp::istrue(nqp::iterval($iter))
                    )
                  )
                )
              )
            )
          )
        ),

        ($iter := nqp::iterator($elems)),        # start post-processing
        nqp::if(
          nqp::istype($type,Set),
          nqp::while(                            # need to create a Set
            $iter,
            nqp::if(
              nqp::ifnull(
                nqp::atpos(
                  nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'),
                  LOWEST
                ),
                0
              ) == 1,
              nqp::deletekey($elems,nqp::iterkey_s($iter)),    # seen > 1
              nqp::bindkey(                                    # only once
                $elems,                                        # convert to
                nqp::iterkey_s($iter),                         # Setty format
                nqp::getattr(nqp::iterval($iter),Pair,'$!key')
              )
            )
          ),
          nqp::if(
            nqp::istype($type,Mix),
            nqp::while(                          # convert to Mixy semantics
              $iter,
              nqp::stmts(
                (my $minmax :=
                  nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')),
                nqp::if(
                  nqp::islt_i(nqp::atpos($minmax,COUNT),$pseen),
                  handle-existing($elems,$iter,0)  # absentee == value 0 seen
                ),
                nqp::if(
                  nqp::ifnull(nqp::atpos($minmax,LOWEST),0)
                   == nqp::atpos($minmax,HIGHEST),
                  nqp::deletekey($elems,nqp::iterkey_s($iter)),  # top 2 same
                  nqp::bindattr(                                 # there's a
                    nqp::iterval($iter),                         # difference
                    Pair,                                        # so convert
                    '$!value',
                    nqp::atpos($minmax,HIGHEST)
                      - nqp::ifnull(nqp::atpos($minmax,LOWEST),0)
                  )
                )
              )
            ),
            nqp::while(                          # convert to Baggy semantics
              $iter,
              nqp::if(
                nqp::ifnull(
                  nqp::atpos(
                    ($minmax := nqp::getattr(
                      nqp::iterval(nqp::shift($iter)),Pair,'$!value')),
                    LOWEST
                  ),
                  0
                ) == nqp::atpos($minmax,HIGHEST),
                nqp::deletekey($elems,nqp::iterkey_s($iter)),    # top 2 same
                nqp::bindattr(                                   # there's a
                  nqp::iterval($iter),                           # difference
                  Pair,                                          # so convert
                  '$!value',
                  nqp::atpos($minmax,HIGHEST)
                    - nqp::ifnull(nqp::atpos($minmax,LOWEST),0)
                )
              )
            )
          )
        ),
        nqp::create($type).SET-SELF($elems)
      )
    )
}

# U+2296 CIRCLED MINUS
my constant &infix:<⊖> := &infix:<(^)>;

# vim: ft=perl6 expandtab sw=4
