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
    nqp::if(
      (my $params := @p.iterator).is-lazy,
      Failure.new(X::Cannot::Lazy.new(:action('symmetric diff'))),  # bye bye

      nqp::stmts(                                 # fixed list of things to diff
        (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
        (my $type  := Set),
        (my $initial-minmax := nqp::setelems(nqp::create(IterationBuffer),2)),
        nqp::bindpos($initial-minmax,1,1),

        nqp::until(
          nqp::eqaddr((my $p := $params.pull-one),IterationEnd),

          nqp::if(                              # not done parsing
            nqp::istype($p,Baggy),

            nqp::stmts(                         # Mixy/Baggy semantics apply
              nqp::unless(
                nqp::istype($type,Mix),
                ($type := nqp::if(nqp::istype($p,Mixy),Mix,Bag))
              ),
              nqp::if(
                (my $raw := $p.RAW-HASH) && (my $iter := nqp::iterator($raw)),
                nqp::while(                     # something to process
                  $iter,
                  nqp::if(
                    nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
                    nqp::if(                    # seen this element before
                      (my $value := nqp::getattr(
                        nqp::iterval($iter),
                        Pair,
                        '$!value'
                      )) > nqp::atpos(
                        (my $minmax := nqp::getattr(
                          nqp::atkey($elems,nqp::iterkey_s($iter)),
                          Pair,
                          '$!value'
                        )),1
                      ),
                      nqp::stmts(               # higher than highest
                        nqp::bindpos($minmax,0,nqp::atpos($minmax,1)),
                        nqp::bindpos($minmax,1,$value)
                      ),
                      nqp::if(
                        nqp::not_i(nqp::defined(nqp::atpos($minmax,0)))
                          || $value > nqp::atpos($minmax,0),
                        nqp::bindpos($minmax,0,$value) # higher than lowest
                      )
                    ),

                    nqp::stmts(                 # new element
                      ($minmax :=
                        nqp::setelems(nqp::create(IterationBuffer),2)),
                      nqp::bindpos($minmax,1,nqp::getattr(
                        nqp::iterval($iter),Pair,'$!value'
                      )),
                      nqp::bindkey(
                        $elems,
                        nqp::iterkey_s($iter),
                        nqp::p6bindattrinvres(
                          nqp::clone(nqp::iterval($iter)),
                          Pair,'$!value',$minmax
                        )
                      )
                    )
                  )
                )
              )
            ),

            nqp::stmts(                           # not a Baggy/Mixy, assume Set
              ($raw := nqp::if(nqp::istype($p,Setty),$p,$p.Set).RAW-HASH)
                && ($iter := nqp::iterator($raw)),
              nqp::while(                         # something to process
                $iter,
                nqp::if(
                  nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
                  nqp::if(                        # seen this element before
                    nqp::atpos(
                      ($minmax := nqp::getattr(
                        nqp::atkey($elems,nqp::iterkey_s($iter)),
                        Pair,
                        '$!value'
                      )),1
                    ) < 1,
                    nqp::stmts(                   # higher than highest
                      nqp::bindpos($minmax,0,nqp::atpos($minmax,1)),
                      nqp::bindpos($minmax,1,1)
                    ),
                    nqp::if(
                      nqp::not_i(nqp::defined(nqp::atpos($minmax,0)))
                        || nqp::atpos($minmax,0) < 1,
                      nqp::bindpos($minmax,0,1)   # higher than lowest
                    )
                  ),
                  nqp::bindkey(                   # new element
                    $elems,
                    nqp::iterkey_s($iter),
                    Pair.new(nqp::iterval($iter),nqp::clone($initial-minmax))
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
                  (nqp::getattr(
                    nqp::iterval(nqp::shift($iter)),
                    Pair,
                    '$!value'
                  )),0
                ),0
              ) == 1,
              nqp::deletekey($elems,nqp::iterkey_s($iter)),    # seen > 1
              nqp::bindkey(                                    # only once
                $elems,                                        # convert to
                nqp::iterkey_s($iter),                         # Setty format
                nqp::getattr(nqp::iterval($iter),Pair,'$!key')
              )
            )
          ),
          nqp::while(                            # need to create a Baggy/Mixy
            $iter,
            nqp::if(
              nqp::ifnull(
                nqp::atpos(
                  ($minmax := nqp::getattr(
                    nqp::iterval(nqp::shift($iter)),Pair,'$!value'
                  )),0
                ),
                0
              ) == nqp::atpos($minmax,1),
              nqp::deletekey($elems,nqp::iterkey_s($iter)),    # top 2 same
              nqp::bindattr(                                   # there's a
                nqp::iterval($iter),                           # difference
                Pair,                                          # so convert to
                '$!value',                                     # Baggy semantics
                nqp::atpos($minmax,1) - nqp::ifnull(nqp::atpos($minmax,0),0)
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
