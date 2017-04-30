my class Rakudo::QuantHash {

    method ADD-BAG-TO-BAG(\elems,Mu \bag --> Nil) {
        nqp::if(
          bag && nqp::elems(bag),
          nqp::stmts(
            (my $iter := nqp::iterator(bag)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::existskey(elems,nqp::iterkey_s(nqp::shift($iter))),
                nqp::stmts(
                  (my $pair := nqp::atkey(elems,nqp::iterkey_s($iter))),
                  nqp::bindattr($pair,Pair,'$!value',
                    nqp::add_i(
                      nqp::getattr($pair,Pair,'$!value'),
                      nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                    )
                  )
                ),
                nqp::bindkey(elems,nqp::iterkey_s($iter),
                  nqp::clone(nqp::iterval($iter))
                )
              )
            )
          )
        )
    }

    method ADD-ITERATOR-TO-BAG(\elems,Mu \iterator --> Nil) {
        nqp::until(
          nqp::eqaddr((my $pulled := iterator.pull-one),IterationEnd),
          nqp::if(
            nqp::existskey(elems,(my $WHICH := $pulled.WHICH)),
            nqp::stmts(
              (my $pair := nqp::atkey(elems,$WHICH)),
              nqp::bindattr($pair,Pair,'$!value',
                nqp::add_i(nqp::getattr($pair,Pair,'$!value'),1)
              )
            ),
            nqp::bindkey(elems,$WHICH,Pair.new($pulled,1))
          )
        )
    }

    method ADD-SET-TO-BAG(\elems,Mu \set --> Nil) {
        nqp::if(
          set && nqp::elems(set),
          nqp::stmts(
            (my $iter := nqp::iterator(set)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::existskey(elems,nqp::iterkey_s(nqp::shift($iter))),
                nqp::stmts(
                  (my $pair := nqp::atkey(elems,nqp::iterkey_s($iter))),
                  nqp::bindattr($pair,Pair,'$!value',
                    nqp::add_i(nqp::getattr($pair,Pair,'$!value'),1)
                  )
                ),
                nqp::bindkey(elems,nqp::iterkey_s($iter),
                  Pair.new(nqp::iterval($iter),1)
                )
              )
            )
          )
        )
    }

    method ADD-MIX-TO-MIX(\elems,Mu \mix --> Nil) {
        nqp::if(
          mix && nqp::elems(mix),
          nqp::stmts(
            (my $iter := nqp::iterator(mix)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::existskey(elems,nqp::iterkey_s(nqp::shift($iter))),
                nqp::stmts(
                  (my $pair := nqp::atkey(elems,nqp::iterkey_s($iter))),
                  nqp::bindattr($pair,Pair,'$!value',
                    nqp::getattr($pair,Pair,'$!value')
                    + nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                  )
                ),
                nqp::bindkey(elems,nqp::iterkey_s($iter),
                  nqp::clone(nqp::iterval($iter))
                )
              )
            )
          )
        )
    }

    method BAGGY-CLONE-RAW(Mu \baggy) {
        nqp::if(
          baggy && nqp::elems(baggy),
          nqp::stmts(                             # something to coerce
            (my $elems := nqp::clone(baggy)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::bindkey(
                $elems,
                nqp::iterkey_s(nqp::shift($iter)),
                nqp::p6bindattrinvres(
                  nqp::clone(nqp::iterval($iter)),
                  Pair,
                  '$!value',
                  nqp::decont(
                    nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                  )
                )
              )
            ),
            $elems
          ),
          baggy
        )
    }

    method MULTIPLY-BAG-TO-BAG(\elems,Mu \bag --> Nil) {
        nqp::stmts(
          (my $iter := nqp::iterator(elems)),
          nqp::if(
            bag && nqp::elems(bag),
            nqp::while(
              $iter,
              nqp::if(
                nqp::existskey(bag,nqp::iterkey_s(nqp::shift($iter))),
                nqp::stmts(
                  (my $pair := nqp::iterval($iter)),
                  nqp::bindattr($pair,Pair,'$!value',
                    nqp::mul_i(
                      nqp::getattr($pair,Pair,'$!value'),
                      nqp::getattr(
                        nqp::atkey(bag,nqp::iterkey_s($iter)),
                        Pair,
                        '$!value'
                      )
                    )
                  )
                ),
                nqp::deletekey(elems,nqp::iterkey_s($iter))
              )
            ),
            nqp::while(   # nothing to match against, so reset
              $iter,
              nqp::deletekey(elems,nqp::iterkey_s(nqp::shift($iter)))
            )
          )
        )
    }

    method MULTIPLY-SET-TO-BAG(\elems,Mu \set --> Nil) {
        nqp::stmts(
          (my $iter := nqp::iterator(elems)),
          nqp::if(
            set && nqp::elems(set),
            nqp::while(
              $iter,
              nqp::unless(
                nqp::existskey(set,nqp::iterkey_s(nqp::shift($iter))),
                nqp::deletekey(elems,nqp::iterkey_s($iter))
              )
            ),
            nqp::while(   # nothing to match against, so reset
              $iter,
              nqp::deletekey(elems,nqp::iterkey_s(nqp::shift($iter)))
            )
          )
        )
    }

    method MULTIPLY-MIX-TO-MIX(\elems,Mu \mix --> Nil) {
        nqp::stmts(
          (my $iter := nqp::iterator(elems)),
          nqp::if(
            mix && nqp::elems(mix),
            nqp::while(
              $iter,
              nqp::if(
                nqp::existskey(mix,nqp::iterkey_s(nqp::shift($iter))),
                nqp::stmts(
                  (my $pair := nqp::iterval($iter)),
                  nqp::bindattr($pair,Pair,'$!value',
                    nqp::getattr($pair,Pair,'$!value')
                    * nqp::getattr(
                        nqp::atkey(mix,nqp::iterkey_s($iter)),
                        Pair,
                        '$!value'
                      )
                  )
                ),
                nqp::deletekey(elems,nqp::iterkey_s($iter))
              )
            ),
            nqp::while(   # nothing to match against, so reset
              $iter,
              nqp::deletekey(elems,nqp::iterkey_s(nqp::shift($iter)))
            )
          )
        )
    }
}
