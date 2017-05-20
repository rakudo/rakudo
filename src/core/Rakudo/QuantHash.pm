my class Rakudo::QuantHash {

    our role Pairs does Iterator {
        has $!elems;
        has $!picked;

        method !SET-SELF(\elems,\count) {
            nqp::stmts(
              ($!elems := elems),
              ($!picked := Rakudo::QuantHash.PICK-N(elems, count)),
              self
            )
        }
        method new(Mu \elems, \count) {
            nqp::if(
              (my $todo := Rakudo::QuantHash.TODO(count))
                && elems
                && nqp::elems(elems),
              nqp::create(self)!SET-SELF(elems, $todo),
              Rakudo::Iterator.Empty
            )
        }
    }

    # Return the iterator state of a randomly selected entry in a
    # given IterationSet
    method ROLL(Mu \elems) {
        nqp::stmts(
          (my int $i = nqp::add_i(nqp::rand_n(nqp::elems(elems)),1)),
          (my $iter := nqp::iterator(elems)),
          nqp::while(
            nqp::shift($iter) && ($i = nqp::sub_i($i,1)),
            nqp::null
          ),
          $iter
        )
    }

    # Return a list_s of N keys of the given IterationSet in random order.
    method PICK-N(Mu \elems, \count) {
        nqp::stmts(
          (my int $elems = nqp::elems(elems)),
          (my int $count = nqp::if(count > $elems,$elems,count)),
          (my $keys := nqp::setelems(nqp::list_s,$elems)),
          (my $iter := nqp::iterator(elems)),
          (my int $i = -1),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::bindpos_s($keys,$i,nqp::iterkey_s(nqp::shift($iter)))
          ),
          (my $picked := nqp::setelems(nqp::list_s,$count)),
          ($i = -1),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$count),
            nqp::stmts(
              nqp::bindpos_s($picked,$i,
                nqp::atpos_s($keys,(my int $pick = $elems.rand.floor))
              ),
              nqp::bindpos_s($keys,$pick,
                nqp::atpos_s($keys,($elems = nqp::sub_i($elems,1)))
              )
            )
          ),
          $picked
        )
    }

    # Return number of items to be done if > 0, or 0 if < 1, or throw if NaN
    method TODO(\count) is raw {
        nqp::if(
          count < 1,
          0,
          nqp::if(
            count == Inf,
            count,
            nqp::if(
              nqp::istype((my $todo := count.Int),Failure),
              $todo.throw,
              $todo
            )
          )
        )
    }

#--- Bag/BagHash related methods

    # Calculate total of value of a Bag(Hash).  Takes a (possibly
    # uninitialized) IterationSet in Bag format.
    method BAG-TOTAL(Mu \elems) {
        nqp::if(
          elems && nqp::elems(elems),
          nqp::stmts(
            (my Int $total := 0),
            (my $iter := nqp::iterator(elems)),
            nqp::while(
              $iter,
              $total := nqp::add_I(
                $total,
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'),
                Int
              )
            ),
            $total
          ),
          0
        )
    }

    # Return random iterator item from a given Bag(Hash).  Takes an
    # initialized IterationSet with at least 1 element in Bag format,
    # and the total value of values in the Bag.
    method BAG-ROLL(\elems, \total) {
        nqp::stmts(
          (my Int $rand := total.rand.Int),
          (my Int $seen := 0),
          (my $iter := nqp::iterator(elems)),
          nqp::while(
            $iter &&
              nqp::isle_I(
                ($seen := nqp::add_I(
                  $seen,
                  nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'),
                  Int
                )),
                $rand
              ),
            nqp::null
          ),
          $iter
        )
    }

    # Return random object from a given BagHash.  Takes an initialized
    # IterationSet with at least 1 element in Bag format, and the total
    # value of values in the Bag.  Decrements the count of the iterator
    # found, completely removes it when going to 0.
    method BAG-GRAB(\elems, \total) {
        nqp::stmts(
          (my $iter := Rakudo::QuantHash.BAG-ROLL(elems,total)),
          nqp::if(
            nqp::iseq_i(
              (my $value := nqp::getattr(nqp::iterval($iter),Pair,'$!value')),
              1
            ),
            nqp::stmts(              # going to 0, so remove
              (my $object := nqp::getattr(nqp::iterval($iter),Pair,'$!key')),
              nqp::deletekey(elems,nqp::iterkey_s($iter)),
              $object
            ),
            nqp::stmts(
              nqp::bindattr(
                nqp::iterval($iter),
                Pair,
                '$!value',
                nqp::sub_i($value,1)
              ),
              nqp::getattr(nqp::iterval($iter),Pair,'$!key')
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
                  nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                )
              )
            ),
            $elems
          ),
          baggy
        )
    }

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

#--- Mix/MixHash related methods

    # Calculate total of values of a Mix(Hash).  Takes a (possibly
    # uninitialized) IterationSet in Mix format.
    method MIX-TOTAL(Mu \elems) {
        nqp::if(
          elems && nqp::elems(elems),
          nqp::stmts(
            (my $total := 0),
            (my $iter := nqp::iterator(elems)),
            nqp::while(
              $iter,
              $total := $total
                + nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
            ),
            $total
          ),
          0
        )
    }

    # Calculate total of positive value of a Mix(Hash).  Takes a
    # (possibly uninitialized) IterationSet in Mix format.
    method MIX-TOTAL-POSITIVE(Mu \elems) {
        nqp::if(
          elems && nqp::elems(elems),
          nqp::stmts(
            (my $total := 0),
            (my $iter := nqp::iterator(elems)),
            nqp::while(
              $iter,
              nqp::if(
                0 < (my $value := 
                  nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')),
                ($total := $total + $value)
              )
            ),
            $total
          ),
          0
        )
    }

    # Return random iterator item from a given Mix(Hash).  Takes an
    # initialized IterationSet with at least 1 element in Mix format,
    # and the total value of values in the Mix.
    method MIX-ROLL(\elems, \total) {
        nqp::stmts(
          (my     $rand := total.rand),
          (my Int $seen := 0),
          (my $iter := nqp::iterator(elems)),
          nqp::while(
            $iter && (
              0 > (my $value :=                      # negative values ignored
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'))
              || $rand > ($seen := $seen + $value)   # positive values add up
            ),
            nqp::null
          ),
          $iter
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

# vim: ft=perl6 expandtab sw=4
