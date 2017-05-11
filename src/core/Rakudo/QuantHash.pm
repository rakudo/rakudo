my class Rakudo::QuantHash {

    our class WeightedRoll {
        has @!pairs;
        has $!total;

        method !SET-SELF(\list-of-pairs) {
            $!total = 0;
            for list-of-pairs.pairs {
                my $value := .value;
                if $value > 0 {
                    @!pairs.push($_);
                    $!total = $!total + $value;
                }
            }
            self
        }
        method new(\list-of-pairs) { nqp::create(self)!SET-SELF(list-of-pairs) }
        method roll() {
            my $rand = $!total.rand;
            my $seen = 0;
            return .key if ( $seen = $seen + .value ) > $rand for @!pairs;
        }
    }

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
        method new(\elems, \count) {
            nqp::if(
              elems && nqp::elems(elems) && count >= 1,
              nqp::create(self)!SET-SELF(elems, count),
              Rakudo::Iterator.Empty
            )
        }
    }

    # Return the iterator state of a randomly selected entry in a
    # given IterationSet
    method ROLL(Mu \elems) {
        nqp::stmts(
          (my int $i = nqp::add_i(nqp::elems(elems).rand.floor,1)),
          (my $iter := nqp::iterator(elems)),
          nqp::while(
            nqp::shift($iter) && ($i = nqp::sub_i($i,1)),
            nqp::null
          ),
          $iter
        )
    }

    # Return a list_s of all keys of the given IterationSet in random order.
    method PICK-N(Mu \elems, \count) {
        nqp::stmts(
          (my int $elems = nqp::elems(elems)),
          (my int $count = nqp::if(
            count >= nqp::elems(elems),
            nqp::elems(elems),
            count.Int
          )),
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
nqp::decont(   # can go when we got rid of containers in BagHashes
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
),
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
nqp::decont(   # can go when we get rid of containers in (Bag|Mix)Hashes
                  nqp::getattr(
                    nqp::iterval(nqp::shift($iter)),Pair,'$!value'
)
                  ),
                  Int
                )),
                $rand
              ),
            nqp::null
          ),
          $iter
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

    # Return random iterator item from a given Mix(Hash).  Takes an
    # initialized IterationSet with at least 1 element in Mix format,
    # and the total value of values in the Mix.
    method MIX-ROLL(\elems, \total) {
        nqp::stmts(
          (my Int $rand := total.rand.Int),
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
