my role Real { ... }
my class X::TypeCheck::Binding { ... }

my class Rakudo::QuantHash {

    # a Pair with the value 0
    my $p0 := nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0);

    # Specialized role for .kv methods on QuantHashes: copied methods
    # from Quanty because of visibility issues wrt to $!elems and $!iter :-(
    our role Quanty-kv does Iterator {
        has $!elems;
        has $!iter;
        has $!on;

        method !SET-SELF(\quanthash) {
            nqp::if(
              ($!elems := quanthash.RAW-HASH) && nqp::elems($!elems),
              nqp::stmts(
                ($!iter := nqp::iterator($!elems)),
                self
              ),
              Rakudo::Iterator.Empty   # nothing to iterate
            )
        }
        method new(\quanthash) { nqp::create(self)!SET-SELF(quanthash) }
        method skip-one() {
            nqp::if(
              $!on,
              nqp::not_i($!on = 0),
              nqp::if(
                $!iter,
                nqp::stmts(
                  nqp::shift($!iter),
                  ($!on = 1)
                )
              )
            )
        }
        method sink-all(--> IterationEnd) { $!iter := nqp::null }
        method is-deterministic(--> False) { }
    }

    our role Pairs does Iterator {
        has $!elems;
        has $!picked;

        method !SET-SELF(\elems,\count) {
            $!elems := elems;
            $!picked := Rakudo::QuantHash.PICK-N(elems, count);
            self
        }
        method new(Mu \elems, \count) {
            (my $todo := Rakudo::QuantHash.TODO(count))
              && elems
              && nqp::elems(elems)
              ?? nqp::create(self)!SET-SELF(elems, $todo)
              !! Rakudo::Iterator.Empty
        }
        method is-deterministic(--> False) { }
    }

    # Return the iterator state of a randomly selected entry in a
    # given IterationSet
    method ROLL(Mu \elems) {
        my int $i = nqp::add_i(nqp::rand_n(nqp::elems(elems)),1);
        my $iter := nqp::iterator(elems);

        nqp::while(
          nqp::shift($iter) && ($i = nqp::sub_i($i,1)),
          nqp::null
        );
        $iter
    }

    # Return a list_s of N keys of the given IterationSet in random order.
    method PICK-N(Mu \elems, \count) {
        my int $elems = nqp::elems(elems);
        my int $count = count > $elems ?? $elems !! count;
        my $keys := nqp::setelems(nqp::list_s,$elems);
        my $iter := nqp::iterator(elems);
        my int $i = -1;

        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos_s($keys,$i,nqp::iterkey_s(nqp::shift($iter)))
        );

        my $picked := nqp::setelems(nqp::list_s,$count);
        $i = -1;

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
        );
        $picked
    }

    # Return number of items to be done if > 0, or 0 if < 1, or throw if NaN
    method TODO(\count) is raw {
        count < 1
          ?? 0
          !! count == Inf
            ?? count
            !! nqp::istype((my $todo := count.Int),Failure)
              ?? $todo.throw
              !! $todo
    }

    # Return an nqp::list_s of all keys of a QuantHash
    method RAW-KEYS(\quanthash) is raw {
        nqp::if(
          (my $elems := quanthash.RAW-HASH)
            && (my $iter := nqp::iterator($elems)),
          nqp::stmts(
            (my $keys :=    # presize result back to 0 so we can push_s
              nqp::setelems(nqp::setelems(nqp::list_s,nqp::elems($elems)),0)),
            nqp::while(
              $iter,
              nqp::push_s($keys,nqp::iterkey_s(nqp::shift($iter)))
            ),
            $keys
          ),
          nqp::list_s
        )
    }

    # Return an nqp::list_s of all values of a QuantHash, mapped to a str
    method RAW-VALUES-MAP(\quanthash, &mapper) is raw {
        nqp::if(
          (my $elems := quanthash.RAW-HASH)
            && (my $iter := nqp::iterator($elems)),
          nqp::stmts(
            (my $values :=    # presize result back to 0 so we can push_s
              nqp::setelems(nqp::setelems(nqp::list_s,nqp::elems($elems)),0)),
            nqp::while(
              $iter,
              nqp::push_s($values,mapper(nqp::iterval(nqp::shift($iter))))
            ),
            $values
          ),
          nqp::list_s
        )
    }

    # Return an nqp::list_s of all keys in a Baggy with the weight
    # joined with a null-byte inbetween.
    method BAGGY-RAW-KEY-VALUES(\baggy) is raw {
        nqp::if(
          (my $elems := baggy.RAW-HASH)
            && (my $iter := nqp::iterator($elems)),
          nqp::stmts(
            (my $list :=    # presize result back to 0 so we can push_s
              nqp::setelems(nqp::setelems(nqp::list_s,nqp::elems($elems)),0)),
            nqp::while(
              $iter,
              nqp::stmts(
                nqp::shift($iter),
                nqp::push_s(
                  $list,
                  nqp::concat(
                    nqp::iterkey_s($iter),
                    nqp::concat(
                      '\0',
                      nqp::getattr(nqp::iterval($iter),Pair,'$!value').Str
                    )
                  )
                )
              )
            ),
            $list
          ),
          nqp::list_s
        )
    }

    # Create intersection of 2 Baggies, default to given type (Bag|Mix)
    method INTERSECT-BAGGIES(\a,\b,\type) {
        my $object := nqp::create(
          nqp::istype(type,Mix) ?? a.WHAT.Mixy !! a.WHAT.Baggy
        );

        nqp::if(
          (my $araw := a.RAW-HASH) && nqp::elems($araw)
            && (my $braw := b.RAW-HASH) && nqp::elems($braw),
          nqp::stmts(                        # both have elems
            nqp::if(
              nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
              nqp::stmts(                    # $a smallest, iterate over it
                (my $iter := nqp::iterator($araw)),
                (my $base := $braw)
              ),
              nqp::stmts(                    # $b smallest, iterate over that
                ($iter := nqp::iterator($braw)),
                ($base := $araw)
              )
            ),
            (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
            nqp::while(
              $iter,
              nqp::if(                       # bind if in both
                nqp::existskey($base,nqp::iterkey_s(nqp::shift($iter))),
                nqp::bindkey(
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::if(
                    nqp::getattr(
                      nqp::decont(nqp::iterval($iter)),
                      Pair,
                      '$!value'
                    ) < nqp::getattr(        # must be HLL comparison
                          nqp::atkey($base,nqp::iterkey_s($iter)),
                          Pair,
                          '$!value'
                        ),
                    nqp::iterval($iter),
                    nqp::atkey($base,nqp::iterkey_s($iter))
                  )
                )
              )
            ),
            $object.SET-SELF($elems)
          ),
          $object                            # one/neither has elems
        )
    }

    # create a deep clone of the given IterSet with baggy
    method BAGGY-CLONE(\raw) {
        my $elems := nqp::clone(raw);
        my $iter  := nqp::iterator($elems);

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
        );
        $elems
    }

#--- Set/SetHash related methods

    # Create an IterationSet with baggy semantics from IterationSet with
    # Setty semantics.
    method SET-BAGGIFY(\raw) {
        my $elems := nqp::clone(raw);
        my $iter  := nqp::iterator($elems);

        nqp::while(
          $iter,
          nqp::bindkey(
            $elems,
            nqp::iterkey_s(nqp::shift($iter)),
            Pair.new(nqp::decont(nqp::iterval($iter)),1)
          )
        );
        $elems
    }

    # bind the given value to the given IterationSet, check for given type
    method BIND-TO-TYPED-SET(\elems, Mu \value, Mu \type --> Nil) {
        nqp::istype(value,type)
          ?? nqp::bindkey(elems,value.WHICH,value)
          !! X::TypeCheck::Binding.new(
               got      => value,
               expected => type
             ).throw
    }

    # add to given IterationSet with setty semantics the values of iterator
    method ADD-ITERATOR-TO-SET(\elems,Mu \iterator, Mu \type) {
        nqp::until(
          nqp::eqaddr(
            (my \pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          self.BIND-TO-TYPED-SET(elems, pulled, type)
        );
        elems
    }

    # Add to IterationSet with setty semantics the values of the given
    # iterator while checking for Pairs (only include if value is trueish)
    method ADD-PAIRS-TO-SET(\elems,Mu \iterator, Mu \type) {
        nqp::until(
          nqp::eqaddr(
            (my \pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::if(
            nqp::istype(pulled,Pair),
            nqp::if(
              nqp::getattr(pulled,Pair,'$!value'),
              self.BIND-TO-TYPED-SET(
                elems, nqp::getattr(pulled,Pair,'$!key'), type
              )
            ),
            self.BIND-TO-TYPED-SET(elems, pulled, type)
          )
        );
        elems
    }

    # Add to given IterationSet with setty semantics the values of the two
    # given iterators where the first iterator supplies objects, and the
    # second supplies values (only include if value is trueish).
    method ADD-OBJECTS-VALUES-TO-SET(\elems,Mu \objects, Mu \bools) is raw {
        nqp::until(
          nqp::eqaddr((my \object := objects.pull-one),IterationEnd),
          nqp::if(
            bools.pull-one,
            nqp::bindkey(elems,object.WHICH,nqp::decont(object))
          )
        );
        elems
    }

    # Add to given IterationSet with setty semantics the keys of given Map
    method ADD-MAP-TO-SET(\elems, \map) {
        nqp::if(
          (my $iter :=
            nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage'))),
          nqp::if(
            nqp::istype(map,Hash::Object),
            nqp::while(                        # object hash
              $iter,
              nqp::if(
                nqp::getattr(
                  nqp::decont(nqp::iterval(nqp::shift($iter))),
                  Pair,
                  '$!value'
                ),
                nqp::bindkey(
                  elems,
                  nqp::iterkey_s($iter),
                  nqp::getattr(nqp::iterval($iter),Pair,'$!key')
                )
              )
            ),
            nqp::while(                        # normal Map
              $iter,
              nqp::if(
                nqp::iterval(nqp::shift($iter)),
                nqp::bindkey(
                  elems,nqp::iterkey_s($iter).WHICH,nqp::iterkey_s($iter))
              )
            )
          )
        );
        elems
    }

    # coerce a Map to an IterationSet with setty semantics
    method COERCE-MAP-TO-SET(\map) {
        # Once object hashes have IterationSets, we could optimize the
        # object hash case by cloning the object hash, rather than creating
        # an empty IterationSet.  Until then, this is just a wrapper.
        Rakudo::QuantHash.ADD-MAP-TO-SET(
          nqp::create(Rakudo::Internals::IterationSet),
          map
        )
    }

    # remove set elements from set, stop when the result is the empty Set
    method SUB-SET-FROM-SET(\aelems, \belems) {
        my $elems := nqp::clone(aelems);     # both have elems
        my $iter  := nqp::iterator(belems);

        nqp::while(
          $iter && nqp::elems($elems),
          nqp::deletekey($elems,nqp::iterkey_s(nqp::shift($iter)))
        );
        $elems
    }

    # remove hash elements from set, stop if the result is the empty Set
    method SUB-MAP-FROM-SET(\aelems, \map) {
        my $elems := nqp::clone(aelems);

        nqp::if(
          (my $iter :=
            nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage'))),
          nqp::if(
            nqp::istype(map,Hash::Object),
            nqp::while(                     # object hash
              $iter && nqp::elems($elems),
              nqp::if(
                nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'),
                nqp::deletekey($elems,nqp::iterkey_s($iter))
              )
            ),
            nqp::while(                     # normal Map
              $iter && nqp::elems($elems),
              nqp::if(
                nqp::iterval(nqp::shift($iter)),
                nqp::deletekey($elems,nqp::iterkey_s($iter).WHICH)
              )
             )
          )
        );
        $elems
    }

    # remove iterator elements from set using Pair semantics, stops pulling
    # from the iterator as soon as the result is the empty set.
    method SUB-PAIRS-FROM-SET(\elems, \iterator) {
        my $elems := nqp::clone(elems);

        nqp::until(
          nqp::eqaddr(                            # end of iterator?
            (my $pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ) || nqp::not_i(nqp::elems($elems)),    # nothing left to remove?
          nqp::if(
            nqp::istype($pulled,Pair),
            nqp::if(                              # must check for thruthiness
              nqp::getattr($pulled,Pair,'$!value'),
              nqp::deletekey($elems,nqp::getattr($pulled,Pair,'$!key').WHICH)
            ),
            nqp::deletekey($elems,$pulled.WHICH)  # attempt to remove
          )
        );
        $elems
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
        my Int $rand := total.rand.Int;
        my Int $seen := 0;
        my $iter := nqp::iterator(elems);

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
        );
        $iter
    }

    # Return random object from a given BagHash.  Takes an initialized
    # IterationSet with at least 1 element in Bag format, and the total
    # value of values in the Bag.  Decrements the count of the iterator
    # found, completely removes it when going to 0.
    method BAG-GRAB(\elems, \total) {
        my $iter := Rakudo::QuantHash.BAG-ROLL(elems,total);

        nqp::if(
          (my $value := nqp::getattr(nqp::iterval($iter),Pair,'$!value')) == 1,
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
              $value - 1
            ),
            nqp::getattr(nqp::iterval($iter),Pair,'$!key')
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

    method ADD-BAG-TO-BAG(\elems,Mu \bag) {
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
        );
        elems
    }

    # bind the given which/object/value to the given IterationSet,
    # check object for given type
    method BIND-TO-TYPED-BAG(
      \elems, Mu \which, Mu \object, Int:D \value, Mu \type
    --> Nil) {
        nqp::istype(object,type)
          ?? nqp::bindkey(elems,which,Pair.new(object,value))
          !! X::TypeCheck::Binding.new(
               got      => object,
               expected => type
             ).throw
    }

    method ADD-ITERATOR-TO-BAG(\elems, Mu \iterator, Mu \type) {
        nqp::until(
          nqp::eqaddr(
            (my \pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::stmts(
            (my $pair := nqp::ifnull(
              nqp::atkey(elems,(my \which := pulled.WHICH)),
              nqp::if(
                nqp::istype(pulled,type),
                nqp::bindkey(elems,which,Pair.new(pulled,0)),
                X::TypeCheck::Binding.new(
                  got      => pulled,
                  expected => type
                ).throw
              )
            )),
            nqp::bindattr($pair,Pair,'$!value',
              nqp::add_i(nqp::getattr($pair,Pair,'$!value'),1)
            )
          )
        );
        elems
    }

    method SUB-ITERATOR-FROM-BAG(\elems, Mu \iterator) {
        nqp::until(
          nqp::eqaddr(
            (my \pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::if(
            nqp::existskey(elems,(my \which := pulled.WHICH)),
            nqp::stmts(
              (my \pair := nqp::atkey(elems,which)),
              nqp::if(
                nqp::isgt_i((my \freq := nqp::getattr(pair,Pair,'$!value')),1),
                nqp::bindattr(pair,Pair,'$!value',nqp::sub_i(freq,1)),
                nqp::deletekey(elems,which)
              )
            )
          )
        );
        elems
    }

    # Add to given IterationSet with baggy semantics the keys of given Map
    method ADD-MAP-TO-BAG(\elems, \map) {
        nqp::if(
          (my $iter :=
            nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage'))),
          nqp::if(
            nqp::istype(map,Hash::Object),
            nqp::while(              # object hash
              $iter,
              nqp::if(
                nqp::istype(
                  (my $value := nqp::getattr(
                    nqp::iterval(nqp::shift($iter)),Pair,'$!value'
                  ).Int),
                  Int
                ),
                nqp::if(             # a valid Int
                  $value > 0,
                  nqp::if(           # and a positive one at that
                    nqp::existskey(elems,nqp::iterkey_s($iter)),
                    nqp::stmts(      # seen before, add value
                      (my $pair := nqp::atkey(elems,nqp::iterkey_s($iter))),
                      nqp::bindattr(
                        $pair,
                        Pair,
                        '$!value',
                        nqp::getattr($pair,Pair,'$!value') + $value
                      )
                    ),
                    nqp::bindkey(    # new, create new Pair
                      elems,
                      nqp::iterkey_s($iter),
                      nqp::p6bindattrinvres(
                        nqp::clone(nqp::iterval($iter)),
                        Pair,
                        '$!value',
                        $value
                      )
                    )
                  )
                ),
                $value.throw         # huh?  let the world know
              )
            ),
            nqp::while(              # ordinary Map
              $iter,
              nqp::if(
                nqp::istype(
                  ($value := nqp::iterval(nqp::shift($iter)).Int),
                  Int
                ),
                nqp::if(             # a valid Int
                  $value > 0,
                  nqp::if(           # and a positive one at that
                    nqp::existskey(
                      elems,
                      (my $which := nqp::iterkey_s($iter).WHICH)
                    ),
                    nqp::stmts(      # seen before, add value
                      ($pair := nqp::atkey(elems,$which)),
                      nqp::bindattr(
                        $pair,
                        Pair,
                        '$!value',
                        nqp::getattr($pair,Pair,'$!value') + $value
                      )
                    ),
                    nqp::bindkey(    # new, create new Pair
                      elems,
                      $which,
                      Pair.new(nqp::iterkey_s($iter),$value)
                    )
                  )
                ),
                $value.throw         # huh?  let the world know
              )
            )
          )
        );
        elems
    }

    # Coerce the given Map to an IterationSet with baggy semantics.
    method COERCE-MAP-TO-BAG(\map) {
        nqp::if(
          (my $iter :=
            nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage'))),
          nqp::if(                   # something to coerce
            nqp::istype(map,Hash::Object),
            nqp::stmts(              # object hash
              # once object hashes have IterationSets inside them, we can
              # make this an nqp::clone for more performance, which would
              # pre-populate the IterationSet with the right keys off the
              # bat.
              (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
              nqp::while(
                $iter,
                nqp::if(
                  nqp::istype(
                    (my $value := nqp::getattr(
                      nqp::iterval(nqp::shift($iter)),Pair,'$!value'
                    ).Int),
                    Int
                  ),
                  nqp::if(           # a valid Int
                    $value > 0,
                    nqp::bindkey(    # and a positive one at that
                      $elems,
                      nqp::iterkey_s($iter),
                      nqp::p6bindattrinvres(
                        nqp::clone(nqp::iterval($iter)),
                        Pair,
                        '$!value',
                        $value
                      )
                    )
                  ),
                  $value.throw       # huh?  let the world know
                )
              ),
              $elems
            ),
            nqp::stmts(              # ordinary Map
              ($elems := nqp::create(Rakudo::Internals::IterationSet)),
              nqp::while(
                $iter,
                nqp::if(
                  nqp::istype(
                    ($value := nqp::iterval(nqp::shift($iter)).Int),
                    Int
                  ),
                  nqp::if(           # a valid Int
                    $value > 0,
                    nqp::bindkey(    # and a positive one at that
                      $elems,
                      nqp::iterkey_s($iter).WHICH,
                      Pair.new(nqp::iterkey_s($iter),$value)
                    )
                  ),
                  $value.throw       # huh?  let the world know
                )
              ),
              $elems
            )
          ),
          nqp::create(Rakudo::Internals::IterationSet)  # nothing to coerce
        )
    }

    # Add to given IterationSet with baggy semantics the values of the given
    # iterator while checking for Pairs with numeric values.
    method ADD-PAIRS-TO-BAG(\elems, Mu \iterator, Mu \type) {
        nqp::until(
          nqp::eqaddr(
            (my $pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::if(
            nqp::istype($pulled,Pair),
            nqp::if(               # we have a Pair
              nqp::istype(
                (my $value :=
                  nqp::decont(nqp::getattr($pulled,Pair,'$!value')).Int),
                Int
              ),
              nqp::if(             # is a (coerced) Int
                $value > 0,
                nqp::if(           # and a positive one at that
                  nqp::existskey(
                    elems,
                    (my $which := nqp::getattr($pulled,Pair,'$!key').WHICH)
                  ),
                  nqp::stmts(      # seen before, add value
                    (my $pair := nqp::atkey(elems,$which)),
                    nqp::bindattr(
                      $pair,
                      Pair,
                      '$!value',
                      nqp::getattr($pair,Pair,'$!value') + $value
                    )
                  ),
                  self.BIND-TO-TYPED-BAG(    # new, create new Pair
                    elems,
                    $which,
                    nqp::getattr($pulled,Pair,'$!key'),
                    $value,
                    type
                  )
                )
              ),
              $value.throw         # value cannot be made Int, so throw
            ),
            nqp::if(               # not a Pair
              nqp::existskey(
                elems,
                ($which := $pulled.WHICH)
              ),
              nqp::stmts(
                ($pair := nqp::atkey(elems,$which)),
                nqp::bindattr(     # seen before, so increment
                  $pair,
                  Pair,
                  '$!value',
                  nqp::getattr($pair,Pair,'$!value') + 1
                )
              ),
              self.BIND-TO-TYPED-BAG(    # new, create new Pair
                elems, $which, $pulled, 1, type
              )
            )
          )
        );
        elems                      # we're done, return what we got so far
    }

    # Add to given IterationSet with baggy semantics the values of the two
    # given iterators where the first iterator supplies objects, and the
    # second supplies values.
    method ADD-OBJECTS-VALUES-TO-BAG(
      \elems, Mu \objects, Mu \values, Mu \type
    ) is raw {
        nqp::until(
          nqp::eqaddr((my \object := objects.pull-one),IterationEnd),
          nqp::if(
            (my \value := values.pull-one.Int) > 0,
            self.BIND-TO-TYPED-BAG(    # new, create new Pair
              elems, object.WHICH, object, value, type
            )
          )
        );
        elems
    }

    # Take the given IterationSet with baggy semantics, and add the other
    # IterationSet with setty semantics to it.  Return the given IterationSet.
    method ADD-SET-TO-BAG(\elems, Mu \set) {
        nqp::if(
          set && nqp::elems(set),
          nqp::stmts(
            (my \iter := nqp::iterator(set)),
            nqp::while(
              iter,
              nqp::if(
                nqp::existskey(elems,nqp::iterkey_s(nqp::shift(iter))),
                nqp::stmts(
                  (my \pair := nqp::atkey(elems,nqp::iterkey_s(iter))),
                  nqp::bindattr(pair,Pair,'$!value',
                    nqp::getattr(pair,Pair,'$!value') + 1
                  )
                ),
                nqp::bindkey(elems,nqp::iterkey_s(iter),
                  Pair.new(nqp::iterval(iter), 1)
                )
              )
            )
          )
        );
        elems
    }

    method MULTIPLY-BAG-TO-BAG(\elems,Mu \bag) {
        my $iter := nqp::iterator(elems);

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
        );
        elems
    }

    method MULTIPLY-SET-TO-BAG(\elems,Mu \set) {
        my $iter := nqp::iterator(elems);

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
        );
        elems
    }

    # set difference Baggy IterSet from Bag IterSet, both assumed to have elems
    method SUB-BAGGY-FROM-BAG(\aelems, \belems) {
        my $elems := nqp::create(Rakudo::Internals::IterationSet);
        my $iter  := nqp::iterator(aelems);

        nqp::while(
          $iter,
          nqp::if(
            (my $value :=
              nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
               - nqp::getattr(
                   nqp::ifnull(nqp::atkey(belems,nqp::iterkey_s($iter)),$p0),
                   Pair,
                   '$!value'
                 )
            ) > 0,
            nqp::bindkey(
              $elems,
              nqp::iterkey_s($iter),
              nqp::p6bindattrinvres(
                nqp::clone(nqp::iterval($iter)),Pair,'$!value',$value
              )
            )
          )
        );
        $elems
    }

    # set difference Setty IterSet from Bag IterSet, both assumed to have elems
    method SUB-SETTY-FROM-BAG(\aelems, \belems) {
        my $elems := nqp::create(Rakudo::Internals::IterationSet);
        my $iter  := nqp::iterator(aelems);

        nqp::while(
          $iter,
          nqp::if(
            (my $value :=
              nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
               - nqp::existskey(belems,nqp::iterkey_s($iter))
            ) > 0,
            nqp::bindkey(
              $elems,
              nqp::iterkey_s($iter),
              nqp::p6bindattrinvres(
                nqp::clone(nqp::iterval($iter)),Pair,'$!value',$value
              )
            )
          )
        );
        $elems
    }

    # set difference of a Baggy and a QuantHash
    method DIFFERENCE-BAGGY-QUANTHASH(\a, \b) {
        nqp::if(
          (my $araw := a.RAW-HASH) && nqp::elems($araw),
          nqp::if(
            (my $braw := b.RAW-HASH) && nqp::elems($braw),
            nqp::create(a.WHAT).SET-SELF(
              nqp::if(
                nqp::istype(b,Setty),
                self.SUB-SETTY-FROM-BAG($araw, $braw),
                self.SUB-BAGGY-FROM-BAG($araw, $braw)
              )
            ),
            a
          ),
          nqp::if(
            nqp::istype(b,Failure),
            b.throw,
            a
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
        my      $rand := total.rand;
        my Real $seen := 0;
        my $iter := nqp::iterator(elems);

        nqp::while(
          $iter && (
            0 > (my $value :=                      # negative values ignored
              nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value'))
            || $rand > ($seen := $seen + $value)   # positive values add up
          ),
          nqp::null
        );

        $iter
    }

    # Given an IterationSet in baggy/mixy format considered to contain the
    # final result, add the other IterationSet using Mix semantics and return
    # the first IterationSet.
    method ADD-MIX-TO-MIX(\elems, Mu \mix) {
        nqp::if(
          mix && nqp::elems(mix),
          nqp::stmts(
            (my $iter := nqp::iterator(mix)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::isnull((my $pair :=
                  nqp::atkey(elems,nqp::iterkey_s(nqp::shift($iter)))
                )),
                nqp::bindkey(                 # doesn't exist on left, create
                  elems,
                  nqp::iterkey_s($iter),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($iter)),
                    Pair,
                    '$!value',
                    nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                  )
                ),
                nqp::if(                      # exists on left, update
                  (my $value := nqp::getattr($pair,Pair,'$!value')
                    + nqp::getattr(nqp::iterval($iter),Pair,'$!value')),
                  nqp::bindattr($pair,Pair,'$!value',$value), # valid for Mix
                  nqp::deletekey(elems,nqp::iterkey_s($iter)) # bye bye
                )
              )
            )
          )
        );
        elems
    }

    # Add to given IterationSet with mixy semantics the keys of given Map
    method ADD-MAP-TO-MIX(\elems, \map) {
        nqp::if(
          (my $iter :=
            nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage'))),
          nqp::if(
            nqp::istype(map,Hash::Object),
            nqp::while(              # object hash
              $iter,
              nqp::if(
                nqp::istype(
                  (my $value := nqp::getattr(
                    nqp::iterval(nqp::shift($iter)),Pair,'$!value'
                  ).Real),
                  Real
                ),
                nqp::if(             # a valid Real
                  $value,
                  nqp::if(           # and not 0
                    nqp::existskey(elems,nqp::iterkey_s($iter)),
                    nqp::if(         # seen before: add value, remove if sum 0
                      ($value := nqp::getattr(
                        (my $pair := nqp::atkey(elems,nqp::iterkey_s($iter))),
                        Pair,
                        '$!value'
                      ) + $value),
                      nqp::bindattr($pair,Pair,'$!value',$value), # okidoki
                      nqp::deletekey(elems,nqp::iterkey_s($iter)) # alas, bye
                    ),
                    nqp::bindkey(    # new, create new Pair
                      elems,
                      nqp::iterkey_s($iter),
                      nqp::p6bindattrinvres(
                        nqp::clone(nqp::iterval($iter)),
                        Pair,
                        '$!value',
                        nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                      )
                    )
                  )
                ),
                $value.throw         # huh?  let the world know
              )
            ),
            nqp::while(              # normal Map
              $iter,
              nqp::if(
                nqp::istype(
                  ($value := nqp::iterval(nqp::shift($iter)).Real),
                  Real
                ),
                nqp::if(             # a valid Real
                  $value,
                  nqp::if(           # and not 0
                    nqp::existskey(
                      elems,
                      (my $which := nqp::iterkey_s($iter).WHICH)
                    ),
                    nqp::if(         # seen before: add value, remove if sum 0
                      ($value := nqp::getattr(
                        ($pair := nqp::atkey(elems,$which)),
                        Pair,
                        '$!value'
                      ) + $value),
                      nqp::bindattr($pair,Pair,'$!value',$value), # okidoki
                      nqp::deletekey(elems,$which)                # alas, bye
                    ),
                    nqp::bindkey(    # new, create new Pair
                      elems,
                      $which,
                      Pair.new(nqp::iterkey_s($iter),$value)
                    )
                  )
                ),
                $value.throw         # huh?  let the world know
              )
            )
          )
        );
        elems
    }

    # bind the given which/object/value to the given IterationSet,
    # check object for given type
    method BIND-TO-TYPED-MIX(
      \elems, Mu \which, Mu \object, Real:D \value, Mu \type
    --> Nil) {
        nqp::istype(object,type)
          ?? nqp::bindkey(elems,which,Pair.new(object,value))
          !! X::TypeCheck::Binding.new(
               got      => object,
               expected => type
             ).throw
    }

    # Add to given IterationSet with mixy semantics the values of the given
    # iterator while checking for Pairs with numeric values.
    method ADD-PAIRS-TO-MIX(\elems, Mu \iterator, Mu \type) is raw {
        nqp::until(
          nqp::eqaddr(
            (my $pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::if(
            nqp::istype($pulled,Pair),
            nqp::if(               # got a Pair
              (my $value :=
                nqp::decont(nqp::getattr($pulled,Pair,'$!value'))),
              nqp::if(             # non-zero value
                nqp::istype($value,Num) && nqp::isnanorinf($value),
                X::OutOfRange.new( # NaN or -Inf or Inf, we're done
                  what  => 'Value',
                  got   => $value,
                  range => '-Inf^..^Inf'
                ).throw,
                nqp::stmts(        # apparently valid
                  nqp::unless(
                    nqp::istype(($value := $value.Real),Real),
                    $value.throw   # not a Real value, so throw Failure
                  ),
                  nqp::if(         # valid Real value
                    nqp::existskey(
                      elems,
                      (my $which := nqp::getattr($pulled,Pair,'$!key').WHICH)
                    ),
                    nqp::if( # seen before, add value
                      ($value := nqp::getattr(
                        (my $pair := nqp::atkey(elems,$which)),
                        Pair,
                        '$!value'
                      ) + $value),
                      nqp::bindattr($pair,Pair,'$!value',$value),  # non-zero
                      nqp::deletekey(elems,$which)                 # zero
                    ),
                    self.BIND-TO-TYPED-MIX(  # new, create new Pair
                      elems, $which,
                      nqp::getattr($pulled,Pair,'$!key'),
                      $value,type
                    )
                  )
                )
              )
            ),
            nqp::if(               # not a Pair
              nqp::existskey(
                elems,
                ($which := $pulled.WHICH)
              ),
              nqp::stmts(
                ($pair := nqp::atkey(elems,$which)),
                nqp::bindattr(     # seen before, so increment
                  $pair,
                  Pair,
                  '$!value',
                  nqp::getattr($pair,Pair,'$!value') + 1
                )
              ),
              self.BIND-TO-TYPED-MIX(  # new, create new Pair
                elems, $which, $pulled, 1, type
              )
            )
          )
        );
        elems                      # we're done, return what we got so far
    }

    # Add to given IterationSet with mixy semantics the values of the two
    # given iterators where the first iterator supplies objects, and the
    # second supplies values.
    method ADD-OBJECTS-VALUES-TO-MIX(
      \elems, Mu \objects, Mu \values, Mu \type
    ) is raw {
        nqp::until(
          nqp::eqaddr((my \object := objects.pull-one),IterationEnd),
          nqp::if(
            nqp::istype((my \value := values.pull-one),Num)
              && nqp::isnanorinf(value),
            X::OutOfRange.new( # NaN or -Inf or Inf, we're done
              what  => 'Value',
              got   => value,
              range => '-Inf^..^Inf'
            ).throw,
            nqp::if(
              nqp::istype(nqp::bind(value,value.Real),Real),
              nqp::if(
                value,
                self.BIND-TO-TYPED-MIX(
                  elems, object.WHICH, object, value, type
                )
              ),
              value.throw
            )
          )
        );
        elems
    }

    # Take the given IterationSet with mixy semantics, and add the other
    # IterationSet with setty semantics to it.  Return the given IterationSet.
    method ADD-SET-TO-MIX(\elems,Mu \set) {
        nqp::if(
          set && nqp::elems(set),
          nqp::stmts(
            (my $iter := nqp::iterator(set)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::existskey(elems,nqp::iterkey_s(nqp::shift($iter))),
                nqp::if(
                  (my $value := nqp::getattr(
                    (my $pair := nqp::atkey(elems,nqp::iterkey_s($iter))),
                    Pair,
                    '$!value'
                  ) + 1),
                  nqp::bindattr($pair,Pair,'$!value',$value),   # still valid
                  nqp::deletekey(elems,nqp::iterkey_s($iter))   # not, byebye
                ),
                nqp::bindkey(elems,nqp::iterkey_s($iter),       # new key
                  Pair.new(nqp::iterval($iter), 1)
                )
              )
            )
          )
        );
        elems
    }

    # Coerce the given Map to an IterationSet with mixy semantics.
    method COERCE-MAP-TO-MIX(\map) {
        nqp::if(
          (my $iter :=
            nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage'))),
          nqp::if(                   # something to coerce
            nqp::istype(map,Hash::Object),
            nqp::stmts(              # object hash
              # once object hashes have IterationSets inside them, we can
              # make this an nqp::clone for more performance, which would
              # pre-populate the IterationSet with the right keys off the
              # bat.
              (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
              nqp::while(
                $iter,
                nqp::if(
                  nqp::istype(
                    (my $value := nqp::getattr(
                      nqp::iterval(nqp::shift($iter)),Pair,'$!value'
                    ).Real),
                    Real
                  ),
                  nqp::if(           # a valid Real
                    $value,
                    nqp::bindkey(    # and not 0
                      $elems,
                      nqp::iterkey_s($iter),
                      nqp::p6bindattrinvres(
                        nqp::clone(nqp::iterval($iter)),
                        Pair,
                        '$!value',
                        $value
                      )
                    )
                  ),
                  $value.throw       # huh?  let the world know
                )
              ),
              $elems
            ),
            nqp::stmts(              # ordinary Map
              ($elems := nqp::create(Rakudo::Internals::IterationSet)),
              nqp::while(
                $iter,
                nqp::if(
                  nqp::istype(
                    ($value := nqp::iterval(nqp::shift($iter)).Real),
                    Real
                  ),
                  nqp::if(           # a valid Real
                    $value,
                    nqp::bindkey(    # and not 0
                      $elems,
                      nqp::iterkey_s($iter).WHICH,
                      Pair.new(nqp::iterkey_s($iter),$value)
                    )
                  ),
                  $value.throw       # huh?  let the world know
                )
              ),
              $elems
            )
          ),
          nqp::create(Rakudo::Internals::IterationSet) # nothing to coerce
        )
    }

    method MULTIPLY-MIX-TO-MIX(\elems,Mu \mix --> Nil) {
        my $iter := nqp::iterator(elems);

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
        );
    }
    method MIX-CLONE-ALL-POSITIVE(\elems) {
        my $iter := nqp::iterator(my $clone := nqp::clone(elems));

        nqp::while(
          $iter,
          nqp::stmts(
            nqp::shift($iter),
            nqp::bindkey(
              $clone,
              nqp::iterkey_s($iter),
              nqp::p6bindattrinvres(
                nqp::clone(nqp::iterval($iter)),
                Pair,
                '$!value',
                abs(nqp::getattr(nqp::iterval($iter),Pair,'$!value'))
              )
            )
          )
        );
        $clone
    }
    method MIX-ALL-POSITIVE(\elems) {
        my $iter := nqp::iterator(elems);

        nqp::while(
          $iter,
          nqp::unless(
            nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value') > 0,
            return False
          )
        );
        True
    }
    method MIX-ALL-NEGATIVE(\elems) {
        my $iter := nqp::iterator(elems);

        nqp::while(
          $iter,
          nqp::unless(
            nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value') < 0,
            return False
          )
        );
        True
    }

    method MIX-IS-EQUAL(\a,\b) {
        nqp::unless(
          nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
          nqp::stmts(                   # A and B not same object
            (my \araw := a.RAW-HASH),
            (my \braw := b.RAW-HASH),
            nqp::if(
              araw && braw,
              nqp::if(                  # A and B both allocated
                nqp::isne_i(nqp::elems(araw),nqp::elems(braw)),
                (return False),         # different number of elements
                nqp::stmts(             # same number of elements
                  (my \iter := nqp::iterator(araw)),
                  nqp::while(           # number of elems in B >= A
                    iter,
                    nqp::unless(
                      nqp::getattr(nqp::iterval(nqp::shift(iter)),Pair,'$!value')
                        ==              # value in A should equal to B
                      nqp::getattr(
                        nqp::ifnull(nqp::atkey(braw,nqp::iterkey_s(iter)),$p0),
                        Pair,
                        '$!value'
                      ),
                      return False      # not same weight
                    )
                  )
                )
              ),
              nqp::if(                  # A and B not both allocated
                (araw && nqp::elems(araw)) || (braw && nqp::elems(braw)),
                return False            # allocated side contains elements
              )
            )
          )
        );

        True
    }

    method MIX-IS-SUBSET($a,$b) {
        nqp::if(
          nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
          True,                     # X is always a subset of itself
          nqp::if(
            (my $araw := $a.RAW-HASH) && (my $iter := nqp::iterator($araw)),
            nqp::if(                # elems in A
              (my $braw := $b.RAW-HASH) && nqp::elems($braw),
              nqp::stmts(           # elems in A and B
                nqp::while(         # check all values in A with B
                  $iter,
                  nqp::unless(
                    nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
                      <=            # value in A should be less or equal than B
                    nqp::getattr(
                      nqp::ifnull(nqp::atkey($braw,nqp::iterkey_s($iter)),$p0),
                      Pair,
                      '$!value'
                    ),
                    return False
                  )
                ),

                ($iter := nqp::iterator($braw)),
                nqp::while(         # check all values in B with A
                  $iter,
                  nqp::unless(
                    nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
                      >=            # value in B should be more or equal than A
                    nqp::getattr(
                      nqp::ifnull(nqp::atkey($araw,nqp::iterkey_s($iter)),$p0),
                      Pair,
                      '$!value'
                    ),
                    return False
                  )
                ),
                True                # all checks worked out, so ok
              ),
              # nothing in B, all elems in A should be < 0
              Rakudo::QuantHash.MIX-ALL-NEGATIVE($araw)
            ),
            nqp::if(
              ($braw := $b.RAW-HASH) && nqp::elems($braw),
              # nothing in A, all elems in B should be >= 0
              Rakudo::QuantHash.MIX-ALL-POSITIVE($braw),
              True                  # nothing in A nor B
            )
          )
        )
    }

    # Return whether first Baggy is a proper subset of the second Baggy,
    # using Mixy semantics
    method MIX-IS-PROPER-SUBSET($a,$b) {
        nqp::if(
          nqp::eqaddr(nqp::decont($a),nqp::decont($b)),
          False,                    # X is never a true subset of itself
          nqp::if(
            (my $araw := $a.RAW-HASH) && (my $iter := nqp::iterator($araw)),
            nqp::if(                # elems in A
              (my $braw := $b.RAW-HASH) && nqp::elems($braw),
              nqp::stmts(           # elems in A and B
                (my int $less),
                nqp::while(         # check all values in A with B
                  $iter,
                  nqp::if(
                    (my $left := nqp::getattr(
                      nqp::iterval(nqp::shift($iter)),
                      Pair,
                      '$!value'
                    ))
                      >             # value in A should be <= than B
                    (my $right := nqp::getattr(
                      nqp::ifnull(nqp::atkey($braw,nqp::iterkey_s($iter)),$p0),
                      Pair,
                      '$!value'
                    )),
                    (return False), # too many on left, we're done
                    nqp::unless($less,$less = $left < $right)
                  )
                ),

                ($iter := nqp::iterator($braw)),
                nqp::while(         # check all values in B with A
                  $iter,
                  nqp::if(
                    ($left := nqp::getattr(
                      nqp::ifnull(
                        nqp::atkey($araw,nqp::iterkey_s(nqp::shift($iter))),
                        $p0
                      ),
                      Pair,
                      '$!value'
                    ))
                      >             # value in A should be <= than B
                    ($right := nqp::getattr(
                      nqp::iterval($iter),Pair,'$!value'
                    )),
                    (return False),
                    nqp::unless($less,$less = $left < $right)
                  )
                ),
                nqp::hllbool($less)  # all checks worked out so far
              ),
              # nothing in B, all elems in A should be < 0
              Rakudo::QuantHash.MIX-ALL-NEGATIVE($araw)
            ),
            nqp::if(                # nothing in A
              ($braw := $b.RAW-HASH) && nqp::elems($braw),
              # something in B, all elems in B should be > 0
              Rakudo::QuantHash.MIX-ALL-POSITIVE($braw),
              False                 # nothing in A nor B
            )
          )
        )
    }

    # set difference QuantHash IterSet from Mix IterSet, both assumed to have
    # elems.  3rd parameter is 1 for Setty, 0 for Baggy semantics
    method SUB-QUANTHASH-FROM-MIX(\aelems, \belems, \issetty) {
        my $elems := nqp::create(Rakudo::Internals::IterationSet);
        my $iter  := nqp::iterator(belems);

        nqp::while(                   # subtract all righthand keys
          $iter,
          nqp::bindkey(
            $elems,
            nqp::iterkey_s(nqp::shift($iter)),
            nqp::if(
              issetty,
              Pair.new(
                nqp::iterval($iter),
                nqp::getattr(
                  nqp::ifnull(nqp::atkey(aelems,nqp::iterkey_s($iter)),$p0),
                  Pair,
                  '$!value'
                ) - 1
              ),
              nqp::p6bindattrinvres(
                nqp::clone(nqp::iterval($iter)),
                Pair,
                '$!value',
                nqp::getattr(
                  nqp::ifnull(nqp::atkey(aelems,nqp::iterkey_s($iter)),$p0),
                  Pair,
                  '$!value'
                ) - nqp::getattr(nqp::iterval($iter),Pair,'$!value')
              )
            )
          )
        ),
        ($iter := nqp::iterator(aelems)),
        nqp::while(                   # vivify all untouched lefthand keys
          $iter,
          nqp::if(
            nqp::existskey($elems,nqp::iterkey_s(nqp::shift($iter))),
            nqp::unless(              # was touched
              nqp::getattr(
                nqp::atkey($elems,nqp::iterkey_s($iter)),
                Pair,
                '$!value'
              ),
              nqp::deletekey($elems,nqp::iterkey_s($iter)) # but no value
            ),
            nqp::bindkey(             # not touched, add it
              $elems,
              nqp::iterkey_s($iter),
              nqp::p6bindattrinvres(
                nqp::clone(nqp::iterval($iter)),
                Pair,
                '$!value',
                nqp::getattr(nqp::iterval($iter),Pair,'$!value')
              )
            )
          )
        );
        $elems
    }

    # set difference of a Mixy and a QuantHash
    method DIFFERENCE-MIXY-QUANTHASH(\a, \b) {
        nqp::if(
          (my $araw := a.RAW-HASH) && nqp::elems($araw),
          nqp::if(
            (my $braw := b.RAW-HASH) && nqp::elems($braw),
            nqp::create(a.WHAT).SET-SELF(
              self.SUB-QUANTHASH-FROM-MIX($araw, $braw, nqp::istype(b,Setty)),
            ),
            a
          ),
          nqp::if(
            nqp::istype(b,Failure),
            b.throw,
            nqp::if(
              ($braw := b.RAW-HASH) && nqp::elems($braw),
              nqp::stmts(
                (my $elems := nqp::clone($braw)),
                (my $iter  := nqp::iterator($braw)),
                nqp::while(
                  $iter,
                  nqp::bindkey(    # clone with negated value
                    $elems,
                    nqp::iterkey_s(nqp::shift($iter)),
                    nqp::p6bindattrinvres(
                      nqp::clone(nqp::iterval($iter)),
                      Pair,
                      '$!value',
                      - nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                    )
                  )
                ),
                nqp::create(a.WHAT).SET-SELF($elems)
              ),
              a
            )
          )
        )
    }
}

# vim: expandtab shiftwidth=4
