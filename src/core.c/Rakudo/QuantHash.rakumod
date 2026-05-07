my role Real { ... }
my class X::TypeCheck::Binding { ... }

# Basically a library of QuantHash related functionality that basically
# works on IterationSets.  Several reasons to make this a separate class:
#
# 1. A lot of QuantHash functionality is shared between the different
#    QuantHash classes: having them all live inside the QuantHash role
#    makes them only live once in bytecode, instead of all the classes
#    that consume the QuantHash role
# 2. All of the different permutations of interactions between different
#    classes of QuantHashes makes the dispatch specification rather
#    complicated: having optimized NQP code inbetween them does **not**
#    improve maintainability.  So all of the highly optimized code for
#    QuantHashses is supposed to live here.
my class Rakudo::QuantHash {

    # A Pair with the value 0.  Apparently it is too early in the setting
    # to actually instantiate a Pair at compile time, so we need to do
    # this at runtime.
    my $p0 := nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0);

    # Create the Callable for checking the type of a key value and
    # potentially coerce the value.  Callable is expected to throw
    # on a typecheck failure or a failure to coerce
    method MAKE-OBJECTIFIER(Mu \type) {
        nqp::eqaddr(type,Mu)
          ?? -> Mu $value { $value }                  # no check needed
          !! type.^archetypes.coercive
            ?? -> Mu $value {                         # actually coerce
                   my $result := type.^coerce($value);
                   nqp::istype($result,Failure)
                     ?? $result.throw
                     !! $result
               }
            !! -> Mu $value {                         # just typecheck
                   nqp::istype($value,type)
                     ?? $value
                     !! X::TypeCheck::Binding.new(
                          got      => $value,
                          expected => type
                        ).throw
               }
    }

    # Specialized role for .kv methods on QuantHashes: copied methods
    # from Quanty because of visibility issues wrt to $!elems and $!iter :-(
    our role Quanty-kv does Iterator {
        has $!elems;
        has $!iter;
        has $!on;

        method !SET-SELF(\quanthash) {
            nqp::if(
              nqp::elems($!elems := quanthash.RAW-HASH),
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

    # Specialized role for .pairs methods on QuantHashes
    our role Pairs does Iterator {
        has $!elems;
        has $!picked;

        method !SET-SELF(\elems,\count) {
            $!elems  := elems;
            $!picked := Rakudo::QuantHash.PICK-N(elems, count);
            self
        }
        method new(Mu \elems, \count) {
            (my $todo := Rakudo::QuantHash.TODO(count))
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
          nqp::shift($iter) && --$i,
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
          nqp::islt_i(++$i,$elems),
          nqp::bindpos_s($keys,$i,nqp::iterkey_s(nqp::shift($iter)))
        );

        my $picked := nqp::setelems(nqp::list_s,$count);
        $i = -1;

        nqp::while(
          nqp::islt_i(++$i,$count),
          nqp::stmts(
            nqp::bindpos_s($picked,$i,
              nqp::atpos_s($keys,(my int $pick = $elems.rand.floor))
            ),
            nqp::bindpos_s($keys,$pick,nqp::atpos_s($keys,--$elems))
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
        my $keys := nqp::list_s;
        my $iter := nqp::iterator(quanthash.RAW-HASH);

        nqp::while(
          $iter,
          nqp::push_s($keys,nqp::iterkey_s(nqp::shift($iter)))
        );

        $keys
    }

    # Return an nqp::list_s of all values of a QuantHash, mapped to a str
    method RAW-VALUES-MAP(\quanthash, &mapper) is raw {
        my $values := nqp::list_s;
        my $iter   := nqp::iterator(quanthash.RAW-HASH);

        nqp::while(
          $iter,
          nqp::push_s($values,mapper(nqp::iterval(nqp::shift($iter))))
        );

        $values
    }

    # Return an nqp::list_s of all keys in a Baggy with the weight
    # joined with a null-byte inbetween.
    method BAGGY-RAW-KEY-VALUES(\baggy) is raw {
        my $list := nqp::list_s;
        my $iter := nqp::iterator(baggy.RAW-HASH);

        nqp::while(
          $iter,
          nqp::stmts(
            nqp::shift($iter),
            nqp::push_s(
              $list,
              nqp::concat(
                nqp::iterkey_s($iter),
                nqp::concat('\0',nqp::iterval($iter).value.Str)
              )
            )
          )
        );

        $list
    }

    # Create intersection of 2 Setties and return an IterationSet with
    # Setty semantics
    method INTERSECT-SETTIES(\a, \b, &objectifier = a.OBJECTIFIER) {
        my $elems := nqp::create(Rakudo::Internals::IterationSet);

        nqp::if(
          nqp::elems(my $araw := a.RAW-HASH)
            && nqp::elems(my $braw := b.RAW-HASH),
          nqp::stmts(                       # both have elems
            nqp::if(
              nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
              nqp::stmts(                   # a smallest, iterate over it
                (my $iter := nqp::iterator($araw)),
                (my $base := $braw)
              ),
              nqp::stmts(                   # b smallest, iterate over that
                ($iter := nqp::iterator($braw)),
                ($base := $araw)
              )
            ),
            nqp::while(
              $iter,
              nqp::stmts(
                (my $object := objectifier(nqp::iterval(nqp::shift($iter)))),
                nqp::if(
                  nqp::existskey($base,my str $which = $object.WHICH),
                  nqp::bindkey($elems,$which,$object)
                )
              )
            )
          )
        );

        $elems
    }

    # Create intersection of 2 Baggies and return an IterationSet with
    # Baggy semantics
    method INTERSECT-BAGGIES(\a, \b, &objectifier = a.OBJECTIFIER) {
        my $elems := nqp::create(Rakudo::Internals::IterationSet);

        nqp::if(
          nqp::elems(my $araw := a.RAW-HASH)
            && nqp::elems(my $braw := b.RAW-HASH),
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
            nqp::while(
              $iter,
              nqp::stmts(
                (my $object := objectifier(
                  nqp::iterval(nqp::shift($iter)).key
                )),
                nqp::if(
                  (my $pair := nqp::atkey($base,my str $which = $object.WHICH)),
                  nqp::bindkey($elems,$which,nqp::clone(
                    nqp::if(
                      $pair.value < nqp::iterval($iter).value,
                      $pair,
                      nqp::iterval($iter)
                    )
                  ))
                )
              )
            )
          )
        );

        $elems
    }

    # Create a deep clone of the given IterSet with Baggy semantics
    method BAGGY-CLONE(\raw) {
        my $elems := nqp::clone(raw);
        my $iter  := nqp::iterator(raw);

        nqp::while(
          $iter,
          nqp::bindkey(
            $elems,
            nqp::iterkey_s(nqp::shift($iter)),
            nqp::clone(nqp::iterval($iter))
          )
        );

        $elems
    }

    # The DELETE-KEY logic is shared by BagHash/MixHash, so put the actual
    # logic here, reachable by private method call
    method BAGGY-DELETE-KEY(\elems, str $which) {
        nqp::if(
          (my $pair := nqp::atkey(elems,$which)),
          nqp::stmts(
            nqp::deletekey(elems,$which),
            $pair.value
          ),
          0
        )
    }

    # Provide basic .pairs iterator logic for mutable Baggies
    method BAGGY-MUTABLE-ITERATOR(\elems, &proxy) {
        my class Iterate does Iterator {
            has $!elems;
            has &!proxy;
            has $!keys;
            method SET-SELF(\elems, &proxy) {
                $!elems := elems;
                &!proxy := &proxy;
                $!keys  := Rakudo::Internals.IterationSet2keys(elems);
                self
            }

            method pull-one() is raw {
                nqp::elems($!keys)
                  ?? nqp::p6bindattrinvres(
                       nqp::clone(
                         nqp::atkey($!elems,(my $key := nqp::shift_s($!keys)))
                       ),
                       Pair,
                       '$!value',
                       &!proxy($key,$!elems)
                     )
                  !! IterationEnd
            }
            method push-all(\target --> IterationEnd) {
                my $elems := $!elems;
                my $keys  := $!keys;
                nqp::while(  # doesn't sink
                  nqp::elems($keys),
                  target.push(nqp::atkey($elems,nqp::shift_s($keys)))
                )
            }
        }

        nqp::create(Iterate).SET-SELF(elems, &proxy)
    }

    # Provide basic .kv iterator logic for mutable Baggies
    method BAGGY-MUTABLE-KV(\elems, &proxy) {
        my class KV does Iterator {
            has $!elems;
            has &!proxy;
            has $!keys;
            has str $!on;
            method SET-SELF(\elems, &proxy) {
                $!elems := elems;
                &!proxy := &proxy;
                $!keys  := Rakudo::Internals.IterationSet2keys(elems);
                self
            }
            method pull-one() is raw {
                nqp::if(
                  $!on,
                  nqp::stmts(
                    (my $container := &!proxy($!on,$!elems)),
                    ($!on = ""),
                    $container
                  ),
                  nqp::if(
                    nqp::elems($!keys),
                    nqp::atkey($!elems,($!on = nqp::shift_s($!keys))).key,
                    IterationEnd
                  )
                )
            }
            method push-all(\target --> IterationEnd) {
                my $elems := $!elems;
                my $keys  := $!keys;
                nqp::while(
                  nqp::elems($keys),
                  nqp::stmts(  # doesn't sink
                    (my $pair := nqp::atkey($elems,nqp::shift_s($keys))),
                    target.push(nqp::getattr($pair,Pair,'$!key')),
                    target.push(nqp::getattr($pair,Pair,'$!value'))
                  )
                )
            }
        }

        nqp::create(KV).SET-SELF(elems, &proxy)
    }

    # Provide basic .values iterator logic for mutable Baggies
    method BAGGY-MUTABLE-VALUES(\elems, &proxy) {
        my class Values does Iterator {
            has $!elems;
            has &!proxy;
            has $!keys;
            method SET-SELF(\elems, &proxy) {
                $!elems := elems;
                &!proxy := &proxy;
                $!keys  := Rakudo::Internals.IterationSet2keys(elems);
                self
            }

            method pull-one() is raw {
                nqp::elems($!keys)
                  ?? &!proxy(nqp::shift_s($!keys),$!elems)
                  !! IterationEnd
            }
            method push-all(\target --> IterationEnd) {
                my $elems := $!elems;
                my $keys  := $!keys;
                nqp::while(  # doesn't sink
                  nqp::elems($keys),
                  target.push(proxy(nqp::shift_s($keys),$elems))
                );
            }
        }

        nqp::create(Values).SET-SELF(elems, &proxy)
    }

    # Configurable logic for handling .deepmap on mutable Baggies
    method BAGGY-MUTABLE-DEEPMAP(\elems, $coercer, &mapper, &judger) {
        my $new  := nqp::create(Rakudo::Internals::IterationSet);
        my $iter := nqp::iterator(elems);

        nqp::while(
          $iter,
          nqp::stmts(
            (my $pair     := nqp::iterval(nqp::shift($iter))),
            (my $value     = $pair.value),  # must be stored in a container
            (my $returned := $coercer(mapper($value))),
            nqp::if(
              nqp::istype($returned,Failure),
              $returned.throw,
              nqp::stmts(
                nqp::if(
                  judger($returned),
                  nqp::bindkey(
                    $new,
                    nqp::iterkey_s($iter),
                    nqp::p6bindattrinvres(
                      nqp::clone($pair),Pair,'$!value',nqp::decont($returned)
                    )
                  )
                ),
                nqp::if(
                  judger($value),
                  nqp::bindattr($pair,Pair,'$!value',nqp::decont($value)),
                  nqp::deletekey(elems,nqp::iterkey_s($iter))
                )
              )
            )
          )
        );

        $new
    }

    # Return IterationSet with symmetric difference of 2 Baggies
    method BAGGY-SYMMETRIC-DIFFERENCE(\a, \b) {
        my $araw := a.RAW-HASH;
        my $braw := b.RAW-HASH;

        nqp::if(
          nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
          nqp::stmts(                        # a smallest, iterate over it
            (my $iter  := nqp::iterator($araw)),
            (my $elems := nqp::clone($braw))
          ),
          nqp::stmts(                        # b smallest, iterate over that
            ($iter  := nqp::iterator($braw)),
            ($elems := nqp::clone($araw))
          )
        );

        nqp::while(
          $iter,
          nqp::stmts(
            (my str $which = nqp::iterkey_s(nqp::shift($iter))),
            nqp::if(
              (my $value := nqp::iterval($iter).value
                - nqp::ifnull(nqp::atkey($elems,$which),$p0).value),
              nqp::bindkey(
                $elems,$which,Pair.new(nqp::iterval($iter).key, abs($value))
              ),
              nqp::deletekey($elems,$which)
            )
          )
        );

        # Unseen keys in mixes can have negative value, so make sure they
        # become positive
        nqp::if(
          nqp::istype(a,Mixy) && ($iter := nqp::iterator($elems)),
          nqp::while(
            $iter,
            nqp::iterval(nqp::shift($iter)).abs-value
          )
        );

        $elems
    }

#--- Set/SetHash related methods

    # Create an IterationSet with baggy semantics from IterationSet with
    # Setty semantics
    method SET-BAGGIFY(\raw) {
        my $elems := nqp::clone(raw);
        my $iter  := nqp::iterator($elems);

        nqp::while(
          $iter,
          nqp::bindkey(
            $elems,
            nqp::iterkey_s(nqp::shift($iter)),
            Pair.new(nqp::iterval($iter),1)
          )
        );

        $elems
    }

    # Add to given IterationSet with setty semantics the values of iterator
    method ADD-ITERATOR-TO-SET(\elems, Mu \iterator, &objectifier) {
        nqp::until(
          nqp::eqaddr(
            (my $pulled := iterator.pull-one),
            IterationEnd
          ),
          nqp::stmts(
            (my $object := objectifier($pulled)),
            nqp::bindkey(elems,$object.WHICH,$object)
          )
        );
        elems
    }

    # Add to IterationSet with setty semantics the values of the given
    # iterator while checking for Pairs (only include if value is trueish)
    method ADD-PAIRS-TO-SET(\elems, Mu \iterator, &objectifier) {
        my $object;

        nqp::until(
          nqp::eqaddr(
            (my $pulled := iterator.pull-one),
            IterationEnd
          ),
          nqp::if(
            nqp::istype($pulled,Pair),
            nqp::if(
              $pulled.value,
              nqp::stmts(
                ($object := objectifier($pulled.key)),
                nqp::bindkey(elems,$object.WHICH,$object)
              )
            ),
            nqp::stmts(
              ($object := objectifier($pulled)),
              nqp::bindkey(elems,$object.WHICH,$object)
            )
          )
        );

        elems
    }

    # Add to given IterationSet with setty semantics the values of the two
    # given iterators where the first iterator supplies objects, and the
    # second supplies values (only include if value is trueish).
    method ADD-OBJECTS-VALUES-TO-SET(
      \elems, Mu \objects, Mu \bools, &objectifier
    ) is raw {
        nqp::until(
          nqp::eqaddr((my $pulled := objects.pull-one),IterationEnd),
          nqp::if(
            bools.pull-one,
            nqp::stmts(
              (my $object := objectifier($pulled)),
              nqp::bindkey(elems,$object.WHICH,$object)
            )
          )
        );

        elems
    }

    # Add an object hash to the given IterationSet using
    # Setty semantics
    my sub ADD-OBJECT-HASH-TO-SET(\elems, Mu \iter, &objectifier) {
        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift(iter)).key)),
            nqp::if(
              nqp::iterval(iter).value,
              nqp::bindkey(elems,$object.WHICH,$object)
            )
          )
        );

        elems
    }

    # Add a generic Associative to the given IterationSet using
    # Setty semantics
    my sub ADD-ASSOCIATIVE-TO-SET(\elems, Mu \iter, &objectifier) {
        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterkey_s(nqp::shift(iter)))),
            nqp::if(
              nqp::iterval(iter),
              nqp::bindkey(elems,$object.WHICH,$object)
            )
          )
        );

        elems
    }

    # Add to given IterationSet with Setty semantics the keys of given Map
    method ADD-MAP-TO-SET(\elems, \map, &objectifier) {
        (nqp::istype(map,Hash::Object)
          ?? &ADD-OBJECT-HASH-TO-SET
          !! &ADD-ASSOCIATIVE-TO-SET
        )(elems,
          nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage')),
          &objectifier
        )
    }

    # Coerce a Map to an IterationSet with setty semantics
    method COERCE-MAP-TO-SET(\map) {
        Rakudo::QuantHash.ADD-MAP-TO-SET(
          nqp::create(Rakudo::Internals::IterationSet), map, Set.OBJECTIFIER
        )
    }

    # Remove set elements from set, stop when the result is the empty Set
    method SUB-SET-FROM-SET(\a, \b) {
        my &objectifier := a.OBJECTIFIER;
        my $elems       := nqp::clone(a.RAW-HASH);
        my $iter        := nqp::iterator(b.RAW-HASH);

        nqp::while(
          nqp::elems($elems) && $iter,
          nqp::deletekey(
            $elems,
            objectifier(nqp::iterval(nqp::shift($iter))).WHICH
          )
        );

        $elems
    }

    # Remove hash elements from Set, stop if the result is the empty Set
    method SUB-MAP-FROM-SET(\a, \map) {
        my &objectifier := a.OBJECTIFIER;
        my $elems       := nqp::clone(a.RAW-HASH);
        my $iter        := nqp::iterator(
          nqp::getattr(nqp::decont(map),Map,'$!storage')
        );
        my $object;

        nqp::if(
          nqp::istype(map,Hash::Object),
          nqp::while(  # object hash
            $iter && nqp::elems($elems),
            nqp::stmts(
              ($object := objectifier(nqp::iterval(nqp::shift($iter)).key)),
              nqp::if(
                nqp::iterval($iter).value,
                nqp::deletekey($elems,$object.WHICH)
              )
            )
          ),
          nqp::while(  # normal Associative
            $iter && nqp::elems($elems),
            nqp::stmts(
              ($object := objectifier(nqp::iterkey_s(nqp::shift($iter)))),
              nqp::if(
                nqp::iterval($iter),
                nqp::deletekey($elems,$object.WHICH)
              )
            )
          )
        );

        $elems
    }

    # Remove iterator elements from set using Pair semantics, stops pulling
    # from the iterator as soon as the result is the empty set.
    method SUB-PAIRS-FROM-SET(\elems, \iterator, &objectifier) {
        my $elems := nqp::clone(elems);

        nqp::until(
          nqp::eqaddr(                            # not at end of iterator
            (my $pulled := iterator.pull-one),
            IterationEnd
          ) || nqp::not_i(nqp::elems($elems)),    # still elems to remove
          nqp::if(
            nqp::istype($pulled,Pair),
            nqp::stmts(
              (my $object := objectifier($pulled.key)),
              nqp::if(                            # must check for thruthiness
                $pulled.value,
                nqp::deletekey($elems,$object.WHICH)
              )
            ),
            nqp::deletekey($elems,objectifier($pulled).WHICH)
          )
        );

        $elems
    }

    # Create a union of two QuantHashes with Setty semantics
    method SET-UNION(\a, \b, &objectifier = a.OBJECTIFIER) {
        my $elems := nqp::clone(a.RAW-HASH);
        my $iter  := nqp::iterator(b.RAW-HASH);

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift($iter)))),
            nqp::bindkey($elems,$object.WHICH,$object)
          )
        );

        $elems
    }

    # Returns 1 if all keys in the first IterationSet also occur in
    # the second IterationSet, else 0
    my sub KEYS-ALL-LEFT(\araw, \braw --> int) {
        my $iter := nqp::iterator(araw);

        my int $result = 1;
        nqp::while(
          $iter && ($result =
            nqp::existskey(braw,nqp::iterkey_s(nqp::shift($iter)))
          ),
          nqp::null
        );

        $result
    }

    # Return True if two Setties are identical using Setty semantics
    method SET-IS-EQUAL(\a, \b) {
        nqp::hllbool(
          nqp::iseq_i(
            nqp::elems(my $araw := a.RAW-HASH),
            nqp::elems(my $braw := b.RAW-HASH)
          ) && KEYS-ALL-LEFT($araw, $braw)
        )
    }

    # Return True if the first Setty is a subset of the second
    method SET-IS-SUBSET(\a, \b) {
        nqp::hllbool(
          nqp::isle_i(
            nqp::elems(my $araw := a.RAW-HASH),
            nqp::elems(my $braw := b.RAW-HASH)
          ) && KEYS-ALL-LEFT($araw, $braw)
        )
    }

    # Return True if the first Setty is a proper subset of the second
    method SET-IS-PROPER-SUBSET(\a, \b) {
        nqp::hllbool(
          nqp::islt_i(
            nqp::elems(my $araw := a.RAW-HASH),
            nqp::elems(my $braw := b.RAW-HASH)
          ) && KEYS-ALL-LEFT($araw, $braw)
        )
    }

    # Return IterationSet with symmetric difference of 2 Setties
    method SET-SYMMETRIC-DIFFERENCE(\a, \b) {
        my $araw := a.RAW-HASH;
        my $braw := b.RAW-HASH;

        nqp::if(
          nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
          nqp::stmts(                        # a smallest, iterate over it
            (my $iter  := nqp::iterator($araw)),
            (my $elems := nqp::clone($braw))
          ),
          nqp::stmts(                        # b smallest, iterate over that
            ($iter  := nqp::iterator($braw)),
            ($elems := nqp::clone($araw))
          )
        );

        nqp::while(
          $iter,
          nqp::stmts(
            (my str $which = nqp::iterkey_s(nqp::shift($iter))),
            nqp::if(                         # remove if in both
              nqp::existskey($elems,$which),
              nqp::deletekey($elems,$which),
              nqp::bindkey($elems,$which,nqp::iterval($iter))
            )
          )
        );

        $elems
    }

#--- Bag/BagHash related methods

    # Calculate total of value of a Bag(Hash).  Takes a (possibly
    # uninitialized) IterationSet in Bag format.
    method BAG-TOTAL(Mu \elems) {
        my     $iter := nqp::iterator(elems);
        my Int $total = 0;

        nqp::while(
          $iter,
          $total = $total + nqp::iterval(nqp::shift($iter)).value
        );

        $total
    }

    # Return random iterator item from a given Bag(Hash).  Takes an
    # initialized IterationSet with at least 1 element in Bag format,
    # and the total value of values in the Bag.
    method BAG-ROLL(\elems, \total) {
        my Int $rand := total.rand.Int;
        my Int $seen := 0;
        my     $iter := nqp::iterator(elems);

        nqp::while(
          $iter && nqp::isle_I(
            ($seen := nqp::add_I(
              $seen,
              nqp::iterval(nqp::shift($iter)).value,
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
        my $pair := nqp::iterval($iter);

        $pair.value == 1
          ?? nqp::deletekey(elems,nqp::iterkey_s($iter)) # going to 0, so remove
          !! $pair.add-value(-1);                        # just update

        $pair.key
    }

    # Create an IterationSet clone from a Baggy
    method BAGGY-CLONE-RAW(Mu \baggy) {
        my $elems := nqp::clone(baggy);
        my $iter  := nqp::iterator($elems);

        nqp::while(
          $iter,
          nqp::bindkey(
            $elems,
            nqp::iterkey_s(nqp::shift($iter)),
            nqp::clone(nqp::iterval($iter))
          )
        );

        $elems
    }

    # Add contents of a strict Baggy to the IterationSet of
    # another baggy, and return that
    method ADD-BAG-TO-BAG(\elems, Mu \bag, &objectifier) {
        my $iter := nqp::iterator(bag);

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift($iter)).key)),
            nqp::if(
              (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
              $pair.add-value(nqp::iterval($iter).value),
              nqp::bindkey(
                elems,
                $which,
                Pair.new($object,nqp::iterval($iter).value)
              )
            )
          )
        );

        elems
    }

    # Add objects of given iterator to given IterationSet using
    # Baggy/Mixy semantics
    method ADD-ITERATOR-TO-BAG(\elems, Mu \iterator, &objectifier) {
        nqp::until(
          nqp::eqaddr(
            (my $pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::stmts(
            (my $object := objectifier($pulled)),
            nqp::if(
              (my $pair := nqp::atkey(elems,(my str $which = $object.WHICH))),
              $pair.add-value(1),
              nqp::bindkey(elems,$which,Pair.new($object,1))
            )
          )
        );

        elems
    }

    # Remove the object from the given iterator from the IterationSet
    # using baggy semantics
    method SUB-ITERATOR-FROM-BAG(\elems, Mu \iterator, &objectifier) {
        nqp::until(
          nqp::eqaddr(
            (my $pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::if(
            (my $pair := nqp::atkey(
              elems,
              my str $which = objectifier($pulled).WHICH
            )) && $pair.value > 1,
            $pair.add-value(-1),
            nqp::deletekey(elems,$which)
          )
        );

        elems
    }

    # Add an object hash to the given IterationSet using
    # baggy (not mixy) semantics
    my sub ADD-OBJECT-HASH-TO-BAG(\elems, Mu \iter, &objectifier) {
        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift(iter)).key)),
            nqp::if(
              nqp::istype(
                (my $value := nqp::iterval(iter).value.Int),
                Failure
              ),
              $value.throw,                 # huh?  let the world know
              nqp::if(                      # a valid Int
                $value > 0,
                nqp::if(
                  (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
                  $pair.add-value($value),  # existing key
                  nqp::bindkey(elems,$which,Pair.new($object,$value))
                )
              ),
            )
          )
        );

        elems
    }

    # Add a generic Associative to the given IterationSet using
    # baggy (not mixy) semantics
    my sub ADD-ASSOCIATIVE-TO-BAG(\elems, Mu \iter, &objectifier) {
        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterkey_s(nqp::shift(iter)))),
            nqp::if(
              nqp::istype(
                (my $value := nqp::iterval(iter).Int),
                Failure
              ),
              $value.throw,                 # huh?  let the world know
              nqp::if(                      # a valid Int
                $value > 0,
                nqp::if(
                  (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
                  $pair.add-value($value),  # existing key
                  nqp::bindkey(elems,$which,Pair.new($object,$value))
                )
              ),
            )
          )
        );

        elems
    }

    # Add to given IterationSet with baggy semantics the keys of given Map
    method ADD-MAP-TO-BAG(\elems, \map, &objectifier) {
        (nqp::istype(map,Hash::Object)
          ?? &ADD-OBJECT-HASH-TO-BAG
          !! &ADD-ASSOCIATIVE-TO-BAG
        )(elems,
          nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage')),
          &objectifier
        );
    }

    # Coerce the given Map to an IterationSet with baggy semantics.
    method COERCE-MAP-TO-BAG(\map) {
        self.ADD-MAP-TO-BAG(
          nqp::create(Rakudo::Internals::IterationSet),
          map,
          Bag.OBJECTIFIER
        )
    }

    # Add to given IterationSet with Baggy (non Mixy) semantics the values
    # of the given iterator while checking for Pairs with numeric values.
    method ADD-PAIRS-TO-BAG(\elems, Mu \iterator, &objectifier) {
        my $object;
        my $value;

        nqp::until(
          nqp::eqaddr(
            (my $pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::stmts(
            nqp::if(
              nqp::istype($pulled,Pair),
              nqp::stmts(          # we have a Pair
                ($object := objectifier($pulled.key)),
                nqp::if(
                  nqp::istype(($value := $pulled.value.Int),Failure),
                  $value.throw     # value cannot be made Int, so throw
                )
              ),
              nqp::stmts(          # not a Pair
                ($object := objectifier($pulled)),
                ($value  := 1)
              )
            ),
            (my str $which = $object.WHICH),
            nqp::if(               # something to add
              $value > 0,
              nqp::if(
                (my $pair := nqp::atkey(elems,$which)),
                $pair.add-value($value),
                nqp::bindkey(elems,$which,Pair.new($object,$value))
              )
            )
          )
        );

        elems                      # we're done, return what we got so far
    }

    # Add to given IterationSet with strict Baggy semantics the values
    # of the two given iterators where the first iterator supplies objects,
    # and the second supplies values.
    method ADD-OBJECTS-VALUES-TO-BAG(
      \elems, Mu \objects, Mu \values, &objectifier
    ) is raw {
        nqp::until(
          nqp::eqaddr((my $pulled := objects.pull-one),IterationEnd),
          nqp::stmts(
            (my $object := objectifier($pulled)),
            nqp::if(
              nqp::istype((my $value := values.pull-one.Int),Int),
              nqp::if(
                $value > 0,
                nqp::if(
                  (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
                  $pair.add-value($value),
                  nqp::bindkey(elems,$which,Pair.new($object,$value))
                )
              ),
              $value.throw  # huh?
            )
          )
        );

        elems
    }

    # Take the given IterationSet with Baggy (non_Mixy) semantics, and
    # add the other IterationSet with setty semantics to it.  Return
    # the given IterationSet.
    method ADD-SET-TO-BAG(\elems, Mu \set, &objectifier) {
        my $iter := nqp::iterator(set);

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift($iter)))),
            nqp::if(
              (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
              $pair.add-value(1),
              nqp::bindkey(elems,$which,Pair.new($object,1))
            )
          )
        );

        elems
    }

    # Multiply values from two IterationSets with Baggy semantics
    # to a Baggy
    method MULTIPLY-BAG-TO-BAG(\left, \right) {
        my $elems := nqp::create(Rakudo::Internals::IterationSet);

        nqp::if(
          nqp::islt_i(nqp::elems(left),nqp::elems(right)),
          nqp::stmts(
            (my $a := left),
            (my $b := right)
          ),
          nqp::stmts(
            ($a := right),
            ($b := left)
          )
        );

        my $iter  := nqp::iterator($a);
        nqp::while(
          $iter,
          nqp::if(
            (my $pair := nqp::atkey($b,nqp::iterkey_s(nqp::shift($iter)))),
            nqp::bindkey(
              $elems,
              nqp::iterkey_s($iter),
              Pair.new(
                $pair.key,
                $pair.value * nqp::iterval($iter).value
              )
            )
          )
        );

        $elems
    }

    # Multiply two Setties to a Baggy
    method MULTIPLY-SET-TO-BAG(\a, \b) {
        my &objectifier := a.OBJECTIFIER;
        my $elems := nqp::create(Rakudo::Internals::IterationSet);
        my $araw  := a.RAW-HASH;
        my $braw  := b.RAW-HASH;

        nqp::if(
          nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
          nqp::stmts(
            (my $iter := nqp::iterator($araw)),
            (my $look := $braw)
          ),
          nqp::stmts(
            ($iter := nqp::iterator($braw)),
            ($look := $araw)
          )
        );

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift($iter)))),
            nqp::if(
              nqp::existskey($look,my str $which = $object.WHICH),
              nqp::bindkey($elems,$which,Pair.new($object,1))
            )
          )
        );

        $elems
    }

    # Set difference from a Setty to a strict Baggy
    my sub SUB-SETTY-FROM-BAG(\base, Mu \iter, &objectifier) {
        my $elems := nqp::clone(base);

        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift(iter)))),
            nqp::if(
              (my $pair := nqp::atkey(base,my str $which = $object.WHICH)),
              nqp::if(
                (my $value := $pair.value - 1) > 0,
                nqp::bindkey($elems,$which,Pair.new($object,$value)),
                nqp::deletekey($elems,$which)
              )
            )
          )
        );

        $elems
    }

    # Set difference from a Baggy to a strict Baggy
    my sub SUB-BAGGY-FROM-BAG(\base, Mu \iter, &objectifier) {
        my $elems := nqp::clone(base);

        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift(iter)).key)),
            nqp::if(
              (my $pair := nqp::atkey(base,my str $which = $object.WHICH)),
              nqp::if(
                (my $value := $pair.value - nqp::iterval(iter).value.Int) > 0,
                nqp::bindkey($elems,$which,Pair.new($object,$value)),
                nqp::deletekey($elems,$which)
              )
            )
          )
        );

        $elems
    }

    # Set difference of strict Baggy and a QuantHash
    method DIFFERENCE-BAGGY-QUANTHASH(\a, \b) {
        a.WHAT.SETUP(
          (nqp::istype(b,Setty)
            ?? &SUB-SETTY-FROM-BAG
            !! &SUB-BAGGY-FROM-BAG
          )(
            a.RAW-HASH, nqp::iterator(b.RAW-HASH), a.OBJECTIFIER
          )
        )
    }

    # Create a union of two QuantHashes with Baggy semantics
    method BAGGY-UNION(\a, \b, &objectifier = a.OBJECTIFIER) {
        my $elems := nqp::clone(a.RAW-HASH);
        my $iter  := nqp::iterator(b.RAW-HASH);

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object   := objectifier(nqp::iterval(nqp::shift($iter)).key)),
            (my str $which = $object.WHICH),
            nqp::bindkey(
              $elems,
              $which,
              Pair.new(
                $object,
                nqp::if(
                  (my $pair := nqp::atkey($elems,$which))
                    && $pair.value > nqp::iterval($iter).value,
                  $pair.value,
                  nqp::iterval($iter).value
                )
              )
            )
          )
        );

        $elems
    }

    # Return True if two Baggies are identical using Mix semantics
    method BAGGY-IS-EQUAL(\a, \b) {
        nqp::if(
          nqp::elems(my $araw := a.RAW-HASH)
            == nqp::elems(my $braw := b.RAW-HASH),
          nqp::stmts(
            (my $iter := nqp::iterator($araw)),
            nqp::while(
              $iter,
              nqp::unless(
                nqp::iterval(nqp::shift($iter)).value
                  == nqp::ifnull(
                       nqp::atkey($braw,nqp::iterkey_s($iter)),$p0
                     ).value,
                (return False)
              )
            ),
            True
          )
          # Don't need else clause, as the failing condition provides it
        )
    }

    # Take two IterationSets and return True if all keys on the left
    # have a value equal or less than that key on the right, using
    # Baggy semantics
    my sub BAGGY-LESS-OR-EQUAL(\araw, \braw) {
        my $iter   := nqp::iterator(araw);
        my $result := True;

        nqp::while(
          $iter
            && ($result := nqp::iterval(nqp::shift($iter)).value <=
                 nqp::ifnull(nqp::atkey(braw,nqp::iterkey_s($iter)),$p0).value
               ),
          nqp::null
        );

        $result
    }

    # Return True if the first Baggy is a proper subset from the second Baggy
    method BAG-IS-PROPER-SUBSET(\a, \b) {
        my $araw := a.RAW-HASH;
        my $braw := b.RAW-HASH;

        nqp::if(
          nqp::islt_i(nqp::elems($araw),nqp::elems($braw)),
          BAGGY-LESS-OR-EQUAL($araw, $braw),
          nqp::if(
            nqp::elems($araw) == nqp::elems($braw),
            nqp::stmts(
              (my $iter := nqp::iterator($araw)),
              (my $less := False),
              nqp::while(
                $iter
                  && (my $left := nqp::iterval(nqp::shift($iter)).value) <=
                     (my $right := nqp::ifnull(
                       nqp::atkey($braw,nqp::iterkey_s($iter)),$p0
                     ).value)
                  && nqp::unless($less,$less := $left < $right),
                nqp::null
              ),
              $less
            )
            # no else needed if the condition fails
          )
        )
    }

    # Return True if the first Baggy is a subset from the second Baggy
    method BAG-IS-SUBSET(\a, \b) {
        BAGGY-LESS-OR-EQUAL(a.RAW-HASH, b.RAW-HASH)
    }

#--- Mix/MixHash related methods

    # Calculate total of values of a Mix(Hash).  Takes an IterationSet
    # in Mix format.
    method MIX-TOTAL(Mu \elems) {
        my $total = 0;
        my $iter := nqp::iterator(elems);

        nqp::while(
          $iter,
          $total := $total + nqp::iterval(nqp::shift($iter)).value
        );

        $total
    }

    # Calculate total of positive value of a Mix(Hash).  Takes an
    # IterationSet in Mix format.
    method MIX-TOTAL-POSITIVE(Mu \elems) {
        my $total = 0;
        my $iter := nqp::iterator(elems);

        nqp::while(
          $iter,
          nqp::if(
            (my $value := nqp::iterval(nqp::shift($iter)).value) > 0,
            $total := $total + $value
          )
        );

        $total
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
            0 >                                    # negative values ignored
              (my $value := nqp::iterval(nqp::shift($iter)).value)
            || $rand > ($seen := $seen + $value)   # positive values add up
          ),
          nqp::null
        );

        $iter
    }

    # Given an IterationSet in Baggy format considered to contain the
    # final result, add the other IterationSet using Mix semantics
    # and return the first IterationSet.
    method ADD-BAGGY-TO-MIX(\elems, Mu \mix, &objectifier) {
        my $iter := nqp::iterator(mix);

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift($iter)).key)),
            nqp::if(
              (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
              nqp::unless(
                $pair.add-value(nqp::iterval($iter).value),
                nqp::deletekey(elems,$which)  # adding nulled the value
              ),
              nqp::bindkey(elems,$which,nqp::clone(nqp::iterval($iter)))
            )
          )
        );

        elems
    }

    # Helper sub to throw out of range error
    my sub MIX-VALUE-OOR($value) {
        X::OutOfRange.new( # NaN or -Inf or Inf, we're done
          what  => 'Value',
          got   => $value,
          range => '-Inf^..^Inf'
        ).throw
    }

    # Add to given IterationSet with mixy semantics the values of the given
    # iterator while checking for Pairs with numeric values.
    method ADD-PAIRS-TO-MIX(\elems, Mu \iterator, &objectifier) is raw {
        my     $object;
        my     $pair;
        my str $which;

        nqp::until(
          nqp::eqaddr(
            (my $pulled := nqp::decont(iterator.pull-one)),
            IterationEnd
          ),
          nqp::if(
            nqp::istype($pulled,Pair),
            nqp::stmts(                             # got a Pair
              ($object := objectifier($pulled.key)),
              (my $value := $pulled.value),
              nqp::if(                              # non-zero value
                nqp::istype($value,Num) && nqp::isnanorinf($value),
                MIX-VALUE-OOR($value),              # alas
                nqp::if(                            # apparently valid
                  nqp::istype((my $real := $value.Real),Failure),
                  $real.throw,
                  nqp::if(                          # valid Real value
                    ($pair := nqp::atkey(elems,$which = $object.WHICH)),
                    nqp::unless(                    # seen before, add value
                      $pair.add-value($real),
                      nqp::deletekey(elems,$which)  # zero, so remove
                    ),
                    nqp::unless(                    # not seen before
                      $real == 0,
                      nqp::bindkey(elems,$which,Pair.new($object,$real))
                    )
                  )
                )
              )
            ),
            nqp::stmts(                             # not a Pair
              ($object := objectifier($pulled)),
              nqp::if(
                ($pair := nqp::atkey(elems,$which = $object.WHICH)),
                $pair.add-value(1),
                nqp::bindkey(elems,$which,Pair.new($object,1))
              )
            )
          )
        );

        elems
    }

    # Add to given IterationSet with mixy semantics the values of the two
    # given iterators where the first iterator supplies objects, and the
    # second supplies values.
    method ADD-OBJECTS-VALUES-TO-MIX(
      \elems, Mu \objects, Mu \values, &objectifier
    ) is raw {
        my $object;
        my $value;

        nqp::until(
          nqp::eqaddr(($object := objects.pull-one),IterationEnd),
          nqp::stmts(
            ($object := objectifier($object)),
            ($value  := values.pull-one),
            nqp::if(
              nqp::istype($value,Num) && nqp::isnanorinf($value),
              MIX-VALUE-OOR($value),
              nqp::if(
                nqp::istype((my $real := $value.Real),Failure),
                $real.throw,   # huh, tell the world
                nqp::if(
                  (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
                  nqp::unless(
                    $pair.add-value($real),
                    nqp::deletekey(elems,$which)
                  ),
                  nqp::if(
                    $value,
                    nqp::bindkey(elems,$which,Pair.new($object,$value))
                  )
                ),
              )
            )
          )
        );

        elems
    }

    # Take the given IterationSet with mixy semantics, and add the other
    # IterationSet with setty semantics to it.  Return the given IterationSet.
    method ADD-SET-TO-MIX(\elems,Mu \set, &objectifier) {
        my $iter := nqp::iterator(set);

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift($iter)))),
            nqp::if(
              (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
              nqp::unless(
                $pair.add-value(1),
                nqp::deletekey(elems,$which)
              ),
              nqp::bindkey(elems,$which,Pair.new($object,1))
            )
          )
        );

        elems
    }

    # Add an object hash to the given IterationSet using Mixy semantics
    my sub ADD-OBJECT-HASH-TO-MIX(\elems, Mu \iter, &objectifier) {
        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift(iter)).key)),
            nqp::if(
              nqp::istype(
                (my $value := nqp::iterval(iter).value.Real),
                Failure
              ),
              $value.throw,                       # huh?  let the world know
              nqp::if(
                nqp::istype($value,Num) && nqp::isnanorinf($value),
                MIX-VALUE-OOR($value),            # alas
                nqp::if(                          # a valid Real
                  (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
                  nqp::unless(                    # existing key
                    $pair.add-value($value),
                    nqp::deletekey(elems,$which)  # no value left, so remove
                  ),
                  nqp::if(
                    $value,
                    nqp::bindkey(elems,$which,Pair.new($object,$value))
                  )
                )
              ),
            )
          )
        );

        elems
    }

    # Add a generic Associative to the given IterationSet using
    # baggy (not mixy) semantics
    my sub ADD-ASSOCIATIVE-TO-MIX(\elems, Mu \iter, &objectifier) {
        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterkey_s(nqp::shift(iter)))),
            nqp::if(
              nqp::istype((my $value := nqp::iterval(iter).Real),Failure),
              $value.throw,                       # huh?  let the world know
              nqp::if(                            # a valid Real
                nqp::istype($value,Num) && nqp::isnanorinf($value),
                MIX-VALUE-OOR($value),            # alas
                nqp::if(
                  (my $pair := nqp::atkey(elems,my str $which = $object.WHICH)),
                  nqp::unless(
                    $pair.add-value($value),       # existing key
                    nqp::deletekey(elems,$which)   # no value left, so remove
                  ),
                  nqp::if(
                    $value,
                    nqp::bindkey(elems,$which,Pair.new($object,$value))
                  )
                )
              ),
            )
          )
        );

        elems
    }

    # Add to given IterationSet with baggy semantics the keys of given Map
    method ADD-MAP-TO-MIX(\elems, \map, &objectifier) {
        (nqp::istype(map,Hash::Object)
          ?? &ADD-OBJECT-HASH-TO-MIX
          !! &ADD-ASSOCIATIVE-TO-MIX
        )(elems,
          nqp::iterator(nqp::getattr(nqp::decont(map),Map,'$!storage')),
          &objectifier
        );
    }

    # Coerce the given Map to an IterationSet with baggy semantics.
    method COERCE-MAP-TO-MIX(\map) {
        self.ADD-MAP-TO-MIX(
          nqp::create(Rakudo::Internals::IterationSet),
          map,
          self.MAKE-OBJECTIFIER(Mu)
        )
    }

    # Multiply two IterationSets using Mixy semantics, produce a new one
    method MULTIPLY-MIX-TO-MIX(\araw, \braw, &objectifier) {
        my $elems := nqp::create(Rakudo::Internals::IterationSet);

        nqp::if(
          nqp::islt_i(nqp::elems(araw),nqp::elems(braw)),
          nqp::stmts(                    # $a smallest, iterate over it
            (my $iter := nqp::iterator(araw)),
            (my $base := braw)
          ),
          nqp::stmts(                    # $b smallest, iterate over that
            ($iter := nqp::iterator(braw)),
            ($base := araw)
          )
        );

        nqp::while(
          $iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift($iter)).key)),
            nqp::if(
              (my $pair := nqp::atkey($base,my str $which = $object.WHICH)),
              nqp::bindkey(
                $elems,
                $which,
                Pair.new($object,$pair.value * nqp::iterval($iter).value)
              )
            )
          )
        );

        $elems
    }

    # Create a IterationSet clone with Mixy semantics where all values
    # have become positive
    method MIX-CLONE-ALL-POSITIVE(\raw) {
        my $iter := nqp::iterator(my $elems := nqp::clone(raw));

        nqp::while(
          $iter,
          nqp::stmts(
            nqp::shift($iter),
            nqp::bindkey(
              $elems,
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

        $elems
    }

    # Helper subs for mix subset logic
    my sub MIX-ALL-POSITIVE(\elems) {
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
    my sub MIX-ALL-NEGATIVE(\elems) {
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

    # Compare two IterationSets with the given comparator with Mixy semantics
    my sub MIXY-COMPARE(\iterable, &comparator, \raw) {
        my \iter   := nqp::iterator(iterable);
        my $result := True;

        nqp::while(
          iter && ($result :=
            comparator(
              nqp::iterval(nqp::shift(iter)).value,
              nqp::ifnull(nqp::atkey(raw,nqp::iterkey_s(iter)),$p0).value
            )
          ),
          nqp::null
        );

        $result
    }

    # Return True if the first Mix is a subset of the second Mix
    method MIX-IS-SUBSET($a,$b) {
        my $araw := $a.RAW-HASH;
        my $braw := $b.RAW-HASH;

        MIXY-COMPARE($araw, &[<=], $braw)
          && MIXY-COMPARE($braw, &[>=], $araw)
    }

    # Return True if the first Baggy is a proper subset of the second Baggy,
    # using Mixy semantics
    method MIX-IS-PROPER-SUBSET($a,$b) {
        my $araw := $a.RAW-HASH;
        my $braw := $b.RAW-HASH;

        nqp::if(
          nqp::elems($araw),
          nqp::if(                # elems in A
            nqp::elems($braw),
            nqp::stmts(           # elems in A and B
              (my $iter := nqp::iterator($araw)),
              (my int $less),
              nqp::while(         # check all values in A with B
                $iter,
                nqp::if(
                  (my $left := nqp::iterval(nqp::shift($iter)).value)
                    >             # value in A should be <= than B
                  (my $right := nqp::ifnull(
                    nqp::atkey($braw,nqp::iterkey_s($iter)),$p0
                  ).value),
                  (return False), # too many on left, we're done
                  nqp::unless($less,$less = $left < $right)
                )
              ),

              ($iter := nqp::iterator($braw)),
              nqp::while(         # check all values in B with A
                $iter,
                nqp::if(
                  ($left := nqp::ifnull(
                   nqp::atkey($araw,nqp::iterkey_s(nqp::shift($iter))),$p0
                  ).value)
                    >             # value in A should be <= than B
                  ($right := nqp::iterval($iter).value),
                  (return False),
                  nqp::unless($less,$less = $left < $right)
                )
              ),
              nqp::hllbool($less)  # all checks worked out so far
            ),
            # nothing in B, all elems in A should be < 0
            MIX-ALL-NEGATIVE($araw)
          ),
          nqp::if(                # nothing in A
            nqp::elems($braw),
            # something in B, all elems in B should be > 0
            MIX-ALL-POSITIVE($braw),
            False                 # nothing in A nor B
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
        );
        $iter := nqp::iterator(aelems);
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

    # Set difference from a Setty to a Mixy
    my sub SUB-SETTY-FROM-MIX(\base, Mu \iter, &objectifier) {
        my $elems := nqp::clone(base);

        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift(iter)))),
            nqp::if(
              (my $pair := nqp::atkey($elems,my str $which = $object.WHICH)),
              nqp::unless(
                $pair.add-value(-1),
                nqp::deletekey($elems,$which)
              ),
              nqp::bindkey($elems,$which,Pair.new($object,-1))
            )
          )
        );

        $elems
    }

    # Set difference from a Baggy to a Mixy
    my sub SUB-BAGGY-FROM-MIX(\base, Mu \iter, &objectifier) {
        my $elems := nqp::clone(base);

        nqp::while(
          iter,
          nqp::stmts(
            (my $object := objectifier(nqp::iterval(nqp::shift(iter)).key)),
            nqp::if(
              (my $value := nqp::iterval(iter).value),
              nqp::if(
                (my $pair := nqp::atkey($elems,my str $which = $object.WHICH)),
                nqp::unless(
                  $pair.add-value(-$value),
                  nqp::deletekey($elems,$which)
                ),
                nqp::bindkey($elems,$which,Pair.new($object,-$value))
              )
            )
          )
        );

        $elems
    }

    # Set difference of Mixy and a QuantHash
    method DIFFERENCE-MIXY-QUANTHASH(\a, \b) {
        a.WHAT.SETUP(
          (nqp::istype(b,Setty)
            ?? &SUB-SETTY-FROM-MIX
            !! &SUB-BAGGY-FROM-MIX
          )(
            a.RAW-HASH, nqp::iterator(b.RAW-HASH), a.OBJECTIFIER
          )
        )
    }
}

# vim: expandtab shiftwidth=4
