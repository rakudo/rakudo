my role Baggy does QuantHash {

# A Bag/BagHash/Mix/MixHash consists of a single hash with Pairs.
# The keys of the hash, are the .WHICH strings of the original object key.
# The values are Pairs containing the original object key and value.

    has %!elems; # key.WHICH => (key,value)

# The Baggy role takes care of all mutable and immutable aspects that are
# shared between Bag,BagHash,Mix,MixHash.  Any specific behaviour for
# mutable and immutable aspects of Mix/MixHash need to live in Mixy.
# Immutables aspects of Bag/Mix, need to live to Bag/Mix respectively.

#--- interface methods
    method SET-SELF(Baggy:D: \elems) {
        nqp::stmts(
          nqp::if(
            nqp::elems(elems),
            # need to have allocated %!elems
            nqp::bindattr(%!elems,Map,'$!storage',elems),
          ),
          self
        )
    }
    multi method ACCEPTS(Baggy:U: \other --> Bool:D) {
        other.^does(self)
    }
    multi method ACCEPTS(Baggy:D: Baggy:D \other --> Bool:D) {
        nqp::p6bool(
          nqp::unless(
            nqp::eqaddr(self,other),
            nqp::if(                         # not same object
              (my $araw := self.RAW-HASH),
              nqp::if(                       # something on left
                (my $braw := other.RAW-HASH),
                nqp::if(                     # something on both sides
                  nqp::iseq_i(nqp::elems($araw),nqp::elems($braw)),
                  nqp::stmts(                # same size
                    (my $iter := nqp::iterator($araw)),
                    nqp::while(
                      $iter,
                      nqp::unless(
                        nqp::getattr(
                          nqp::ifnull(
                            nqp::atkey($braw,nqp::iterkey_s(nqp::shift($iter))),
                            BEGIN nqp::p6bindattrinvres(  # virtual Pair with 0
                              nqp::create(Pair),Pair,'$!value',0)
                          ),Pair,'$!value')
                          == nqp::getattr(nqp::iterval($iter),Pair,'$!value'),
                        return False         # missing/different: we're done
                      )
                    ),
                    True                     # all keys identical/same value
                  )
                )
              ),
              # true -> both empty
              nqp::isfalse(
                ($braw := other.RAW-HASH) && nqp::elems($braw)
              )
            )
          )
        )
    }
    multi method ACCEPTS(Baggy:D: Mu \other --> Bool:D) {
        self.ACCEPTS(other.Bag)
    }

    multi method AT-KEY(Baggy:D: \k) {  # exception: ro version for Bag/Mix
        nqp::if(
          (my $raw := self.RAW-HASH),
          nqp::getattr(
            nqp::ifnull(
              nqp::atkey($raw,k.WHICH),
              BEGIN nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0)
            ),
            Pair,
            '$!value'
          ),
          0
        )
    }
    multi method DELETE-KEY(Baggy:D: \k) {
        nqp::if(
          (my $raw := self.RAW-HASH)
            && nqp::existskey($raw,(my $which := k.WHICH)),
          nqp::stmts(
            (my $value :=
              nqp::getattr(nqp::atkey($raw,$which),Pair,'$!value')),
            nqp::deletekey($raw,$which),
            $value
          ),
          0
        )
    }
    multi method EXISTS-KEY(Baggy:D: \k) {
        nqp::p6bool(
          (my $raw := self.RAW-HASH) && nqp::existskey($raw,k.WHICH)
        )
    }

#--- object creation methods

    # helper sub to create Bag from iterator, check for laziness
    sub create-from-iterator(\type, \iterator --> Baggy:D) {
        nqp::if(
          iterator.is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(type.^name))),
          nqp::create(type).SET-SELF(
            Rakudo::QuantHash.ADD-ITERATOR-TO-BAG(
              nqp::create(Rakudo::Internals::IterationSet), iterator
            )
          )
        )
    }

    multi method new(Baggy:_: --> Baggy:D) { nqp::create(self) }
    multi method new(Baggy:_: \value --> Baggy:D) {
        nqp::if(
          nqp::istype(value,Iterable) && nqp::not_i(nqp::iscont(value)),
          create-from-iterator(self, value.iterator),
          nqp::stmts(
            nqp::bindkey(
              (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
              value.WHICH,
              Pair.new(value,1)
            ),
            nqp::create(self).SET-SELF($elems)
          )
        )
    }
    multi method new(Baggy:_: **@args) {
        create-from-iterator(self, @args.iterator)
    }

    method new-from-pairs(Baggy:_: *@pairs --> Baggy:D) {
        nqp::if(
          (my $iterator := @pairs.iterator).is-lazy,
          Failure.new(X::Cannot::Lazy.new(:action<coerce>,:what(self.^name))),
          nqp::create(self).SET-SELF(
            Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
              nqp::create(Rakudo::Internals::IterationSet),$iterator
            )
          )
        )
    }

#--- iterator methods
    multi method iterator(Baggy:D:) {
        Rakudo::Iterator.Mappy-values(%!elems)
    }
    multi method keys(Baggy:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
                $!iter
                  ?? nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
                  !! IterationEnd
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(
                    nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
                  )
                )
            }
        }.new(%!elems))
    }
    multi method kv(Baggy:D:) {
        Seq.new(Rakudo::Iterator.Mappy-kv-from-pairs(%!elems))
    }
    multi method values(Baggy:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() is raw {
                nqp::if(
                  $!iter,
                  nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!value'),
                  IterationEnd
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(
                    nqp::getattr(
                      nqp::iterval(nqp::shift($!iter)),
                      Pair,
                      '$!value'
                    )
                  )
                )
            }
        }.new(%!elems))
    }
    multi method antipairs(Baggy:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
                nqp::if(
                  $!iter,
                  nqp::iterval(nqp::shift($!iter)).antipair,
                  IterationEnd
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(
                  $!iter,
                  $target.push(nqp::iterval(nqp::shift($!iter)).antipair),
                )
            }
        }.new(%!elems))
    }
    proto method kxxv(|) { * }
    multi method kxxv(Baggy:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            has Mu $!key;
            has int $!times;

            method pull-one() is raw {
                nqp::if(
                  $!times,
                  nqp::stmts(
                    ($!times = nqp::sub_i($!times,1)),
                    $!key
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!key := nqp::getattr(
                        (my $pair := nqp::iterval(nqp::shift($!iter))),
                        Pair,
                        '$!key'
                      )),
                      ($!times =
                        nqp::sub_i(nqp::getattr($pair,Pair,'$!value'),1)),
                      $!key
                    ),
                    IterationEnd
                  )
                )
            }
            method skip-one() { # the default skip-one, too difficult to handle
                nqp::not_i(nqp::eqaddr(self.pull-one,IterationEnd))
            }
            method push-all($target --> IterationEnd) {
                nqp::while(
                  $!iter,
                  nqp::stmts(
                    ($!key := nqp::getattr(
                      (my $pair := nqp::iterval(nqp::shift($!iter))),
                      Pair,
                      '$!key'
                    )),
                    ($!times =
                      nqp::add_i(nqp::getattr($pair,Pair,'$!value'),1)),
                    nqp::while(  # doesn't sink
                      ($!times = nqp::sub_i($!times,1)),
                      $target.push($!key)
                    )
                  )
                )
            }
        }.new(%!elems))
    }
    multi method invert(Baggy:D:) {
        Seq.new(Rakudo::Iterator.Invert(%!elems.values.iterator))
    }

#--- introspection methods
    multi method elems(Baggy:D: --> Int:D) { %!elems.elems }
    multi method Bool(Baggy:D: --> Bool:D) { %!elems.Bool }

    method HASHIFY(\type) {
        nqp::stmts(
          (my $hash := Hash.^parameterize(type,Any).new),
          (my $descriptor := nqp::getattr($hash,Hash,'$!descriptor')),
          nqp::if(
            (my $raw := self.RAW-HASH) && nqp::elems($raw),
            nqp::stmts(
              (my $storage := nqp::clone($raw)),
              (my $iter := nqp::iterator($storage)),
              nqp::while(
                $iter,
                nqp::bindkey(
                  $storage,
                  nqp::iterkey_s(nqp::shift($iter)),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($iter)),
                    Pair,
                    '$!value',
                    (nqp::p6scalarfromdesc($descriptor) =
                      nqp::getattr(nqp::iterval($iter),Pair,'$!value'))
                  )
                )
              ),
              nqp::bindattr($hash,Map,'$!storage',$storage)
            )
          ),
          $hash
        )
    }
    multi method hash(Baggy:D: --> Hash:D) { self.HASHIFY(Any) }
    multi method Hash(Baggy:D: --> Hash:D) { self.HASHIFY(UInt) }

    method default(Baggy:D: --> 0) { }

    multi method Str(Baggy:D: --> Str:D) {
        nqp::join(' ',Rakudo::QuantHash.RAW-VALUES-MAP(self, { 
            nqp::if(
              (my $value := nqp::getattr($_,Pair,'$!value')) == 1,
              nqp::getattr($_,Pair,'$!key').gist,
              "{nqp::getattr($_,Pair,'$!key').gist}($value)"
            )
        }))
    }
    multi method gist(Baggy:D: --> Str:D) {
        nqp::concat(
          nqp::concat(
            nqp::concat(self.^name,'('),
            nqp::join(', ',
              Rakudo::Sorting.MERGESORT-str(
                Rakudo::QuantHash.RAW-VALUES-MAP(self, { 
                    nqp::if(
                      (my $value := nqp::getattr($_,Pair,'$!value')) == 1,
                      nqp::getattr($_,Pair,'$!key').gist,
                      "{nqp::getattr($_,Pair,'$!key').gist}($value)"
                    )
                })
              )
            )
          ),
          ')',
        )
    }
    multi method perl(Baggy:D: --> Str:D) {
        nqp::if(
          (my $raw := self.RAW-HASH) && nqp::elems($raw),
          nqp::concat(
            nqp::concat(
              '(',
              nqp::join(',',
                Rakudo::QuantHash.RAW-VALUES-MAP(self, { 
                    nqp::if(
                      (my $value := nqp::getattr($_,Pair,'$!value')) == 1,
                      nqp::getattr($_,Pair,'$!key').perl,
                      "{nqp::getattr($_,Pair,'$!key').perl}=>$value"
                    )
                })
              )
            ),
            nqp::concat(').',self.^name)
          ),
          nqp::if(
            nqp::istype(self,Bag),
            'bag()',
            nqp::if(
              nqp::istype(self,Mix),
              'mix()',
              nqp::concat('().',self.^name)
            )
          )
        )
    }

#--- selection methods
    proto method grabpairs (|) { * }
    multi method grabpairs(Baggy:D:) {
        nqp::if(
          (my $raw := self.RAW-HASH) && nqp::elems($raw),
          nqp::stmts(
            (my $iter := Rakudo::QuantHash.ROLL($raw)),
            (my $pair := nqp::iterval($iter)),
            nqp::deletekey($raw,nqp::iterkey_s($iter)),
            $pair
          ),
          Nil
        )
    }
    multi method grabpairs(Baggy:D: Callable:D $calculate) {
        self.grabpairs( $calculate(self.elems) )
    }
    multi method grabpairs(Baggy:D: Whatever $) {
        self.grabpairs(Inf)
    }
    multi method grabpairs(Baggy:D: $count) {
        Seq.new(class :: does Rakudo::QuantHash::Pairs {
            method pull-one() is raw {
                nqp::if(
                  nqp::elems($!picked),
                  nqp::stmts(
                    (my $pair := nqp::atkey(
                      $!elems,
                      (my $key := nqp::pop_s($!picked))
                    )),
                    nqp::deletekey($!elems,$key),
                    $pair
                  ),
                  IterationEnd
                )
            }
        }.new(self.RAW-HASH, $count))
    }

    proto method pickpairs(|) { * }
    multi method pickpairs(Baggy:D:) {
        nqp::if(
          (my $raw := self.RAW-HASH) && nqp::elems($raw),
          nqp::iterval(Rakudo::QuantHash.ROLL($raw)),
          Nil
        )
    }
    multi method pickpairs(Baggy:D: Callable:D $calculate) {
        self.pickpairs( $calculate(self.total) )
    }
    multi method pickpairs(Baggy:D: Whatever $) {
        self.pickpairs(Inf)
    }
    multi method pickpairs(Baggy:D: $count) {
        Seq.new(class :: does Rakudo::QuantHash::Pairs {
            method pull-one() is raw {
                nqp::if(
                  nqp::elems($!picked),
                  nqp::atkey($!elems,nqp::pop_s($!picked)),
                  IterationEnd
                )
            }
        }.new(self.RAW-HASH, $count))
    }

    proto method grab(|) { * }
    multi method grab(Baggy:D: |c) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }

    proto method pick(|) { * }
    multi method pick(Baggy:D:) { self.roll }
    multi method pick(Baggy:D: Callable:D $calculate) {
        self.pick( $calculate(self.total) )
    }
    multi method pick(Baggy:D: Whatever) { self.pick(Inf) }
    multi method pick(Baggy:D: $count) {
        Seq.new(nqp::if(
          (my $todo = nqp::if(
            $count == Inf,
            (my $total := self.total),
            $count.Int                          # also handles NaN
          )) < 1,
          Rakudo::Iterator.Empty,               # nothing to do
          class :: does Iterator {
              has $!raw;      # the IterationSet of the Baggy
              has $!weights;  # clone of raw, but with just the weights
              has $!todo;     # number of draws to do
              has $!total;    # total number of draws possible

              # Return the .WHICH key of a randomly picked object.  Updates
              # the weight of the picked object and the total number of draws
              # still possible.
              method BAG-PICK() {
                  nqp::stmts(
                    (my Int $rand := $!total.rand.Int),
                    (my Int $seen := 0),
                    (my $iter := nqp::iterator($!weights)),
                    nqp::while(
                      $iter && nqp::isle_I(
                        ($seen := nqp::add_I(
                          $seen,
                          nqp::iterval(nqp::shift($iter)),
                          Int
                        )),
                        $rand
                      ),
                      nqp::null
                    ),
                    nqp::bindkey(                # $iter now contains picked one
                      $!weights,
                      nqp::iterkey_s($iter),
                      nqp::sub_I(nqp::iterval($iter),1,Int)
                    ),
                    ($!total := nqp::sub_I($!total,1,Int)),
                    nqp::iterkey_s($iter)
                  )
              }

              method SET-SELF(\raw, \todo, \total) {
                  nqp::stmts(
                    ($!weights := nqp::clone($!raw := raw)),
                    (my $iter := nqp::iterator($!weights)),
                    nqp::while(
                      $iter,
                      nqp::bindkey(
                        $!weights,
                        nqp::iterkey_s(nqp::shift($iter)),
                        nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                      )
                    ),
                    ($!todo := nqp::if(todo > total,total,todo)),
                    ($!total := total),
                    self
                  )
              }
              method new(\raw, \todo, \total) {
                  nqp::create(self).SET-SELF(raw, todo, total)
              }

              method pull-one() is raw {
                  nqp::if(
                    $!todo,
                    nqp::stmts(
                      ($!todo := nqp::sub_I($!todo,1,Int)),
                      nqp::getattr(nqp::atkey($!raw,self.BAG-PICK),Pair,'$!key')
                    ),
                    IterationEnd
                  )
              }
              method skip-one() {
                  nqp::if(
                    $!todo,
                    nqp::stmts(
                      ($!todo := nqp::sub_I($!todo,1,Int)),
                      self.BAG-PICK
                    )
                  )
              }
              method push-all($target --> IterationEnd) {
                  nqp::stmts(
                    (my $todo = $!todo),
                    nqp::while(
                      $todo,
                      nqp::stmts(
                        --$todo,
                        $target.push(nqp::getattr(
                          nqp::atkey($!raw,self.BAG-PICK),
                          Pair,
                          '$!key'
                        ))
                      )
                    ),
                    ($!todo := nqp::decont($todo))
                  )
              }
              method count-only() { $!todo - 1 }
              method bool-only(--> True) { }
              method sink-all() { $!todo := 0 }

          }.new(self.RAW-HASH, $todo, nqp::ifnull($total,self.total))
        ))
    }

    proto method roll(|) { * }
    multi method roll(Baggy:D:) {
        nqp::if(
          (my $total := self.total),
          nqp::getattr(
            nqp::iterval(Rakudo::QuantHash.BAG-ROLL(self.RAW-HASH,$total)),
            Pair,
            '$!key'
          ),
          Nil
        )
    }
    multi method roll(Baggy:D: Whatever) {
        Seq.new(nqp::if(
          (my $raw := self.RAW-HASH) && (my $total := self.total),
          Rakudo::Iterator.Callable( {
              nqp::getattr(
                nqp::iterval(Rakudo::QuantHash.BAG-ROLL($raw, $total)),
                Pair,
                '$!key'
              )
          }, True ),
          Rakudo::Iterator.Empty
        ))
    }
    multi method roll(Baggy:D: Callable:D $calculate) {
      nqp::if(
        (my $total := self.total),
        self.roll($calculate($total)),
        Seq.new(Rakudo::Iterator.Empty)
      )
    }
    multi method roll(Baggy:D: $count) {
        nqp::if(
          $count == Inf,
          self.roll(*),                         # let Whatever handle it
          Seq.new(nqp::if(                      # something else as count
            (my $todo = $count.Int) < 1, # also handles NaN
            Rakudo::Iterator.Empty,             # nothing to do
            nqp::if(
              (my $raw := self.RAW-HASH)
                && (my $total := self.total)
                && ++$todo,
              Rakudo::Iterator.Callable( {      # need to do a number of times
                  nqp::if(
                    --$todo,
                    nqp::getattr(
                      nqp::iterval(Rakudo::QuantHash.BAG-ROLL($raw, $total)),
                      Pair,
                      '$!key'
                    ),
                    IterationEnd
                  )
              }),
              Rakudo::Iterator.Empty            # nothing to roll for
            )
          ))
        )
    }

#--- classification method
    proto method classify-list(|) { * }
    multi method classify-list( &test, \list) {
        fail X::Cannot::Lazy.new(:action<classify>) if list.is-lazy;
        my \iter = (nqp::istype(list, Iterable) ?? list !! list.list).iterator;

        while (my $value := iter.pull-one) !=:= IterationEnd {
            my $tested := test($value);
            if nqp::istype($tested, Iterable) { # multi-level classify
                X::Invalid::ComputedValue.new(
                    :name<mapper>,
                    :method<classify-list>,
                    :value<an Iterable item>,
                    :reason(self.^name ~ ' cannot be nested and so does not '
                        ~ 'support multi-level classification'),
                ).throw;
            }
            else {
                self{$tested}++;
            }
        }
        self;
    }
    multi method classify-list( %test, |c ) {
        self.classify-list( { %test{$^a} }, |c );
    }
    multi method classify-list( @test, |c ) {
        self.classify-list( { @test[$^a] }, |c );
    }
    multi method classify-list(&test, **@list, |c) {
        self.classify-list(&test, @list, |c);
    }

    proto method categorize-list(|) { * }
    multi method categorize-list( &test, \list ) {
        fail X::Cannot::Lazy.new(:action<categorize>) if list.is-lazy;
        my \iter = (nqp::istype(list, Iterable) ?? list !! list.list).iterator;
        my $value := iter.pull-one;
        unless $value =:= IterationEnd {
            my $tested := test($value);

            # multi-level categorize
            if nqp::istype($tested[0],Iterable) {
                X::Invalid::ComputedValue.new(
                    :name<mapper>,
                    :method<categorize-list>,
                    :value<a nested Iterable item>,
                    :reason(self.^name ~ ' cannot be nested and so does not '
                        ~ 'support multi-level categorization'),
                ).throw;
            }
            # simple categorize
            else {
                loop {
                    self{$_}++ for @$tested;
                    last if ($value := iter.pull-one) =:= IterationEnd;
                    nqp::istype(($tested := test($value))[0], Iterable)
                        and X::Invalid::ComputedValue.new(
                            :name<mapper>,
                            :method<categorize-list>,
                            :value('an item with different number of elements '
                                ~ 'in it than previous items'),
                            :reason('all values need to have the same number '
                                ~ 'of elements. Mixed-level classification is '
                                ~ 'not supported.'),
                        ).throw;
                };
            }
       }
       self;
    }
    multi method categorize-list( %test, |c ) {
        self.categorize-list( { %test{$^a} }, |c );
    }
    multi method categorize-list( @test, |c ) {
        self.categorize-list( { @test[$^a] }, |c );
    }
    multi method categorize-list( &test, **@list, |c ) {
        self.categorize-list( &test, @list, |c );
    }

#--- coercion methods
   sub SETIFY(\baggy, \type) {
        nqp::if(
          (my $raw := baggy.RAW-HASH) && nqp::elems($raw),
          nqp::stmts(
            (my $elems := nqp::clone($raw)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::bindkey(
                $elems,
                nqp::iterkey_s(nqp::shift($iter)),
                nqp::getattr(nqp::iterval($iter),Pair,'$!key'),
              )
            ),
            nqp::create(type).SET-SELF($elems)
          ),
          nqp::if(
            nqp::eqaddr(type,Set),
            set(),
            nqp::create(type)
          )
        )
    }
    multi method Set(Baggy:D:)     { SETIFY(self,Set)     }
    multi method SetHash(Baggy:D:) { SETIFY(self,SetHash) }

    sub MIXIFY(\baggy, \type) {
        nqp::if(
          (my $raw := baggy.RAW-HASH) && nqp::elems($raw),
          nqp::create(type).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE($raw)),
          nqp::if(
            nqp::istype(type,Mix),
            mix(),
            nqp::create(MixHash)
          )
        )
    }

    multi method Mix(Baggy:D:)     { MIXIFY(self, Mix)     }
    multi method MixHash(Baggy:D:) { MIXIFY(self, MixHash) }

    method RAW-HASH() is raw { nqp::getattr(%!elems,Map,'$!storage') }
}

multi sub infix:<eqv>(Baggy:D \a, Baggy:D \b --> Bool:D) {
    nqp::p6bool(
      nqp::eqaddr(a,b) || (nqp::eqaddr(a.WHAT,b.WHAT) && a.ACCEPTS(b))
    )
}
# vim: ft=perl6 expandtab sw=4
