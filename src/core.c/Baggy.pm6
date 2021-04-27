my role Baggy does QuantHash {

# A Bag/BagHash/Mix/MixHash consists of a single hash with Pairs.
# The keys of the hash, are the .WHICH strings of the original object key.
# The values are Pairs containing the original object key and value.

    has Rakudo::Internals::IterationSet $!elems; # key.WHICH => (key,value)

# The Baggy role takes care of all mutable and immutable aspects that are
# shared between Bag,BagHash,Mix,MixHash.  Any specific behaviour for
# mutable and immutable aspects of Mix/MixHash need to live in Mixy.
# Immutables aspects of Bag/Mix, need to live to Bag/Mix respectively.

#--- interface methods
    method of() { UInt }

    multi method ACCEPTS(Baggy:U: \other --> Bool:D) {
        other.^does(self)
    }
    multi method ACCEPTS(Baggy:D: Baggy:D \other --> Bool:D) {
        self (==) other
    }
    multi method ACCEPTS(Baggy:D: \other --> Bool:D) {
        self (==) other.Bag
    }

    my constant notfound =
      nqp::p6bindattrinvres(nqp::create(Pair),Pair,'$!value',0);

    multi method AT-KEY(Baggy:D: \k) {  # exception: ro version for Bag/Mix
        $!elems
          ?? nqp::getattr(
               nqp::ifnull(nqp::atkey($!elems,k.WHICH),notfound),
               Pair,
               '$!value'
             )
          !! 0
    }
    multi method DELETE-KEY(Baggy:D: \k) {
        nqp::if(
          $!elems && nqp::existskey($!elems,(my $which := k.WHICH)),
          nqp::stmts(
            (my \value :=
              nqp::getattr(nqp::atkey($!elems,$which),Pair,'$!value')),
            nqp::deletekey($!elems,$which),
            value
          ),
          0
        )
    }
    multi method EXISTS-KEY(Baggy:D: \k) {
        nqp::hllbool(
          $!elems ?? nqp::existskey($!elems,k.WHICH) !! 0
        )
    }

#--- object creation methods

    # helper method to create Bag from iterator, check for laziness
    method !create-from-iterator(\type, \iterator --> Baggy:D) {
        iterator.is-lazy
          ?? type.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::create(type).SET-SELF(
               Rakudo::QuantHash.ADD-ITERATOR-TO-BAG(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
               )
             )
    }

    multi method new(Baggy:_: --> Baggy:D) { nqp::create(self) }
    multi method new(Baggy:_: \value --> Baggy:D) {
        nqp::if(
          nqp::istype(value,Iterable) && nqp::not_i(nqp::iscont(value)),
          self!create-from-iterator(self, value.iterator),
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
        self!create-from-iterator(self, @args.iterator)
    }

    method new-from-pairs(Baggy:_: *@pairs --> Baggy:D) {
        (my \iterator := @pairs.iterator).is-lazy
          ?? self.fail-iterator-cannot-be-lazy('coerce')
          !! nqp::create(self).SET-SELF(
               Rakudo::QuantHash.ADD-PAIRS-TO-BAG(
                 nqp::create(Rakudo::Internals::IterationSet),
                 iterator,
                 self.keyof
               )
             )
    }

#--- iterator methods
    multi method iterator(Baggy:D:) {
        Rakudo::Iterator.Mappy-values($!elems)
    }

    my class Keys does Rakudo::Iterator::Mappy {
        method pull-one() {
            $!iter
              ?? nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              target.push(
                nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
              )
            )
        }
    }
    multi method keys(Baggy:D:) { Seq.new(Keys.new($!elems)) }

    multi method kv(Baggy:D:) {
        Seq.new(Rakudo::Iterator.Mappy-kv-from-pairs($!elems))
    }

    my class Values does Rakudo::Iterator::Mappy {
        method pull-one() is raw {
            $!iter
              ?? nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!value')
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(  # doesn't sink
              $!iter,
              target.push(
                nqp::getattr(
                  nqp::iterval(nqp::shift($!iter)),
                  Pair,
                  '$!value'
                )
              )
            )
        }
    }
    multi method values(Baggy:D:) { Seq.new(Values.new($!elems)) }

    my class AntiPairs does Rakudo::Iterator::Mappy {
        method pull-one() {
            $!iter
              ?? nqp::iterval(nqp::shift($!iter)).antipair
              !! IterationEnd
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              target.push(nqp::iterval(nqp::shift($!iter)).antipair),
            )
        }
    }
    multi method antipairs(Baggy:D:) { Seq.new(AntiPairs.new($!elems)) }

    my class KxxV does Rakudo::Iterator::Mappy {
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
                    (my \pair := nqp::iterval(nqp::shift($!iter))),
                    Pair,
                    '$!key'
                  )),
                  ($!times =
                    nqp::sub_i(nqp::getattr(pair,Pair,'$!value'),1)),
                  $!key
                ),
                IterationEnd
              )
            )
        }
        method skip-one() { # the default skip-one, too difficult to handle
            nqp::not_i(nqp::eqaddr(self.pull-one,IterationEnd))
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
              $!iter,
              nqp::stmts(
                ($!key := nqp::getattr(
                  (my \pair := nqp::iterval(nqp::shift($!iter))),
                  Pair,
                  '$!key'
                )),
                ($!times =
                  nqp::add_i(nqp::getattr(pair,Pair,'$!value'),1)),
                nqp::while(  # doesn't sink
                  ($!times = nqp::sub_i($!times,1)),
                  target.push($!key)
                )
              )
            )
        }
    }
    proto method kxxv(|) {*}
    multi method kxxv(Baggy:D:) { Seq.new(KxxV.new($!elems)) }

    multi method invert(Baggy:D:) {
        Seq.new(Rakudo::Iterator.Invert(Rakudo::Iterator.Mappy-values($!elems)))
    }

#--- introspection methods
    multi method elems(Baggy:D:) {
        nqp::istrue($!elems) && nqp::elems($!elems)
    }
    multi method Bool(Baggy:D: --> Bool:D) {
        nqp::hllbool($!elems ?? nqp::elems($!elems) !! 0)
    }

    method !HASHIFY(\type) {
        my \hash := Hash.^parameterize(type,Any).new;
        my \descriptor := nqp::getattr(hash,Hash,'$!descriptor');
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my \storage := nqp::clone($!elems)),
            (my \iter := nqp::iterator(storage)),
            nqp::while(
              iter,
              nqp::bindkey(
                storage,
                nqp::iterkey_s(nqp::shift(iter)),
                nqp::p6bindattrinvres(
                  nqp::clone(nqp::iterval(iter)),Pair,'$!value',
                  (nqp::p6scalarfromdesc(descriptor) =
                    nqp::getattr(nqp::iterval(iter),Pair,'$!value'))
                )
              )
            ),
            nqp::bindattr(hash,Map,'$!storage',storage)
          )
        );
        hash
    }
    multi method hash(Baggy:D: --> Hash:D) { self!HASHIFY(UInt) }
    multi method Hash(Baggy:D: --> Hash:D) { self!HASHIFY(Any) }

    method default(Baggy:D: --> 0) { }

    multi method Str(Baggy:D: --> Str:D) {
        nqp::join(' ',Rakudo::QuantHash.RAW-VALUES-MAP(self, {
            (my \value := nqp::getattr($_,Pair,'$!value')) == 1
              ?? nqp::getattr($_,Pair,'$!key').gist
              !! "{nqp::getattr($_,Pair,'$!key').gist}({value})"
        }))
    }
    multi method gist(Baggy:D: --> Str:D) {
        nqp::concat(
          nqp::concat(
            nqp::concat(self.^name,'('),
            nqp::join(' ',
              Rakudo::Sorting.MERGESORT-str(
                Rakudo::QuantHash.RAW-VALUES-MAP(self, {
                    (my \value := nqp::getattr($_,Pair,'$!value')) == 1
                      ?? nqp::getattr($_,Pair,'$!key').gist
                      !! "{nqp::getattr($_,Pair,'$!key').gist}({value})"
                })
              )
            )
          ),
          ')',
        )
    }
    multi method raku(Baggy:D: --> Str:D) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my \pairs := nqp::join(',',
              Rakudo::QuantHash.RAW-VALUES-MAP(self, {
                  nqp::concat(
                    nqp::concat(
                      nqp::getattr($_,Pair,'$!key').raku,
                      '=>'
                    ),
                    nqp::getattr($_,Pair,'$!value').raku
                  )
              })
            )),
            nqp::if(
              nqp::eqaddr(self.keyof,Mu),
              nqp::concat(
                nqp::concat('(',pairs),
                nqp::concat(').',self.^name)
              ),
              nqp::concat(
                nqp::concat(self.^name,'.new-from-pairs('),
                nqp::concat(pairs,')')
              )
            )
          ),
          nqp::if(
            nqp::eqaddr(self,bag()),
            'bag()',
            nqp::if(
              nqp::eqaddr(self,mix()),
              'mix()',
              nqp::concat('().',self.^name)
            )
          )
        )
    }

#--- selection methods
    proto method grabpairs (|) {*}
    multi method grabpairs(Baggy:D:) {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my \iter := Rakudo::QuantHash.ROLL($!elems)),
            (my \pair := nqp::iterval(iter)),
            nqp::deletekey($!elems,nqp::iterkey_s(iter)),
            pair
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

    my class GrabPairsN does Rakudo::QuantHash::Pairs {
        method pull-one() is raw {
            nqp::if(
              nqp::elems($!picked),
              nqp::stmts(
                (my \pair := nqp::atkey(
                  $!elems,
                  (my \key := nqp::pop_s($!picked))
                )),
                nqp::deletekey($!elems,key),
                pair
              ),
              IterationEnd
            )
        }
    }
    multi method grabpairs(Baggy:D: \count) {
        Seq.new(GrabPairsN.new($!elems,count))
    }

    proto method pickpairs(|) {*}
    multi method pickpairs(Baggy:D:) {
        $!elems && nqp::elems($!elems)
          ?? nqp::iterval(Rakudo::QuantHash.ROLL($!elems))
          !! Nil
    }
    multi method pickpairs(Baggy:D: Callable:D $calculate) {
        self.pickpairs( $calculate(self.elems) )
    }
    multi method pickpairs(Baggy:D: Whatever $) {
        self.pickpairs(Inf)
    }

    my class PickPairsN does Rakudo::QuantHash::Pairs {
        method pull-one() is raw {
            nqp::elems($!picked)
              ?? nqp::atkey($!elems,nqp::pop_s($!picked))
              !! IterationEnd
        }
    }
    multi method pickpairs(Baggy:D: \count) {
        Seq.new(PickPairsN.new($!elems,count))
    }

    proto method grab(|) {*}
    multi method grab(Baggy:D: |c) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }

    proto method pick(|) {*}
    multi method pick(Baggy:D:) { self.roll }
    multi method pick(Baggy:D: Callable:D $calculate) {
        self.pick( $calculate(self.total) )
    }
    multi method pick(Baggy:D: Whatever) { self.pick(Inf) }

    my class PickN does PredictiveIterator {
        has $!raw;      # the IterationSet of the Baggy
        has $!weights;  # clone of raw, but with just the weights
        has $!todo;     # number of draws to do
        has $!total;    # total number of draws possible

        # Return the .WHICH key of a randomly picked object.  Updates
        # the weight of the picked object and the total number of draws
        # still possible.
        method BAG-PICK() {
            my Int $rand := $!total.rand.Int;
            my Int $seen := 0;
            my \iter := nqp::iterator($!weights);
            nqp::while(
              iter && nqp::isle_I(
                ($seen := nqp::add_I(
                  $seen,
                  nqp::iterval(nqp::shift(iter)),
                  Int
                )),
                $rand
              ),
              nqp::null
            );

            nqp::bindkey(                # iter now contains picked one
              $!weights,
              nqp::iterkey_s(iter),
              nqp::sub_I(nqp::iterval(iter),1,Int)
            );
            $!total := nqp::sub_I($!total,1,Int);

            nqp::iterkey_s(iter)
        }

        method !SET-SELF(\raw, \todo, \total) {
            $!weights := nqp::clone($!raw := raw);
            my \iter := nqp::iterator($!weights);
            nqp::while(
              iter,
              nqp::bindkey(
                $!weights,
                nqp::iterkey_s(nqp::shift(iter)),
                nqp::getattr(nqp::iterval(iter),Pair,'$!value')
              )
            );
            $!todo := todo > total ?? total !! todo;
            $!total := total;
            self
        }
        method new(\raw, \todo, \total) {
            nqp::create(self)!SET-SELF(raw, todo, total)
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
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my $todo = $!todo),
              nqp::while(
                $todo,
                nqp::stmts(
                  --$todo,
                  target.push(nqp::getattr(
                    nqp::atkey($!raw,self.BAG-PICK),
                    Pair,
                    '$!key'
                  ))
                )
              ),
              ($!todo := nqp::decont($todo))
            )
        }
        method count-only(--> Int:D) { $!todo }
        method sink-all(--> IterationEnd) { $!todo := 0 }

    }
    multi method pick(Baggy:D: \count) {
        Seq.new(
          (my \total := self.total) < 1
            || (my \todo := count == Inf ?? total !! count.Int) < 1
            ?? Rakudo::Iterator.Empty            # nothing to do
            !! PickN.new($!elems,todo,total)
        )
    }

    proto method roll(|) {*}
    multi method roll(Baggy:D:) {
        $!elems && (my \total := self.total)
          ?? nqp::getattr(
               nqp::iterval(Rakudo::QuantHash.BAG-ROLL($!elems,total)),
               Pair,
               '$!key'
             )
          !! Nil
    }
    multi method roll(Baggy:D: Whatever) {
        Seq.new(
          $!elems && (my \total := self.total)
            ?? Rakudo::Iterator.Callable( {
                 nqp::getattr(
                   nqp::iterval(Rakudo::QuantHash.BAG-ROLL($!elems,total)),
                   Pair,
                   '$!key'
                 )
               }, True)
            !! Rakudo::Iterator.Empty
        )
    }
    multi method roll(Baggy:D: Callable:D $calculate) {
        (my \total := self.total)
          ?? self.roll($calculate(total))
          !! Seq.new(Rakudo::Iterator.Empty)
    }
    multi method roll(Baggy:D: \count) {
        count == Inf
          ?? self.roll(*)                         # let Whatever handle it
          !! Seq.new(                             # something else as count
               (my $todo = count.Int) < 1         # also handles NaN
                 ?? Rakudo::Iterator.Empty        # nothing to do
                 !! $!elems && (my \total := self.total) && ++$todo
                   ?? Rakudo::Iterator.Callable({ # need to do a number of times
                          --$todo
                          ?? nqp::getattr(
                               nqp::iterval(
                                 Rakudo::QuantHash.BAG-ROLL($!elems,total)
                               ),
                               Pair,
                              '$!key'
                             )
                          !! IterationEnd
                      })
                   !! Rakudo::Iterator.Empty      # nothing to roll for
             )
    }

#--- classification method
    proto method classify-list(|) {*}
    multi method classify-list( &test, \list) {
        return self.fail-iterator-cannot-be-lazy('classify')
          if list.is-lazy;
        my \iter = (nqp::istype(list, Iterable) ?? list !! list.list).iterator;

        until nqp::eqaddr((my $value := iter.pull-one),IterationEnd) {
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
                ++self{$tested};
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

    proto method categorize-list(|) {*}
    multi method categorize-list( &test, \list ) {
        return self.fail-iterator-cannot-be-lazy('categorize')
          if list.is-lazy;

        my \iter = (nqp::istype(list, Iterable) ?? list !! list.list).iterator;
        my $value := iter.pull-one;
        unless nqp::eqaddr($value,IterationEnd) {
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
                    ++self{$_} for @$tested;
                    last if nqp::eqaddr(($value := iter.pull-one),IterationEnd);
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
   sub SETIFY(\raw, \type) {
        nqp::if(
          raw && nqp::elems(raw),
          nqp::stmts(
            (my \elems := nqp::clone(raw)),
            (my \iter := nqp::iterator(elems)),
            nqp::while(
              iter,
              nqp::bindkey(
                elems,
                nqp::iterkey_s(nqp::shift(iter)),
                nqp::getattr(nqp::iterval(iter),Pair,'$!key'),
              )
            ),
            nqp::create(type).SET-SELF(elems)
          ),
          nqp::if(
            nqp::eqaddr(type,Set),
            set(),
            nqp::create(type)
          )
        )
    }
    multi method Set(Baggy:D:)     { SETIFY($!elems,Set)     }
    multi method SetHash(Baggy:D:) { SETIFY($!elems,SetHash) }

    sub MIXIFY(\raw, \type) {
        raw && nqp::elems(raw)
          ?? nqp::create(type).SET-SELF(Rakudo::QuantHash.BAGGY-CLONE(raw))
          !! nqp::istype(type,Mix)
            ?? mix()
            !! nqp::create(MixHash)
    }

    multi method Mix(Baggy:D:)     { MIXIFY($!elems, Mix)     }
    multi method MixHash(Baggy:D:) { MIXIFY($!elems, MixHash) }

    method Map {
        nqp::if(
          $!elems && nqp::elems($!elems),
          nqp::stmts(
            (my \storage := nqp::hash),
            (my \iter := nqp::iterator($!elems)),
            nqp::while(
              iter,
              nqp::bindkey(
                storage,
                nqp::getattr(nqp::iterval(nqp::shift(iter)),Pair,'$!key').Str,
                nqp::getattr(nqp::iterval(iter),Pair,'$!value')
              )
            ),
            nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',storage)
          ),
          nqp::create(Map)
        )
    }

    method RAW-HASH() is raw is implementation-detail { $!elems }
}

multi sub infix:<eqv>(Baggy:D \a, Baggy:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT) && a.ACCEPTS(b))
    )
}

# vim: expandtab shiftwidth=4
