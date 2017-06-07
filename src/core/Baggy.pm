my role Baggy does QuantHash {

# A Bag/BagHash/Mix/MixHash consists of a single hash with Pairs.
# The keys of the hash, are the .WHICH strings of the original object key.
# The values are Pairs containing the original object key and value.

    has %!elems; # key.WHICH => (key,value)

# The Baggy role takes care of all mutable and immutable aspects that are
# shared between Bag,BagHash,Mix,MixHash.  Any specific behaviour for
# mutable and immutable aspects of Mix/MixHash need to live in Mixy.
# Immutables aspects of Bag/Mix, need to live to Bag/Mix respectively.

#--- private methods
    method !WHICH() {
        self.^name
          ~ '|'
          ~ self.keys.sort.map( { $_.WHICH ~ '(' ~ self.AT-KEY($_) ~ ')' } );
    }
    method !LISTIFY(&formatter, str $joiner) {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::stmts(
            (my $list := nqp::setelems(nqp::list_s,nqp::elems($raw))),
            (my $iter := nqp::iterator($raw)),
            (my int $i = -1),
            nqp::while(
              $iter,
              nqp::bindpos_s($list,($i = nqp::add_i($i,1)),
                formatter(
                  (my $pair := nqp::iterval(nqp::shift($iter))).key,
                  $pair.value
                )
              )
            ),
            nqp::p6box_s(nqp::join($joiner,$list))
          ),
          ""
        )
    }

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
    multi method ACCEPTS(Baggy:U: $other) {
        $other.^does(self)
    }
    multi method ACCEPTS(Baggy:D: Mu $other) {
        $other (<+) self && self (<+) $other
    }
    multi method ACCEPTS(Baggy:D: Baggy:D $other --> Bool:D) {
        nqp::p6bool(
          nqp::unless(
            nqp::eqaddr(self,$other),
            nqp::if(
              (%!elems.elems
                == nqp::getattr($other,$other.WHAT,'%!elems').elems),
              nqp::stmts(
                (my $iter := nqp::iterator(
                  nqp::getattr(%!elems,Map,'$!storage'))),
                (my $oelems := nqp::getattr(
                  nqp::getattr($other,$other.WHAT,'%!elems'),Map,'$!storage')),
                nqp::while(
                  $iter,
                  nqp::unless(
                    (nqp::existskey($oelems,nqp::iterkey_s(nqp::shift($iter)))
                      && nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                      == nqp::getattr(nqp::atkey(
                           $oelems,nqp::iterkey_s($iter)),Pair,'$!value')),
                    return False
                  )
                ),
                1
              )
            )
          )
        )
    }

    multi method AT-KEY(Baggy:D: \k) {  # exception: ro version for Bag/Mix
        nqp::if(
          (my $raw := self.raw_hash),
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
          (my $raw := self.raw_hash)
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
          (my $raw := self.raw_hash) && nqp::existskey($raw,k.WHICH)
        )
    }

#--- object creation methods
    multi method new(Baggy:_: +@args) {
        nqp::stmts(
          Rakudo::QuantHash.ADD-ITERATOR-TO-BAG(
            (my $elems := nqp::create(Rakudo::Internals::IterationSet)),
            (my $iterator := @args.iterator)
          ),
          nqp::create(self).SET-SELF($elems)
        )
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
                  ?? nqp::iterval(nqp::shift($!iter)).key
                  !! IterationEnd
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::iterval(nqp::shift($!iter)).key)
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
    multi method WHICH(Baggy:D:)   { self!WHICH }
    multi method elems(Baggy:D: --> Int:D) { %!elems.elems }
    multi method Bool(Baggy:D: --> Bool:D) { %!elems.Bool }

    method HASHIFY(\type) {
        nqp::stmts(
          (my $hash := Hash.^parameterize(type,Any).new),
          (my $descriptor := nqp::getattr($hash,Hash,'$!descriptor')),
          nqp::if(
            (my $raw := self.raw_hash) && nqp::elems($raw),
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
        self!LISTIFY(-> \k,\v {v==1 ?? k.gist !! "{k.gist}({v})"}, ' ')
    }
    multi method gist(Baggy:D: --> Str:D) {
        my str $name = nqp::unbox_s(self.^name);
        ( nqp::chars($name) == 3 ?? nqp::lc($name) !! "$name.new" )
        ~ '('
        ~ self!LISTIFY(-> \k,\v {v==1 ?? k.gist !! "{k.gist}({v})"}, ', ')
        ~ ')'
    }
    multi method perl(Baggy:D: --> Str:D) {
        '('
        ~ self!LISTIFY( -> \k,\v {"{k.perl}=>{v}"}, ',')
        ~ ").{self.^name}"
    }

#--- selection methods
    proto method grabpairs (|) { * }
    multi method grabpairs(Baggy:D:) {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
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
        }.new(self.raw_hash, $count))
    }

    proto method pickpairs(|) { * }
    multi method pickpairs(Baggy:D:) {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
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
        }.new(self.raw_hash, $count))
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
          (my $todo = Rakudo::QuantHash.TODO($count))
            && (my $raw := self.raw_hash)
            && (my int $elems = nqp::elems($raw)),
          nqp::stmts(
            (my $pairs := nqp::setelems(nqp::list,$elems)),
            (my $iter := nqp::iterator($raw)),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos($pairs,$i,Pair.new(
                nqp::getattr(
                  (my $pair := nqp::iterval(nqp::shift($iter))),Pair,'$!key'),
                nqp::assign(nqp::p6scalarfromdesc(nqp::null),
                  nqp::getattr($pair,Pair,'$!value'))
              ))
            ),
            self!ROLLPICKGRABN(nqp::if($todo == Inf,self.total,$todo),$pairs)
          ),
          Rakudo::Iterator.Empty
        ))
    }

    proto method roll(|) { * }
    multi method roll(Baggy:D:) {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::getattr(
            nqp::iterval(Rakudo::QuantHash.BAG-ROLL($raw,self.total)),
            Pair,
            '$!key'
          ),
          Nil
        )
    }
    multi method roll(Baggy:D: Whatever) { self.roll(Inf) }
    multi method roll(Baggy:D: $count) {
        Seq.new(nqp::if(
          $count < 1,
          Rakudo::Iterator.Empty,
          nqp::if(
            $count == Inf,
            Rakudo::Iterator.Roller(self),
            self!ROLLPICKGRABN($count.Int, %!elems.values, :keep)
          )
        ))
    }

    method !ROLLPICKGRABN(Int() $count, @pairs, :$keep) { # N times
        class :: does Iterator {
            has Int $!total;
            has int $!elems;
            has $!pairs;
            has int $!todo;
            has int $!keep;

            method !SET-SELF($!total, \pairs, \keep, \todo) {
                $!elems  = pairs.elems;  # reifies
                $!pairs := nqp::getattr(pairs,List,'$!reified');
                $!todo   = todo;
                $!keep   = +?keep;
                self
            }
            method new(\total,\pairs,\keep,\count) {
                nqp::create(self)!SET-SELF(
                  total, pairs, keep, keep ?? count !! (total min count))
            }

            method pull-one() {
                if $!todo {
                    $!todo = nqp::sub_i($!todo,1);
                    my Int $rand = $!total.rand.Int;
                    my Int $seen = 0;
                    my int $i    = -1;
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$!elems),
                      ($seen = $seen + nqp::atpos($!pairs,$i).value),
                      nqp::if(
                        $seen > $rand,
                        nqp::stmts(
                          nqp::unless(
                            $!keep,
                            nqp::stmts(
                              --(nqp::atpos($!pairs,$i)).value,
                              --$!total,
                            )
                          ),
                          return nqp::atpos($!pairs,$i).key
                        )
                      )
                    );
                }
                IterationEnd
            }
        }.new(self.total,@pairs,$keep,$count)
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
          (my $raw := baggy.raw_hash) && nqp::elems($raw),
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

    sub BAGGIFY(\baggy, \type) {
        nqp::if(
          (my $raw := baggy.raw_hash) && nqp::elems($raw),
          nqp::stmts(                               # something to coerce
            (my $elems := nqp::clone($raw)),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::isgt_i(
                  (my $value := nqp::getattr(
                    nqp::iterval(nqp::shift($iter)),
                    Pair,
                    '$!value'
                    ).Int
                  ),                                # .Int also deconts
                  0
                ),
                nqp::bindkey(                       # ok to keep value.Int
                  $elems,
                  nqp::iterkey_s($iter),
                  nqp::p6bindattrinvres(
                    nqp::clone(nqp::iterval($iter)),
                    Pair,
                    '$!value',
                    $value
                  )
                ),
                nqp::deletekey(                     # we don't do <= 0 in bags
                  $elems,
                  nqp::iterkey_s($iter)
                )
              )
            ),
            nqp::create(type).SET-SELF($elems),
          ),
          nqp::if(                                  # nothing to coerce
            nqp::istype(type,Bag),
            bag(),
            nqp::create(BagHash)
          )
        )
    }

    multi method Bag(Baggy:D:)     { BAGGIFY(self, Bag)     }
    multi method BagHash(Baggy:D:) { BAGGIFY(self, BagHash) }

    method !MIXIFY(\type) {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::stmts(                             # something to coerce
            (my $elems := nqp::clone($raw)),
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
            nqp::create(type).SET-SELF($elems)
          ),
          nqp::if(                                # nothing to coerce
            nqp::istype(type,Mix),
            mix(),
            nqp::create(MixHash)
          )
        )
    }

    multi method Mix(Baggy:D:)     { self!MIXIFY(Mix)     }
    multi method MixHash(Baggy:D:) { self!MIXIFY(MixHash) }

    method clone() {
        nqp::if(
          (my $raw := self.raw_hash) && nqp::elems($raw),
          nqp::stmts(                             # something to clone
            (my $elems := nqp::clone($raw)),
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
                  nqp::clone(nqp::getattr(nqp::iterval($iter),Pair,'$!value'))
                )
              )
            ),
            nqp::create(self).SET-SELF($elems)
          ),
          nqp::create(self)                       # nothing to clone
        )
    }

    method raw_hash() is raw { nqp::getattr(%!elems,Map,'$!storage') }
}

multi sub infix:<eqv>(Baggy:D \a, Baggy:D \b) {
    nqp::p6bool(
      nqp::unless(
        nqp::eqaddr(a,b),
        nqp::eqaddr(a.WHAT,b.WHAT)
          && nqp::getattr(nqp::decont(a),a.WHAT,'%!elems')
               eqv nqp::getattr(nqp::decont(b),b.WHAT,'%!elems')
      )
    )
}
# vim: ft=perl6 expandtab sw=4
