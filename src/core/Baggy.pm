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
    method !PAIR(\key,\value) { Pair.new(key, my Int $ = value ) }
    method !TOTAL() {
        nqp::if(
          (my $storage := nqp::getattr(%!elems,Map,'$!storage')),
          nqp::stmts(
            (my $total = 0),
            (my $iter := nqp::iterator($storage)),
            nqp::while(
              $iter,
              $total = $total
                + nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
            ),
            $total
          ),
          0
        )
    }
    method !SANITY(%hash --> Nil) {
        my @toolow;
        my $elems := nqp::getattr(%hash,Map,'$!storage');
        my $iter  := nqp::iterator($elems);
        while $iter {
            my \tmp   := nqp::shift($iter);
            my \pair  := nqp::iterval(tmp);
            my $value := pair.value;
            @toolow.push( pair.key )                   if $value <  0;
            nqp::deletekey($elems,nqp::iterkey_s(tmp)) if $value <= 0;
        }
        fail "Found negative values for {@toolow} in {self.^name}" if @toolow;
    }
    method !LISTIFY(&formatter) {
        my $elems := nqp::getattr(%!elems,Map,'$!storage');
        my $list  := nqp::list();
        nqp::setelems($list,nqp::elems($elems));  # presize
        nqp::setelems($list,0);

        my $iter := nqp::iterator($elems);
        while $iter {
            my \pair = nqp::iterval(nqp::shift($iter));
            nqp::push($list,formatter(pair.key,pair.value));
        }
        $list
    }

#--- interface methods
    method !SET-SELF(Baggy:D: Mu \elems) {
        %!elems := elems;

        if nqp::istype(self, Bag) || nqp::istype(self, Mix) {
            my $iter := nqp::iterator(nqp::getattr(%!elems,Map,'$!storage'));
            while $iter {
                my \pair = nqp::iterval(nqp::shift($iter));
                nqp::bindattr(pair,Pair,'$!value',
                  nqp::decont(nqp::getattr(pair,Pair,'$!value'))
                );
            }
        }
        self
    }
    multi method ACCEPTS(Baggy:U: $other) {
        $other.^does(self)
    }
    multi method ACCEPTS(Baggy:D: Mu $other) {
        $other (<+) self && self (<+) $other
    }
    multi method ACCEPTS(Baggy:D: Baggy:D $other --> Bool) {
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
                  nqp::stmts(
                    nqp::shift($iter),
                    nqp::unless(
                      (nqp::existskey($oelems,nqp::iterkey_s($iter))
                        && nqp::getattr(nqp::iterval($iter),Pair,'$!value')
                        == nqp::getattr(nqp::atkey(
                             $oelems,nqp::iterkey_s($iter)),Pair,'$!value')),
                      return False
                    )
                  )
                ),
                1
              )
            )
          )
        )
    }

    multi method AT-KEY(Baggy:D: \k) {  # exception: ro version for Bag/Mix
        my $elems    := nqp::getattr(%!elems,Map,'$!storage');
        my str $which = nqp::unbox_s(k.WHICH);
        nqp::existskey($elems,$which)
          ?? nqp::getattr(nqp::decont(nqp::atkey($elems,$which)),Pair,'$!value')
          !! 0
    }
    multi method DELETE-KEY(Baggy:D: \k) {
        my $elems    := nqp::getattr(%!elems,Map,'$!storage');
        my str $which = nqp::unbox_s(k.WHICH);
        if nqp::existskey($elems,$which) {
            my \v = nqp::getattr(
              nqp::decont(nqp::atkey($elems,$which)),
              Pair,'$!value');
            nqp::deletekey($elems,$which);
            v
        }
        else {
            0
        }
    }
    multi method EXISTS-KEY(Baggy:D: \k) {
        nqp::p6bool(
          nqp::existskey(
            nqp::getattr(%!elems,Map,'$!storage'),nqp::unbox_s(k.WHICH)));
    }

#--- object creation methods
    multi method new(Baggy:_: +@args) {
        my $elems := nqp::hash();
        my str $which;
        for @args {
            $which = nqp::unbox_s(.WHICH);
            if nqp::existskey($elems,$which) {
                my $value :=
                  nqp::getattr(nqp::atkey($elems,$which),Pair,'$!value');
                $value = $value + 1;
            }
            else {
                nqp::bindkey($elems,$which,self!PAIR($_,1));
            }
        }
        nqp::create(self)!SET-SELF($elems)
    }
    method new-from-pairs(*@pairs) {
        my $elems := nqp::hash();
        my str $which;
        my int $seen-pair;
        for @pairs {
            when Pair {
                $seen-pair = 1;
                $which = nqp::unbox_s(.key.WHICH);
                if nqp::existskey($elems,$which) {
                    my $value :=
                      nqp::getattr(nqp::atkey($elems,$which),Pair,'$!value');
                    $value = $value + .value;
                }
                else {
                    nqp::bindkey($elems,$which,self!PAIR(.key,.value));
                }
            }
            default {
                $which = nqp::unbox_s(.WHICH);
                if nqp::existskey($elems,$which) {
                    my $value :=
                      nqp::getattr(nqp::atkey($elems,$which),Pair,'$!value');
                    $value = $value + 1;
                }
                else {
                    nqp::bindkey($elems,$which,self!PAIR($_,1));
                }
            }
        }
        self!SANITY($elems) if $seen-pair;
        nqp::create(self)!SET-SELF($elems)
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
                $!iter
                    ?? nqp::getattr(nqp::decont(
                         nqp::iterval(nqp::shift($!iter))),Pair,'$!value')
                    !! IterationEnd
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  $target.push(nqp::getattr(nqp::decont(
                    nqp::iterval(nqp::shift($!iter))),Pair,'$!value'))
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
                        (my $pair := nqp::decont(
                          nqp::iterval(nqp::shift($!iter)))),
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
                      (my $pair := nqp::decont(
                        nqp::iterval(nqp::shift($!iter)))),
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
        %!elems.values.map: { (.value »=>» .key).cache.Slip }
    }

#--- introspection methods
    multi method WHICH(Baggy:D:)   { self!WHICH }
    method total(Baggy:D:)         { self!TOTAL }
    method elems(Baggy:D: --> Int) { %!elems.elems }
    method Bool(Baggy:D: --> Bool) {
        nqp::p6bool(nqp::elems(nqp::getattr(%!elems,Map,'$!storage')))
    }
    method hash(Baggy:D: --> Hash) {
        my \h = Hash.^parameterize(Any, Any).new;
        h = %!elems.values;
        h;
    }
    method default(Baggy:D:)       { 0 }

    multi method Str(Baggy:D: --> Str) {
        join(' ', self!LISTIFY(-> \k,\v {v==1 ?? k.gist !! "{k.gist}({v})"}))
    }
    multi method gist(Baggy:D: --> Str) {
        my str $name = nqp::unbox_s(self.^name);
        ( nqp::chars($name) == 3 ?? nqp::lc($name) !! "$name.new" )
        ~ '('
        ~ join(', ',self!LISTIFY(-> \k,\v {v==1 ?? k.gist !! "{k.gist}({v})"}))
        ~ ')'
    }
    multi method perl(Baggy:D: --> Str) {
        '('
        ~ join(',', self!LISTIFY( -> \k,\v {"{k.perl}=>{v}"} ))
        ~ ").{self.^name}"
    }

#--- selection methods
    proto method grabpairs (|) { * }
    multi method grabpairs(Baggy:D:) {
        %!elems.DELETE-KEY(%!elems.keys.pick);
    }
    multi method grabpairs(Baggy:D: $count) {
        if nqp::istype($count,Whatever) || $count == Inf {
            my @grabbed = %!elems{%!elems.keys.pick(%!elems.elems)};
            %!elems = ();
            @grabbed;
        }
        else {
            %!elems{ %!elems.keys.pick($count) }:delete;
        }
    }

    proto method pickpairs(|) { * }
    multi method pickpairs(Baggy:D:) {
        %!elems.AT-KEY(%!elems.keys.pick);
    }
    multi method pickpairs(Baggy:D: Callable:D $calculate) {
        self.pickpairs( $calculate(self.total) )
    }
    multi method pickpairs(Baggy:D: $count) {
        %!elems{ %!elems.keys.pick(
          nqp::istype($count,Whatever) || $count == Inf
            ?? %!elems.elems
            !! $count
        ) };
    }

    proto method grab(|) { * }
    multi method grab(Baggy:D:) {
        my \grabbed := self.roll;
        %!elems.DELETE-KEY(grabbed.WHICH)
          if %!elems.AT-KEY(grabbed.WHICH).value-- == 1;
        grabbed;
    }
    multi method grab(Baggy:D: Callable:D $calculate) {
        self.grab( $calculate(self.total) )
    }
    multi method grab(Baggy:D: $count) {
        if nqp::istype($count,Whatever) || $count == Inf {
            my @grabbed = self!ROLLPICKGRABN(self.total,%!elems.values);
            %!elems = ();
            @grabbed;
        }
        else {
            my @grabbed = self!ROLLPICKGRABN($count,%!elems.values);
            for @grabbed {
                if %!elems.AT-KEY(.WHICH) -> $pair {
                    %!elems.DELETE-KEY(.WHICH) unless $pair.value;
                }
            }
            @grabbed;
        }
    }

    proto method pick(|) { * }
    multi method pick(Baggy:D:) { self.roll }
    multi method pick(Baggy:D: Callable:D $calculate) {
        self.pick( $calculate(self.total) )
    }
    multi method pick(Baggy:D: $count) {
        my $hash     := nqp::getattr(%!elems,Map,'$!storage');
        my int $elems = nqp::elems($hash);
        my $pairs    := nqp::setelems(nqp::list,$elems);

        my \iter := nqp::iterator($hash);
        my int $i = -1;
        my $pair;

        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos($pairs,$i,Pair.new(
            nqp::getattr(
              ($pair := nqp::iterval(nqp::shift(iter))),Pair,'$!key'),
            nqp::assign(nqp::p6scalarfromdesc(nqp::null),
              nqp::getattr($pair,Pair,'$!value'))
          ))
        );

        self!ROLLPICKGRABN(
          nqp::istype($count,Whatever) || $count == Inf ?? self.total !! $count,
          $pairs
        )
    }

    proto method roll(|) { * }
    multi method roll(Baggy:D:) {
        nqp::stmts(
          (my Int $rand = self.total.rand.Int),
          (my Int $seen = 0),
          (my \iter := nqp::iterator(nqp::getattr(%!elems,Map,'$!storage'))),
          nqp::while(
            iter && ($seen = $seen + nqp::getattr(
              nqp::iterval(nqp::shift(iter)),Pair,'$!value')) <= $rand,
            nqp::null
          ),
          nqp::if(
            $seen > $rand,
            nqp::getattr(nqp::iterval(iter),Pair,'$!key'),
            Nil
          )
        )
    }
    multi method roll(Baggy:D: $count) {
        nqp::istype($count,Whatever) || $count == Inf
          ?? Seq.new(Rakudo::Iterator.Roller(self))
          !! self!ROLLPICKGRABN($count, %!elems.values, :keep);
    }

    method !ROLLPICKGRABN(\count, @pairs, :$keep) { # N times
        Seq.new(class :: does Iterator {
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
        }.new(self.total,@pairs,$keep,count))
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
    method Set()     {     Set.new(self.keys) }
    method SetHash() { SetHash.new(self.keys) }
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
