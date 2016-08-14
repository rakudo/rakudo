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

        if self.^name.chars == 3 { # shoddy heuristic for Bag/Mix
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
    method ACCEPTS(Baggy:_: $other) {
        self.defined
          ?? $other (<+) self && self (<+) $other
          !! $other.^does(self);
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
    multi method pairs(Baggy:D:) {
        Seq.new(Rakudo::Internals::MappyIterator-values.new(%!elems))
    }
    multi method keys(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
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
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            has Mu $!value;

            method pull-one() is raw {
                if $!value.DEFINITE {
                    my \tmp  = $!value;
                    $!value := Mu;
                    tmp
                }
                elsif $!iter {
                    my \tmp = nqp::decont(nqp::iterval(nqp::shift($!iter)));
                    $!value := nqp::getattr(tmp,Pair,'$!value');
                    nqp::getattr(tmp,Pair,'$!key')
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target --> IterationEnd) {
                my $tmp;
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                    ($tmp := nqp::decont(nqp::iterval(nqp::shift($!iter)))),
                    ($target.push(nqp::getattr($tmp,Pair,'$!key'))),
                    ($target.push(nqp::getattr($tmp,Pair,'$!value')))
                  )
                )
            }
        }.new(%!elems))
    }
    multi method values(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
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
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                if $!iter {
                    my \tmp = nqp::iterval(nqp::shift($!iter));
                    Pair.new(tmp.value, tmp.key)
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target --> IterationEnd) {
                my $tmp;
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                    ($tmp := nqp::iterval(nqp::shift($!iter))),
                    ($target.push(Pair.new($tmp.value,$tmp.key)))
                  )
                )
            }
        }.new(%!elems))
    }
    proto method kxxv(|) { * }
    multi method kxxv(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            has Mu $!key;
            has int $!times;

            method pull-one() is raw {
                if $!times {
                    $!times = $!times - 1;
                    $!key
                }
                elsif $!iter {
                    my \tmp = nqp::iterval(nqp::shift($!iter));
                    $!key  := tmp.key;
                    $!times = tmp.value - 1;
                    $!key
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target --> IterationEnd) {
                my $tmp;
                nqp::while(
                  $!iter,
                  nqp::stmts(
                    ($tmp   := nqp::iterval(nqp::shift($!iter))),
                    ($!key  := $tmp.key),
                    ($!times = nqp::add_i($tmp.value,1)),
                    nqp::while(  # doesn't sink
                      ($!times = nqp::sub_i($!times,1)),
                      ($target.push($!key))
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
        my Int $rand = self.total.rand.Int;
        my Int $seen = 0;
        my \iter    := nqp::iterator(nqp::getattr(%!elems,Map,'$!storage'));

        nqp::while(
          iter,
          nqp::stmts(
            nqp::shift(iter),
            ($seen = $seen + nqp::iterval(iter).value),
            nqp::if(
              $seen > $rand,
              return nqp::iterval(iter).key
            )
          )
        );
        Nil
    }
    multi method roll(Baggy:D: $count) {
        nqp::istype($count,Whatever) || $count == Inf
          ?? Rakudo::Internals.RollerIterator(self)
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
    multi method classify-list( &test, *@list ) {
        fail X::Cannot::Lazy.new(:action<classify>) if @list.is-lazy;
        if @list {

            # multi-level classify
            if nqp::istype(test(@list[0]),Iterable) {
                for @list -> $l {
                    my @keys  = test($l);
                    my $last := @keys.pop;
                    my $bag   = self;
                    $bag = $bag{$_} //= self.new for @keys;
                    $bag{$last}++;
                }
            }

            # just a simple classify
            else {
                self{test $_}++ for @list;
            }
        }
        self;
    }
    multi method classify-list( %test, *@list ) {
        self.classify-list( { %test{$^a} }, @list );
    }
    multi method classify-list( @test, *@list ) {
        self.classify-list( { @test[$^a] }, @list );
    }

    proto method categorize-list(|) { * }
    multi method categorize-list( &test, *@list ) {
        fail X::Cannot::Lazy.new(:action<categorize>) if @list.is-lazy;
        if @list {

            # multi-level categorize
            if nqp::istype(test(@list[0])[0],List) {
                for @list -> $l {
                    for test($l) -> $k {
                        my @keys  = @($k);
                        my $last := @keys.pop;
                        my $bag   = self;
                        $bag = $bag{$_} //= self.new for @keys;
                        $bag{$last}++;
                    }
                }
            }

            # just a simple categorize
            else {
                for @list -> $l {
                    self{$_}++ for test($l);
                }
            }
        }
        self;
    }
    multi method categorize-list( %test, *@list ) {
        self.categorize-list( { %test{$^a} }, @list );
    }
    multi method categorize-list( @test, *@list ) {
        self.categorize-list( { @test[$^a] }, @list );
    }

#--- coercion methods
    method Set()     {     Set.new(self.keys) }
    method SetHash() { SetHash.new(self.keys) }
}

# vim: ft=perl6 expandtab sw=4
