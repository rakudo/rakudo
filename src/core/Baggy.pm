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
        my $total = 0;
        my $iter := nqp::iterator(nqp::getattr(%!elems,Map,'$!storage'));
        $total += nqp::getattr(nqp::iterval(nqp::shift($iter)),Pair,'$!value')
          while $iter;
        $total;
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
    method BUILD(Baggy:D: Mu \elems) {
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
        nqp::create(self).BUILD($elems)
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
        nqp::create(self).BUILD($elems)
    }

#--- iterator methods
    multi method pairs(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                $!hash-iter
                  ?? nqp::iterval(nqp::shift($!hash-iter))
                  !! IterationEnd
            }
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd unless $!hash-iter;
                    $no-sink :=
                      $target.push(nqp::iterval(nqp::shift($!hash-iter)));
                    $done = $done + 1;
                }
                $done
            }
            method push-all($target) {
                my $no-sink;
                $no-sink := $target.push(nqp::iterval(nqp::shift($!hash-iter)))
                  while $!hash-iter;
                IterationEnd
            }
        }.new(%!elems))
    }
    multi method keys(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                $!hash-iter
                  ?? nqp::iterval(nqp::shift($!hash-iter)).key
                  !! IterationEnd
            }
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd unless $!hash-iter;
                    $no-sink :=
                      $target.push(nqp::iterval(nqp::shift($!hash-iter)).key);
                    $done = $done + 1;
                }
                $done
            }
            method push-all($target) {
                my $no-sink;
                $no-sink :=
                  $target.push(nqp::iterval(nqp::shift($!hash-iter)).key)
                    while $!hash-iter;
                IterationEnd
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
                elsif $!hash-iter {
                    my \tmp =
                      nqp::decont(nqp::iterval(nqp::shift($!hash-iter)));
                    $!value := nqp::getattr(tmp,Pair,'$!value');
                    nqp::getattr(tmp,Pair,'$!key')
                }
                else {
                    IterationEnd
                }
            }
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                if $!value.DEFINITE {
                    $no-sink := $target.push($!value);
                    $!value := Mu;
                    $done = $done + 1;
                }
                while $done < $n {
                    return IterationEnd unless $!hash-iter;
                    my \tmp =
                      nqp::decont(nqp::iterval(nqp::shift($!hash-iter)));
                    $no-sink := $target.push(nqp::getattr(tmp,Pair,'$!key'));
                    if ($done = $done + 1) < $n {
                        $no-sink :=
                          $target.push(nqp::getattr(tmp,Pair,'$!value'));
                        $done = $done + 1;
                    }
                    else {
                        $!value := nqp::getattr(tmp,Pair,'$!value');
                    }
                }
                $done
            }
            method push-all($target) {
                my $no-sink;
                while $!hash-iter {
                    my \tmp =
                      nqp::decont(nqp::iterval(nqp::shift($!hash-iter)));
                    $no-sink := $target.push(nqp::getattr(tmp,Pair,'$!key'));
                    $no-sink := $target.push(nqp::getattr(tmp,Pair,'$!value'));
                }
                IterationEnd
            }
        }.new(%!elems))
    }
    multi method values(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() is raw {
                $!hash-iter
                    ?? nqp::getattr(nqp::decont(
                         nqp::iterval(nqp::shift($!hash-iter))),Pair,'$!value')
                    !! IterationEnd
            }
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd unless $!hash-iter;
                    $no-sink := $target.push(nqp::getattr(nqp::decont(
                      nqp::iterval(nqp::shift($!hash-iter))),Pair,'$!value'));
                    $done = $done + 1;
                }
                $done
            }
            method push-all($target) {
                my $no-sink;
                $no-sink := $target.push(nqp::getattr(nqp::decont(
                  nqp::iterval(nqp::shift($!hash-iter))),Pair,'$!value'
                )) while $!hash-iter;
                IterationEnd
            }
        }.new(%!elems))
    }
    multi method antipairs(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MappyIterator {
            method pull-one() {
                if $!hash-iter {
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    Pair.new(tmp.value, tmp.key)
                }
                else {
                    IterationEnd
                }
            }
            method push-exactly($target, int $n) {
                my int $done;
                my $no-sink;
                while $done < $n {
                    return IterationEnd unless $!hash-iter;
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    $no-sink := $target.push(Pair.new(tmp.value, tmp.key));
                    $done = $done + 1;
                }
                IterationEnd
            }
            method push-all($target) {
                my $no-sink;
                while $!hash-iter {
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    $no-sink := $target.push(Pair.new(tmp.value, tmp.key));
                }
                IterationEnd
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
                elsif $!hash-iter {
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    $!key  := tmp.key;
                    $!times = tmp.value - 1;
                    $!key
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                my $no-sink;
                while $!hash-iter {
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    $!key  := tmp.key;
                    $!times = tmp.value + 1;
                    $no-sink := $target.push($!key) while $!times = $!times - 1;
                }
                IterationEnd
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
    method hash(Baggy:D: --> Hash) { %!elems.values.hash }
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
        my \grabbed := ROLLPICKGRAB1(self,%!elems.values);
        %!elems.DELETE-KEY(grabbed.WHICH)
          if %!elems.AT-KEY(grabbed.WHICH).value-- == 1;
        grabbed;
    }
    multi method grab(Baggy:D: $count) {
        if nqp::istype($count,Whatever) || $count == Inf {
            my @grabbed = ROLLPICKGRABN(self,self.total,%!elems.values);
            %!elems = ();
            @grabbed;
        }
        else {
            my @grabbed = ROLLPICKGRABN(self,$count,%!elems.values);
            for @grabbed {
                if %!elems.AT-KEY(.WHICH) -> $pair {
                    %!elems.DELETE-KEY(.WHICH) unless $pair.value;
                }
            }
            @grabbed;
        }
    }

    proto method pick(|) { * }
    multi method pick(Baggy:D:) {
        ROLLPICKGRAB1(self,%!elems.values);
    }
    multi method pick(Baggy:D: $count) {
        ROLLPICKGRABN(self,
          nqp::istype($count,Whatever) || $count == Inf ?? self.total !! $count,
          %!elems.values.map: { (.key => my $ = .value) }
        );
    }

    proto method roll(|) { * }
    multi method roll(Baggy:D:) {
        ROLLPICKGRAB1(self,%!elems.values);
    }
    multi method roll(Baggy:D: $count) {
        nqp::istype($count,Whatever) || $count == Inf
          ?? ROLLPICKGRABW(self,%!elems.values)
          !! ROLLPICKGRABN(self,$count, %!elems.values, :keep);
    }

    sub ROLLPICKGRAB1($self,@pairs) { # one time
        my Int $rand = $self.total.rand.Int;
        my Int $seen = 0;
        return .key if ( $seen += .value ) > $rand for @pairs;
        Nil;
    }

    sub ROLLPICKGRABN($self, \count, @pairs, :$keep) { # N times
        Seq.new(class :: does Iterator {
            has Int $!total;
            has @!pairs;
            has int $!todo;
            has int $!keep;

            method BUILD($!total, @!pairs, \keep, \todo) {
                $!todo = todo;
                $!keep = +?keep;
                self
            }
            method new(\total,\pairs,\keep,\count) {
                nqp::create(self).BUILD(
                  total, pairs, keep, keep ?? count !! (total min count))
            }

            method pull-one() {
                if $!todo {
                    my Int $rand = $!total.rand.Int;
                    my Int $seen = 0;
                    $!todo = $!todo - 1;
                    for @!pairs {
                        if ( $seen += .value ) > $rand {
                            .value--, $!total-- unless $!keep;
                            return .key;
                        }
                    }
                }
                IterationEnd
            }
        }.new($self.total,@pairs,$keep,count))
    }

    sub ROLLPICKGRABW($self,@pairs) { # keep going
        Seq.new(class :: does Iterator {
            has Int $!total;
            has @!pairs;

            method BUILD($!total, @!pairs) { self }
            method new(\total,\pairs) { nqp::create(self).BUILD(total,pairs) }
            method is-lazy() { True }

            method pull-one() {
                my Int $rand = $!total.rand.Int;
                my Int $seen = 0;
                return .key if ( $seen += .value ) > $rand for @!pairs;
                IterationEnd
            }
        }.new($self.total,@pairs))
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
