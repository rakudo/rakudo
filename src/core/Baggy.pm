my role Baggy does QuantHash {
    has %!elems; # key.WHICH => (key,value)

    method PAIR(\key) { Pair.new(key, my Int $ = 0 ) }
    method SANITY(%elems --> Nil) {
        my @toolow;
        for %elems -> $p {
            my $pair  := $p.value;
            my $value := $pair.value;
            @toolow.push( $pair.key ) if $value <  0;
            %elems.DELETE-KEY($p.key) if $value <= 0;
        }
        fail "Found negative values for {@toolow} in {self.^name}" if @toolow;
    }
    multi method new(Baggy: +@args) {
        my %elems;
        for @args {
            (%elems{.WHICH} //= self.PAIR($_)).value++
        } 
        nqp::create(self).BUILD(%elems)
    }
    method new-from-pairs(*@pairs) {
        my %elems;
        for @pairs {
            when Pair {
                (%elems.AT-KEY(.key.WHICH) //= self.PAIR(.key)).value += .value;
            }
            default {
                (%elems.AT-KEY(.WHICH) //= self.PAIR($_)).value++;
            }
        }
        self.SANITY(%elems);
        nqp::create(self).BUILD(%elems)
    }

    method default(--> Int) { 0 }

    multi method pairs(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() {
                $!hash-iter
                  ?? nqp::iterval(nqp::shift($!hash-iter))
                  !! IterationEnd
            }
            method push-all($target) {
                $target.push(nqp::iterval(nqp::shift($!hash-iter)))
                  while $!hash-iter;
                IterationEnd
            }
        }.new(%!elems))
    }
    multi method keys(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() {
                $!hash-iter
                  ?? nqp::iterval(nqp::shift($!hash-iter)).key
                  !! IterationEnd
            }
            method push-all($target) {
                $target.push(nqp::iterval(nqp::shift($!hash-iter)).key)
                  while $!hash-iter;
                IterationEnd
            }
        }.new(%!elems))
    }
    multi method kv(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
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
            method push-all($target) {
                while $!hash-iter {
                    my \tmp =
                      nqp::decont(nqp::iterval(nqp::shift($!hash-iter)));
                    $target.push(nqp::getattr(tmp,Pair,'$!key'));
                    $target.push(nqp::getattr(tmp,Pair,'$!value'));
                }
                IterationEnd
            }
        }.new(%!elems))
    }
    multi method values(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() is raw {
                $!hash-iter
                    ?? nqp::getattr(nqp::decont(
                         nqp::iterval(nqp::shift($!hash-iter))),Pair,'$!value')
                    !! IterationEnd
            }
            method push-all($target) {
                $target.push(nqp::getattr(nqp::decont(
                  nqp::iterval(nqp::shift($!hash-iter))),Pair,'$!value'
                )) while $!hash-iter;
                IterationEnd
            }
        }.new(%!elems))
    }
    multi method antipairs(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
            method pull-one() {
                if $!hash-iter {
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    Pair.new(tmp.value, tmp.key)
                }
                else {
                    IterationEnd
                }
            }
            method push-all($target) {
                while $!hash-iter {
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    $target.push(Pair.new(tmp.value, tmp.key));
                }
                IterationEnd
            }
        }.new(%!elems))
    }
    method kxxv(Baggy:D:) {
        Seq.new(class :: does Rakudo::Internals::MapIterator {
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
                while $!hash-iter {
                    my \tmp = nqp::iterval(nqp::shift($!hash-iter));
                    $!key  := tmp.key;
                    $!times = tmp.value + 1;
                    $target.push($!key) while $!times = $!times - 1;
                }
                IterationEnd
            }
        }.new(%!elems))
    }

    multi method invert(Baggy:D:) {
        %!elems.values.map: { (.value »=>» .key).cache.Slip }
    }
    method elems(Baggy:D: --> Int) { %!elems.elems }
    method total(--> Int) { [+] self.values }
    method Bool(Baggy:D:) { %!elems.Bool }

    method hash(Baggy:D: --> Hash) { %!elems.values.hash }

    method ACCEPTS($other) {
        self.defined
          ?? $other (<+) self && self (<+) $other
          !! $other.^does(self);
    }

    multi method Str(Baggy:D $ : --> Str) {
        ~ %!elems.values.map( {
              .value == 1 ?? .key.gist !! "{.key.gist}({.value})"
          } );
    }
    multi method gist(Baggy:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Bag' ?? 'bag' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {
              .value == 1 ?? .key.gist !! "{.key.gist}({.value})"
          } ).join(', ')
        ~ ')';
    }
    multi method perl(Baggy:D $ : --> Str) {
        '('
        ~ %!elems.values.map( {"{.key.perl}=>{.value}"} ).join(',')
        ~ ").{self.^name}"
    }

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

    method Set()     {     Set.new(self.keys) }
    method SetHash() { SetHash.new(self.keys) }

    # all read/write candidates, to be shared with Mixes
    multi method DELETE-KEY(Baggy:D: \k) {
        my \v := %!elems.DELETE-KEY(k.WHICH);
        nqp::istype(v,Pair) ?? v.value !! 0;
    }
    multi method EXISTS-KEY(Baggy:D: \k)    { %!elems.EXISTS-KEY(k.WHICH) }
}

# vim: ft=perl6 expandtab sw=4
