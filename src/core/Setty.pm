my role Setty does QuantHash {
    has %!elems; # key.WHICH => key

    method !SET-SELF(%!elems) { self }
    multi method new(Setty: +@args --> Setty) {
        nqp::stmts(
          (my $elems := nqp::hash),
          (my $iter  := @args.iterator),
          nqp::until(
            nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
            nqp::bindkey($elems,$pulled.WHICH,$pulled)
          ),
          nqp::create(self)!SET-SELF($elems)
        )
    }
    method new-from-pairs(*@pairs --> Setty) {
        nqp::stmts(
          (my $elems := nqp::hash),
          (my $iter  := @pairs.iterator),
          nqp::until(
            nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
            nqp::if(
              nqp::istype($pulled,Pair),
              nqp::if(
                $pulled.value,
                nqp::bindkey($elems,$pulled.key.WHICH,$pulled.key)
              ),
              nqp::bindkey($elems,$pulled.WHICH,$pulled)
            )
          ),
          nqp::create(self)!SET-SELF($elems)
        )
    }

    method default(--> False) { }

    multi method keys(Setty:D:) {
        Seq.new(Rakudo::Iterator.Mappy-values(%!elems))
    }

    method elems(Setty:D: --> Int) { %!elems.elems }
    method total(Setty:D: --> Int) { %!elems.elems }
    multi method antipairs(Setty:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
              nqp::if(
                $!iter,
                Pair.new(True,nqp::iterval(nqp::shift($!iter))),
                IterationEnd
              )
            }
        }.new(%!elems))
    }
    multi method minpairs(Setty:D:) { self.pairs }
    multi method maxpairs(Setty:D:) { self.pairs }
    multi method Bool(Setty:D:) { %!elems.Bool }

    multi method hash(Setty:D: --> Hash) {
        my \e = Hash.^parameterize(Bool, Any).new;
        e{$_} = True for %!elems.values;
        e;
    }

    multi method ACCEPTS(Setty:U: $other) {
        $other.^does(self)
    }
    multi method ACCEPTS(Setty:D: Seq:D \seq) {
        self.ACCEPTS(seq.list)
    }
    multi method ACCEPTS(Setty:D: $other) {
        $other (<=) self && self (<=) $other
    }

    multi method Str(Setty:D $ : --> Str) { ~ %!elems.values }
    multi method gist(Setty:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Set' ?? 'set' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {.gist} ).join(', ')
        ~ ')';
    }
    multi method perl(Setty:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Set' ?? 'set' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {.perl} ).join(',')
        ~ ')';
    }

    proto method grab(|) { * }
    multi method grab(Setty:D:) {
        %!elems.DELETE-KEY(%!elems.keys.pick)
    }
    multi method grab(Setty:D: Callable:D $calculate) {
        self.grab($calculate(%!elems.elems))
    }
    multi method grab(Setty:D: $count) {
        (%!elems{ %!elems.keys.pick($count) }:delete).cache;
    }

    proto method grabpairs(|) { * }
    multi method grabpairs(Setty:D:) {
        Pair.new(%!elems.DELETE-KEY(%!elems.keys.pick),True)
    }
    multi method grabpairs(Setty:D: Callable:D $calculate) {
        self.grabpairs($calculate(%!elems.elems))
    }
    multi method grabpairs(Setty:D: $count) {
        (%!elems{ %!elems.keys.pick($count) }:delete).map( { ($_=>True) } );
    }

    proto method pick(|) { * }
    multi method pick(Setty:D:)       { %!elems.values.pick()       }
    multi method pick(Setty:D: Callable:D $calculate) {
        %!elems.values.pick($calculate(%!elems.elems))
    }
    multi method pick(Setty:D: $count) { %!elems.values.pick($count) }

    proto method roll(|) { * }
    multi method roll(Setty:D:)       { %!elems.values.roll()       }
    multi method roll(Setty:D: $count) { %!elems.values.roll($count) }

    multi method EXISTS-KEY(Setty:D: \k --> Bool) {
        nqp::p6bool(
          %!elems.elems && nqp::existskey(%!elems, nqp::unbox_s(k.WHICH))
        );
    }

    method Bag { Bag.new( %!elems.values ) }
    method BagHash { BagHash.new( %!elems.values ) }
    method Mix { Mix.new( %!elems.values ) }
    method MixHash { MixHash.new( %!elems.values ) }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

# vim: ft=perl6 expandtab sw=4
