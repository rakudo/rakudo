my role Setty does QuantHash {
    has %!elems; # key.WHICH => key

    method SET-SELF(\elems) {
        nqp::stmts(
          nqp::if(
            nqp::elems(nqp::getattr(elems,Map,'$!storage')),
            nqp::bindattr(%!elems,Map,'$!storage',
              nqp::getattr(elems,Map,'$!storage')
            )
          ),
          self
        )
    }
    multi method new(Setty: --> Setty:D) { nqp::create(self) }
    multi method new(Setty: +@args --> Setty:D) {
        nqp::stmts(
          (my $elems := nqp::hash),
          (my $iter  := @args.iterator),
          nqp::until(
            nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd),
            nqp::bindkey($elems,$pulled.WHICH,$pulled)
          ),
          nqp::create(self).SET-SELF($elems)
        )
    }
    method new-from-pairs(*@pairs --> Setty:D) {
        nqp::stmts(
          (my $elems := nqp::hash),
          (my $iter  := @pairs.iterator),
          nqp::until(
            nqp::eqaddr(
              (my $pulled := $iter.pull-one),
              IterationEnd
            ),
            nqp::if(
              nqp::istype($pulled,Pair),
              nqp::if(
                nqp::getattr(nqp::decont($pulled),Pair,'$!value'),
                nqp::bindkey(
                  $elems,
                  nqp::getattr(nqp::decont($pulled),Pair,'$!key').WHICH,
                  nqp::getattr(nqp::decont($pulled),Pair,'$!key')
                )
              ),
              nqp::bindkey($elems,$pulled.WHICH,$pulled)
            )
          ),
          nqp::create(self).SET-SELF($elems)
        )
    }

    method default(--> False) { }

    multi method keys(Setty:D:) {
        Seq.new(Rakudo::Iterator.Mappy-values(%!elems))
    }

    method elems(Setty:D: --> Int:D) { %!elems.elems }
    method total(Setty:D: --> Int:D) { %!elems.elems }
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

    multi method hash(Setty:D: --> Hash:D) {
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

    multi method Str(Setty:D $ : --> Str:D) { ~ %!elems.values }
    multi method gist(Setty:D $ : --> Str:D) {
        my $name := self.^name;
        ( $name eq 'Set' ?? 'set' !! "$name.new" )
        ~ '('
        ~ %!elems.values.map( {.gist} ).join(', ')
        ~ ')';
    }
    multi method perl(Setty:D $ : --> Str:D) {
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

    multi method EXISTS-KEY(Setty:D: \k --> Bool:D) {
        nqp::p6bool(
          nqp::getattr(%!elems,Map,'$!storage')
            && nqp::existskey(nqp::getattr(%!elems,Map,'$!storage'),k.WHICH)
        )
    }

    method !BAGGIFY(\type, int $bind) {
        nqp::if(
          nqp::getattr(%!elems,Map,'$!storage'),
          nqp::stmts(
            (my $elems := nqp::clone(nqp::getattr(%!elems,Map,'$!storage'))),
            (my $iter := nqp::iterator($elems)),
            nqp::while(
              $iter,
              nqp::bindkey(
                $elems,
                nqp::iterkey_s(my $tmp := nqp::shift($iter)),
                Pair.new(
                  nqp::decont(nqp::iterval($tmp)),
                  nqp::if(
                    $bind,
                    1,
                    (nqp::p6scalarfromdesc(nqp::null) = 1)
                  )
                )
              )
            ),
            nqp::create(type).SET-SELF($elems)
          ),
          nqp::create(type)
        )
    }
    method Bag()     { self!BAGGIFY(Bag,     1) }
    method BagHash() { self!BAGGIFY(BagHash, 0) }
    method Mix()     { self!BAGGIFY(Mix,     1) }
    method MixHash() { self!BAGGIFY(MixHash, 0) }

    method raw_hash() is raw { %!elems }

    # TODO: WHICH will require the capability for >1 pointer in ObjAt
}

# vim: ft=perl6 expandtab sw=4
