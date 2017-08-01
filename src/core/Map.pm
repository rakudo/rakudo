my class X::Hash::Store::OddNumber { ... }

my class Map does Iterable does Associative { # declared in BOOTSTRAP
    # my class Map is Iterable is Cool
    #   has Mu $!storage;

    multi method WHICH(Map:D:) {
        (nqp::istype(self.WHAT,Map) ?? 'Map|' !! (self.^name ~ '|'))
          ~ self.keys.sort.map( { $_.WHICH ~ '(' ~ self.AT-KEY($_) ~ ')' } )
    }
    method new(*@args) {
        @args
          ?? nqp::create(self).STORE(@args)
          !! nqp::create(self)
    }

    multi method Map(Map:) { self }

    multi method Hash(Map:U:) { Hash }
    multi method Hash(Map:D:) {
        if nqp::defined($!storage) && nqp::elems($!storage) {
            my $hash       := nqp::create(Hash);
            my $storage    := nqp::bindattr($hash,Map,'$!storage',nqp::hash);
            my $descriptor := nqp::null;
            my $iter       := nqp::iterator(nqp::getattr(self,Map,'$!storage'));
            nqp::while(
              $iter,
              nqp::bindkey($storage,nqp::iterkey_s(nqp::shift($iter)),
                nqp::p6scalarfromdesc($descriptor) =
                  nqp::decont(nqp::iterval($iter))
              )
            );
            $hash
        }
        else {
            nqp::create(Hash)
        }
    }

    multi method Bool(Map:D:) {
        nqp::p6bool(nqp::defined($!storage) && nqp::elems($!storage));
    }
    method elems(Map:D:) {
        nqp::p6box_i(nqp::defined($!storage) && nqp::elems($!storage));
    }
    multi method Int(Map:D:)     { self.elems }
    multi method Numeric(Map:D:) { self.elems }
    multi method Str(Map:D:)     { self.sort.join("\n") }

    method IterationBuffer() {
        nqp::stmts(
          (my $buffer := nqp::create(IterationBuffer)),
          nqp::if(
            nqp::defined($!storage) && nqp::elems($!storage),
            nqp::stmts(
              (my $iterator := nqp::iterator($!storage)),
              nqp::setelems($buffer,nqp::elems($!storage)),
              (my int $i = -1),
              nqp::while(
                $iterator,
                nqp::bindpos($buffer,($i = nqp::add_i($i,1)),
                  Pair.new(
                    nqp::iterkey_s(nqp::shift($iterator)),
                    nqp::iterval($iterator)
                  )
                )
              )
            )
          ),
          $buffer
        )
    }

    method List() {
        nqp::p6bindattrinvres(
          nqp::create(List),List,'$!reified',self.IterationBuffer)
    }

    multi method sort(Map:D:) {
        Seq.new(
          Rakudo::Iterator.ReifiedList(
            Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
              nqp::p6bindattrinvres(
                nqp::create(List),List,'$!reified',self.IterationBuffer
              ),
              { nqp::getattr(nqp::decont($^a),Pair,'$!key') }
            )
          )
        )
    }

    multi method ACCEPTS(Map:D: Any $topic) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(Map:D: Cool:D $topic) {
        self.EXISTS-KEY($topic);
    }

    multi method ACCEPTS(Map:D: Positional $topic) {
        self.EXISTS-KEY($topic.any);
    }

    multi method ACCEPTS(Map:D: Regex $topic) {
        so self.keys.any.match($topic);
    }

    multi method ACCEPTS(Map:D: Map:D \m --> Bool) {
    	self eqv m;
    }

    multi method EXISTS-KEY(Map:D: Str:D \key) {
        nqp::p6bool(
          nqp::defined($!storage) && nqp::existskey($!storage,key)
        )
    }
    multi method EXISTS-KEY(Map:D: \key) {
        nqp::p6bool(
          nqp::defined($!storage) && nqp::existskey($!storage,key.Str)
        )
    }

    multi method perl(Map:D:) {
        self.^name
          ~ '.new(('
          ~ self.sort.map({.perl}).join(',')
          ~ '))';
    }

    method iterator(Map:D:) {
        class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
                nqp::if(
                  $!iter,
                  nqp::stmts(
                    nqp::shift($!iter),
                    Pair.new(nqp::iterkey_s($!iter), nqp::iterval($!iter))
                  ),
                  IterationEnd
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                     nqp::shift($!iter),
                     $target.push(
                       Pair.new(nqp::iterkey_s($!iter), nqp::iterval($!iter)))
                  )
                )
            }
        }.new(self)
    }
    method list(Map:D:) { Seq.new(self.iterator) }
    multi method pairs(Map:D:) { Seq.new(self.iterator) }
    multi method keys(Map:D:) { Seq.new(Rakudo::Iterator.Mappy-keys(self)) }
    multi method values(Map:D:) { Seq.new(Rakudo::Iterator.Mappy-values(self)) }

    multi method kv(Map:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            has int $!on-value;

            method pull-one() is raw {
                nqp::if(
                  $!on-value,
                  nqp::stmts(
                    ($!on-value = 0),
                    nqp::iterval($!iter)
                  ),
                  nqp::if(
                    $!iter,
                    nqp::stmts(
                      ($!on-value = 1),
                      nqp::iterkey_s(nqp::shift($!iter))
                    ),
                    IterationEnd
                  )
                )
            }
            method skip-one() {
                nqp::if(
                  $!on-value,
                  nqp::not_i($!on-value = 0), # skipped a value
                  nqp::if(
                    $!iter,                   # if false, we didn't skip
                    nqp::stmts(               # skipped a key
                      nqp::shift($!iter),
                      ($!on-value = 1)
                    )
                  )
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::while(  # doesn't sink
                  $!iter,
                  nqp::stmts(
                    $target.push(nqp::iterkey_s(nqp::shift($!iter))),
                    $target.push(nqp::iterval($!iter))
                  )
                )
            }
            method count-only() {
                nqp::mul_i(nqp::elems($!hash),2)
            }
        }.new(self))
    }
    multi method antipairs(Map:D:) {
        Seq.new(class :: does Rakudo::Iterator::Mappy {
            method pull-one() {
                nqp::if(
                  $!iter,
                  nqp::stmts(
                    nqp::shift($!iter),
                    Pair.new( nqp::iterval($!iter), nqp::iterkey_s($!iter) )
                  ),
                  IterationEnd
                );
            }
            method push-all($target --> IterationEnd) {
                nqp::while(
                  $!iter,
                  nqp::stmts(  # doesn't sink
                    nqp::shift($!iter),
                    $target.push(
                      Pair.new( nqp::iterval($!iter), nqp::iterkey_s($!iter) ))
                  )
                )
            }
        }.new(self))
    }
    multi method invert(Map:D:) {
        Seq.new(Rakudo::Iterator.Invert(self.iterator))
    }

    multi method AT-KEY(Map:D: Str:D \key) is raw {
        nqp::defined($!storage)
          ?? nqp::ifnull(nqp::atkey($!storage,nqp::unbox_s(key)),Nil)
          !! Nil
    }
    multi method AT-KEY(Map:D: \key) is raw {
        nqp::defined($!storage)
          ?? nqp::ifnull(nqp::atkey($!storage,nqp::unbox_s(key.Str)),Nil)
          !! Nil
    }

    method !STORE_MAP(\map --> Nil) {
        nqp::if(
          nqp::defined(my $other := nqp::getattr(map,Map,'$!storage')),
          nqp::stmts(
            (my $iter := nqp::iterator($other)),
            nqp::while(
              $iter,
              self.STORE_AT_KEY(
                nqp::iterkey_s(nqp::shift($iter)),nqp::iterval($iter)
              )
            )
          )
        )
    }

    method STORE(\to_store) {
        my $temp := nqp::p6bindattrinvres(
          nqp::clone(self),   # make sure we get a possible descriptor as well
          Map,
          '$!storage',
          my $storage := nqp::hash
        );
        my $iter := to_store.iterator;
        my Mu $x;
        my Mu $y;

        nqp::until(
          nqp::eqaddr(($x := $iter.pull-one),IterationEnd),
          nqp::if(
            nqp::istype($x,Pair),
            $temp.STORE_AT_KEY(
              nqp::getattr(nqp::decont($x),Pair,'$!key'),
              nqp::getattr(nqp::decont($x),Pair,'$!value')
            ),
            nqp::if(
              (nqp::istype($x,Map) && nqp::not_i(nqp::iscont($x))),
              $temp!STORE_MAP($x),
              nqp::if(
                nqp::eqaddr(($y := $iter.pull-one),IterationEnd),
                nqp::if(
                  nqp::istype($x,Failure),
                  $x.throw,
                  X::Hash::Store::OddNumber.new(
                    found => nqp::add_i(nqp::mul_i(nqp::elems($storage),2),1),
                    last  => $x
                  ).throw
                ),
                $temp.STORE_AT_KEY($x,$y)
              )
            )
          )
        );

        nqp::p6bindattrinvres(self,Map,'$!storage',$storage)
    }

    proto method STORE_AT_KEY(|) { * }
    multi method STORE_AT_KEY(Str:D \key, Mu \value --> Nil) {
        nqp::bindkey($!storage, nqp::unbox_s(key), nqp::decont(value))
    }
    multi method STORE_AT_KEY(\key, Mu \value --> Nil) {
        nqp::bindkey($!storage, nqp::unbox_s(key.Str), nqp::decont(value))
    }

    method Capture(Map:D:) {
        nqp::defined($!storage)
          ?? nqp::p6bindattrinvres(
               nqp::create(Capture),Capture,'%!hash',$!storage)
          !! nqp::create(Capture)
    }

    method FLATTENABLE_LIST() { nqp::list() }
    method FLATTENABLE_HASH() {
        nqp::defined($!storage)
          ?? $!storage
          !! nqp::bindattr(self,Map,'$!storage',nqp::hash)
    }

    method fmt(Map: Cool $format = "%s\t\%s", $sep = "\n") {
        nqp::iseq_i(nqp::sprintfdirectives( nqp::unbox_s($format.Stringy)),1)
          ?? self.keys.fmt($format, $sep)
          !! self.pairs.fmt($format, $sep)
    }

    method hash() { self }
    method clone(Map:D:) is raw { self }

    multi method roll(Map:D:) {
        nqp::if(
          $!storage && nqp::elems($!storage),
          nqp::stmts(
            (my int $i = nqp::add_i(nqp::elems($!storage).rand.floor,1)),
            (my $iter := nqp::iterator($!storage)),
            nqp::while(
              nqp::shift($iter) && ($i = nqp::sub_i($i,1)),
              nqp::null
            ),
            Pair.new(nqp::iterkey_s($iter),nqp::iterval($iter))
          ),
          Nil
        )
    }
    multi method roll(Map:D: Callable:D $calculate) {
        self.roll( $calculate(self.elems) )
    }
    multi method roll(Map:D: Whatever $) { self.roll(Inf) }
    multi method roll(Map:D: $count) {
        Seq.new(nqp::if(
          $!storage && nqp::elems($!storage) && $count > 0,
          class :: does Iterator {
              has $!storage;
              has $!keys;
              has $!pairs;
              has $!count;

              method !SET-SELF(\hash,\count) {
                  nqp::stmts(
                    ($!storage := nqp::getattr(hash,Map,'$!storage')),
                    ($!count = $count),
                    (my int $i = nqp::elems($!storage)),
                    (my $iter := nqp::iterator($!storage)),
                    ($!keys := nqp::setelems(nqp::list_s,$i)),
                    ($!pairs := nqp::setelems(nqp::list,$i)),
                    nqp::while(
                      nqp::isge_i(($i = nqp::sub_i($i,1)),0),
                      nqp::bindpos_s($!keys,$i,
                        nqp::iterkey_s(nqp::shift($iter)))
                    ),
                    self
                  )
              }
              method new(\h,\c) { nqp::create(self)!SET-SELF(h,c) }
              method pull-one() {
                  nqp::if(
                    $!count,
                    nqp::stmts(
                      --$!count,  # must be HLL to handle Inf
                      nqp::ifnull(
                        nqp::atpos(
                          $!pairs,
                          (my int $i = nqp::elems($!keys).rand.floor)
                        ),
                        nqp::bindpos($!pairs,$i,
                          Pair.new(
                            nqp::atpos_s($!keys,$i),
                            nqp::atkey($!storage,nqp::atpos_s($!keys,$i))
                          )
                        )
                      )
                    ),
                    IterationEnd
                  )
              }
              method is-lazy() { $!count == Inf }
          }.new(self,$count),
          Rakudo::Iterator.Empty
        ))
    }

    multi method pick(Map:D:) { self.roll }

    multi method Set(Map:D:)     {
        nqp::create(Set).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-SET(self))
    }
    multi method SetHash(Map:D:)     {
        nqp::create(SetHash).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-SET(self))
    }
    multi method Bag(Map:D:)     {
        nqp::create(Bag).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-BAG(self))
    }
    multi method BagHash(Map:D:)     {
        nqp::create(BagHash).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-BAG(self))
    }
    multi method Mix(Map:D:)     {
        nqp::create(Mix).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-MIX(self))
    }
    multi method MixHash(Map:D:)     {
        nqp::create(MixHash).SET-SELF(Rakudo::QuantHash.COERCE-MAP-TO-MIX(self))
    }
}

multi sub infix:<eqv>(Map:D \a, Map:D \b) {

    class NotEQV { }

    nqp::p6bool(
      nqp::unless(
        nqp::eqaddr(a,b),
        nqp::if(                                 # not comparing with self
          nqp::eqaddr(a.WHAT,b.WHAT),
          nqp::if(                               # same types
            (my $amap := nqp::getattr(nqp::decont(a),Map,'$!storage'))
              && (my int $elems = nqp::elems($amap)),
            nqp::if(                             # elems on left
              (my $bmap := nqp::getattr(nqp::decont(b),Map,'$!storage'))
                && nqp::iseq_i($elems,nqp::elems($bmap)),
              nqp::stmts(                        # same elems on right
                (my $iter := nqp::iterator($amap)),
                nqp::while(
                  $iter && infix:<eqv>(
                    nqp::iterval(nqp::shift($iter)),
                    nqp::ifnull(nqp::atkey($bmap,nqp::iterkey_s($iter)),NotEQV)
                  ),
                  ($elems = nqp::sub_i($elems,1))
                ),
                nqp::not_i($elems)               # ok if none left
              )
            ),
            nqp::isfalse(                        # nothing on left
              ($bmap := nqp::getattr(nqp::decont(b),Map,'$!storage'))
                && nqp::elems($bmap)             # something on right: fail
            )
          )
        )
      )
    )
}

# vim: ft=perl6 expandtab sw=4
