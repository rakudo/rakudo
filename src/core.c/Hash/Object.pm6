my role Hash::Object[::TValue, ::TKey] does Associative[TValue] {

    # make sure we get the right descriptor
    multi method new(::?CLASS:) {
        nqp::p6bindattrinvres(
          nqp::create(self),Hash,'$!descriptor',
          ContainerDescriptor.new(:of(TValue), :default(TValue))
        )
    }
    method keyof () { TKey }
    method AT-KEY(::?CLASS:D: TKey \key) is raw {
        my \storage := nqp::getattr(self, Map, '$!storage');
        my str $which = nqp::unbox_s(key.WHICH);
        nqp::existskey(storage,$which)
          ?? nqp::getattr(nqp::atkey(storage,$which),Pair,'$!value')
          !! nqp::p6scalarfromdesc(
               ContainerDescriptor::BindObjHashKey.new(
                 nqp::getattr(self,Hash,'$!descriptor'),
                 self, key, $which, Pair
               )
             )
    }

    method STORE_AT_KEY(::?CLASS:D: TKey \key, Mu \value --> Nil) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          nqp::unbox_s(key.WHICH),
          Pair.new(
            key,
            nqp::p6scalarfromdesc(nqp::getattr(self,Hash,'$!descriptor'))
            = value
          )
        )
    }

    method ASSIGN-KEY(::?CLASS:D: TKey \key, Mu \assignval) is raw {
        my \storage  := nqp::getattr(self, Map, '$!storage');
        my \WHICH    := key.WHICH;
        my \existing := nqp::atkey(storage,WHICH);
        nqp::if(
          nqp::isnull(existing),
          nqp::stmts(
            ((my \scalar := nqp::p6scalarfromdesc(    # assign before
              nqp::getattr(self,Hash,'$!descriptor')  # binding to get
            )) = assignval),                          # type check
            nqp::bindkey(storage,WHICH,Pair.new(key,scalar)),
            scalar
          ),
          (nqp::getattr(existing,Pair,'$!value') = assignval)
        )
    }

    method BIND-KEY(TKey \key, TValue \value) is raw {
        nqp::getattr(
          nqp::bindkey(
            nqp::getattr(self,Map,'$!storage'),
            key.WHICH,
            Pair.new(key,value)
          ),
          Pair,
          '$!value'
        )
    }

    method EXISTS-KEY(TKey \key) {
        nqp::hllbool(
          nqp::existskey(nqp::getattr(self,Map,'$!storage'),key.WHICH)
        )
    }

    method DELETE-KEY(TKey \key) {
        nqp::if(
          nqp::isnull(my \value := nqp::atkey(
            nqp::getattr(self,Map,'$!storage'),
            (my str $WHICH = key.WHICH)
          )),
          TValue,
          nqp::stmts(
            nqp::deletekey(nqp::getattr(self,Map,'$!storage'),$WHICH),
            nqp::getattr(value,Pair,'$!value')
          )
        )
    }

    method FLATTENABLE_HASH() {
        my $flattened := nqp::hash;
        nqp::if(
          (my $iter := nqp::iterator(nqp::getattr(self,Map,'$!storage'))),
          nqp::while(
            $iter,
            nqp::bindkey(
              $flattened,
              nqp::if(
                nqp::istype(
                  (my $key := nqp::getattr(
                    nqp::iterval(nqp::shift($iter)),
                    Pair,
                    '$!key'
                  )),
                  Str,
                ),
                $key,
                $key.Str
              ),
              nqp::getattr(nqp::iterval($iter),Pair,'$!value')
            )
          )
        );
        $flattened
    }

    method IterationBuffer() {
        my \storage := nqp::getattr(self, Map, '$!storage');
        my \buffer  := nqp::create(IterationBuffer);
        nqp::if(
          nqp::elems(storage),
          nqp::stmts(
            (my \iterator := nqp::iterator(storage)),
            nqp::setelems(buffer,nqp::elems(storage)),
            (my int $i = -1),
            nqp::while(
              iterator,
              nqp::bindpos(buffer,($i = nqp::add_i($i,1)),
                nqp::iterval(nqp::shift(iterator)))
            )
          )
        );
        buffer
    }

    multi method head(::?CLASS:D:) {
        my \storage := nqp::getattr(self, Map, '$!storage');
        nqp::elems(storage)
          ?? nqp::iterval(nqp::shift(nqp::iterator(storage)))
          !! Nil
    }

    multi method sort(::?CLASS:D: --> Seq:D) {
        Seq.new(
          Rakudo::Iterator.ReifiedList(
            Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
              self.IterationBuffer.List,
              *.key
            )
          )
        )
    }

    my class Keys does Rakudo::Iterator::Mappy {
        method pull-one() {
            $!iter
              ?? nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!key')
              !! IterationEnd
         }
    }
    method keys() { Seq.new(Keys.new(self)) }

    my class Values does Rakudo::Iterator::Mappy {
        method pull-one() is raw {
            $!iter
              ?? nqp::getattr(nqp::iterval(nqp::shift($!iter)),Pair,'$!value')
              !! IterationEnd
         }
    }
    method values() { Seq.new(Values.new(self)) }

    method kv() {
        Seq.new(Rakudo::Iterator.Mappy-kv-from-pairs(self))
    }
    method iterator() { Rakudo::Iterator.Mappy-values(self) }

    my class AntiPairs does Rakudo::Iterator::Mappy {
        method pull-one() {
            $!iter
              ?? nqp::iterval(nqp::shift($!iter)).antipair
              !! IterationEnd
         }
    }
    method antipairs() { Seq.new(AntiPairs.new(self)) }

    multi method roll(::?CLASS:D:) {
        my \storage := nqp::getattr(self, Map, '$!storage');
        nqp::if(
          nqp::elems(storage),
          nqp::stmts(
            (my int $i =
              nqp::add_i(nqp::floor_n(nqp::rand_n(nqp::elems(storage))),1)),
            (my \iter := nqp::iterator(storage)),
            nqp::while(
              nqp::shift(iter) && ($i = nqp::sub_i($i,1)),
              nqp::null
            ),
            nqp::iterval(iter)
          ),
          Nil
        )
    }
    multi method roll(::?CLASS:D: Callable:D $calculate) {
        self.roll( $calculate(self.elems) )
    }
    multi method roll(::?CLASS:D: Whatever $) { self.roll(Inf) }

    my class RollN does Iterator {
        has $!storage;
        has $!keys;
        has $!count;

        method !SET-SELF(\hash,\count) {
            $!storage := nqp::getattr(hash,Map,'$!storage');
            $!count = count;
            my $iter := nqp::iterator($!storage);
            $!keys := nqp::list_s;
            nqp::while(
              $iter,
              nqp::push_s($!keys,nqp::iterkey_s(nqp::shift($iter)))
            );
            self
        }
        method new(\h,\c) { nqp::create(self)!SET-SELF(h,c) }
        method pull-one() {
            nqp::if(
              $!count,
              nqp::stmts(
                --$!count,  # must be HLL to handle Inf
                nqp::atkey(
                  $!storage,
                  nqp::atpos_s(
                    $!keys,
                    nqp::floor_n(nqp::rand_n(nqp::elems($!keys)))
                  )
                )
              ),
              IterationEnd
            )
        }
        method is-lazy() { $!count == Inf }
        method is-deterministic(--> False) { }
    }
    multi method roll(::?CLASS:D: $count) {
        Seq.new(
          $count > 0 && nqp::elems(nqp::getattr(self,Map,'$!storage'))
            ?? RollN.new(self, $count)
            !! Rakudo::Iterator.Empty
        )
    }

    multi method raku(::?CLASS:D \SELF:) {
        SELF.rakuseen('Hash', {
            my $TKey-perl   := TKey.raku;
            my $TValue-perl := TValue.raku;
            $TKey-perl eq 'Any' && $TValue-perl eq 'Mu'
              ?? ( '$(' x nqp::iscont(SELF)
                    ~ ':{' ~ SELF.sort.map({.raku}).join(', ') ~ '}'
                    ~ ')' x nqp::iscont(SELF)
                 )
              !! '$' x nqp::iscont(SELF)
                 ~ (self.elems
                      ?? "(my $TValue-perl %\{$TKey-perl\} = {
                            self.sort.map({.raku}).join(', ')
                         })"
                      !! "(my $TValue-perl %\{$TKey-perl\})"
                 )
        })
    }

    # gotta force capture keys to strings or binder fails
    method Capture() {
        nqp::elems(nqp::getattr(self,Map,'$!storage'))
          ?? do {
                 my $cap := nqp::create(Capture);
                 my $h := nqp::hash();
                 for self.kv -> \k, \v {
                     nqp::bindkey($h,
                       nqp::unbox_s(nqp::istype(k,Str) ?? k !! k.Str),
                       v)
                 }
                 nqp::bindattr($cap,Capture,'%!hash',$h);
                 $cap
             }
          !! nqp::create(Capture)
    }
    method Map() { self.pairs.Map }
}

# vim: expandtab shiftwidth=4
