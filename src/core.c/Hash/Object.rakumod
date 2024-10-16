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
        nqp::istype(key,Failure)
          ?? key.throw
          !! nqp::bindkey(
               nqp::getattr(self,Map,'$!storage'),
               nqp::unbox_s(key.WHICH),
               Pair.new(
                 key,
                 nqp::p6scalarfromdesc(nqp::getattr(self,Hash,'$!descriptor'))
                 = value
               )
             )
    }

    method PUSH_FROM_MAP(\target --> Nil) is implementation-detail {
        my $iter := nqp::iterator(nqp::getattr(self,Map,'$!storage'));
        nqp::while(
          $iter,
          nqp::stmts(
            (my $pair := nqp::iterval(nqp::shift($iter))),
            target.STORE_AT_KEY(
              nqp::getattr($pair,Pair,'$!key'),
              nqp::getattr($pair,Pair,'$!value'),
            )
          )
        );
    }

    method ASSIGN-KEY(::?CLASS:D: TKey \key, Mu \assignval) is raw {
        key.throw if nqp::istype(key,Failure);

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
        nqp::istype(key,Failure)
          ?? key.throw
          !! nqp::getattr(
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
        nqp::istype(key,Failure)
          ?? key.throw
          !! nqp::hllbool(
               nqp::existskey(nqp::getattr(self,Map,'$!storage'),key.WHICH)
             )
    }

    method DELETE-KEY(TKey \key) {
        key.throw if nqp::istype(key,Failure);

        nqp::if(
          nqp::isnull(my \value := nqp::atkey(
            nqp::getattr(self,Map,'$!storage'),
            (my str $WHICH = key.WHICH)
          )),
          nqp::getattr(self, Hash, '$!descriptor').default,
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
              nqp::bindpos(buffer,++$i,nqp::iterval(nqp::shift(iterator)))
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

    multi method sort(::?CLASS:D: Bool :$safe --> Seq:D) {
        # With :safe we don't sort directly over the keys but stringify them appropriately first. This is necessary
        # whenever a key could happen to be a Junction, in which case MERGESORT-REIFIED-LIST-AS would throw due to
        # autothreading.
        Seq.new(
          Rakudo::Iterator.ReifiedList(
            Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
              self.IterationBuffer.List,
              ($safe
                ?? { ($_ ~~ Junction ?? .gist !! .Stringy) with .key }
                !! *.key))))
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
              nqp::shift(iter) && --$i,
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

        method !SET-SELF(\hash, $count) {
            $!storage := nqp::getattr(hash,Map,'$!storage');
            $!count = $count;
            my $iter := nqp::iterator($!storage);
            $!keys := nqp::list_s;
            nqp::while(
              $iter,
              nqp::push_s($!keys,nqp::iterkey_s(nqp::shift($iter)))
            );
            self
        }
        method new(\hash, $count) { nqp::create(self)!SET-SELF(hash, $count) }
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

    method is-generic {
        nqp::hllbool(
            callsame()
            || TValue.^archetypes.generic
            || TKey.^archetypes.generic )
    }

    multi method INSTANTIATE-GENERIC(::?CLASS:U: TypeEnv:D \type-environment --> Associative) is raw {
        self.^mro.first({ !(.^is_mixin && .is-generic) }).^parameterize:
            type-environment.instantiate(TValue),
            type-environment.instantiate(TKey)
    }

    multi method INSTANTIATE-GENERIC(::?CLASS:D: TypeEnv:D \type-environment --> Associative) is raw {
        my \ins-hash = self.INSTANTIATE-GENERIC(type-environment);
        my Mu $descr := type-environment.instantiate( nqp::getattr(self, Hash, '$!descriptor') );
        nqp::p6bindattrinvres((self.elems ?? ins-hash.new(self) !! ins-hash.new), Hash, '$!descriptor', $descr )
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

    multi method gist(::?CLASS:D:) {
        self.gistseen: self.^name, {
            '{'
              ~ self.sort(:safe).head(100).map(*.gist).join(', ')
              ~ (', ...' if self.elems > 100)
              ~ '}'
        }
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

    method TEMP-LET-LOCALIZE() is raw is implementation-detail {
        my \handle = self.TEMP-LET-GET-HANDLE;
        my \iter = nqp::iterator(nqp::getattr(self, Map, '$!storage'));
        nqp::bindattr(self, Map, '$!storage', my \new-storage = nqp::hash);
        nqp::while(
            iter,
            nqp::stmts(
                nqp::shift(iter),
                # What we do here is very much stripped down versions of ASSIGN-KEY and BIND-KEY.
                (my \p = nqp::iterval(iter)),
                nqp::bindkey(
                    new-storage,
                    nqp::iterkey_s(iter),
                    Pair.new(
                        p.key,
                        nqp::if( nqp::isrwcont(my \v = p.value),
                                 nqp::p6assign(nqp::p6scalarfromdesc(nqp::getattr(self, Hash, '$!descriptor')), v),
                                 v )))));
        handle
    }
}

# vim: expandtab shiftwidth=4
