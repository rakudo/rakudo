my class X::Cannot::Lazy { ... }
my class X::NotEnoughDimensions { ... }
my class X::TooManyDimensions { ... }
my class X::TypeCheck::Binding { ... }

# A hash with a shape of more than one dimension, keyed by a list holding a
# key of the type of each dimension. It is an object hash keyed by List, whose
# entries are stored under a string joined from the WHICH of each of those keys.
my role Hash::Shaped[::TValue, ::TDefault, *@KEYS]
  does Hash::Object[TValue, List, TDefault] {
    my $types := nqp::list;
    nqp::push($types, nqp::decont($_)) for @KEYS;
    my int $dims = nqp::elems($types);

    # The keys a key gives for the dimensions, which only a list or a Seq of
    # them gives for more than one
    my sub key-parts(\key) is raw {
        key.throw if nqp::istype(key,Failure) && nqp::isconcrete(key);
        nqp::istype(key,List) || nqp::istype(key,Seq)
          ?? key.is-lazy
            ?? X::Cannot::Lazy.new(:action('key a shaped hash with')).throw
            !! nqp::istype(key,Array)
              ?? MD-VALUES(key)
              !! key.cache.eager
          !! nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',nqp::list(key))
    }

    # The key of a dimension, coerced to or checked against its type
    my sub shaped-part(int $dim, Mu \part) is raw {
        part.throw if nqp::istype(part,Failure) && nqp::isconcrete(part);
        my \type  := nqp::atpos($types,$dim);
        my \value := type.HOW.archetypes(type).coercive
          ?? nqp::decont(type.HOW.coerce(type,part))
          !! part;
        value.throw if nqp::istype(value,Failure) && nqp::isconcrete(value);
        X::TypeCheck::Binding.new(
          :got(value), :expected(type), :symbol("key of dimension {$dim + 1}")
        ).throw unless nqp::istype(value,type);
        value
    }

    # The key list and the storage key for the keys of the dimensions. A
    # junction in a dimension that does not take one yields the index of that
    # dimension instead.
    my sub shaped-key(\keys, str $operation) {
        my \parts := nqp::ifnull(nqp::getattr(keys,List,'$!reified'),nqp::list);
        my int $elems = nqp::elems(parts);
        X::NotEnoughDimensions.new(
          :$operation, :aggregate<hash>,
          :got-dimensions($elems), :needed-dimensions($dims)
        ).throw if $elems < $dims;
        X::TooManyDimensions.new(
          :$operation, :aggregate<hash>,
          :got-dimensions($elems), :needed-dimensions($dims)
        ).throw if $elems > $dims;

        my \list := nqp::setelems(nqp::list,$dims);
        my str $which;
        my int $i = -1;
        while ++$i < $dims {
            my \part := nqp::decont(nqp::atpos(parts,$i));
            return $i if nqp::istype(part,Junction)
              && nqp::not_i(nqp::istype(part,nqp::atpos($types,$i)));
            my \value := shaped-part($i, part);
            nqp::bindpos(list,$i,value);
            my str $part-which = value.WHICH;
            $which = $which ~ nqp::chars($part-which) ~ ':' ~ $part-which;
        }
        Pair.new(
          nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',list),
          $which
        )
    }

    # The result of a call for every eigenstate of a junction in the keys
    my sub thread(\keys, int $dim, &call) {
        my \parts := nqp::getattr(keys,List,'$!reified');
        nqp::atpos(parts,$dim).THREAD(-> \eigenstate {
            my \threaded := nqp::clone(parts);
            nqp::bindpos(threaded,$dim,eigenstate);
            call(nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',threaded))
        })
    }

    # The key of a dimension as the hash takes it, for a slice to match
    method SHAPED-KEY-PART(int $dim, Mu \part) is raw is implementation-detail {
        shaped-part($dim, part)
    }

    # The entries keyed by the WHICH of their lists of keys, as an object hash
    # stores them, for code that reads the storage of one directly
    method WHICH-KEYED(::?CLASS:D:) is implementation-detail {
        my \storage := nqp::hash;
        my \iter    := nqp::iterator(nqp::getattr(self,Map,'$!storage'));
        nqp::while(
          iter,
          nqp::bindkey(
            storage,
            nqp::getattr(
              (my \pair := nqp::iterval(nqp::shift(iter))),Pair,'$!key'
            ).WHICH,
            pair
          )
        );
        nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',storage)
    }

    method SHAPED-DIMENSIONS() is implementation-detail { $dims }

    method shape() { @KEYS.List }

    # Sorting safely sorts by the gist of each list of keys, as a junction or
    # a type object among them does not stringify
    multi method sort(::?CLASS:D: Bool :$safe --> Seq:D) {
        Seq.new(
          Rakudo::Iterator.ReifiedList(
            Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
              self.IterationBuffer.List,
              $safe ?? { .key.gist } !! { .key }
            )
          )
        )
    }

    method AT-KEY(\SELF: \key) is raw {
        my \parts  := key-parts(key);
        my \shaped := shaped-key(parts, 'access');
        nqp::if(
          nqp::istype(shaped,Int),
          thread(parts, shaped, { SELF.AT-KEY($_) }),
          nqp::if(
            nqp::isconcrete(SELF),
            nqp::if(
              nqp::isnull(my \existing := nqp::atkey(
                nqp::getattr(self,Map,'$!storage'),shaped.value
              )),
              nqp::p6scalarfromdesc(
                ContainerDescriptor::BindObjHashKey.new(
                  nqp::getattr(self,Hash,'$!descriptor'),
                  self, shaped.key, shaped.value, Pair
                )
              ),
              nqp::getattr(existing,Pair,'$!value')
            ),
            nqp::p6scalarfromcertaindesc(
              ContainerDescriptor::VivifyHash.new(SELF, shaped.key)
            )
          )
        )
    }

    method STORE_AT_KEY(::?CLASS:D: \key, Mu \value --> Nil) {
        my \parts  := key-parts(key);
        my \shaped := shaped-key(parts, 'assign to');
        nqp::istype(shaped,Int)
          ?? thread(parts, shaped, { self.STORE_AT_KEY($_, value) })
          !! nqp::bindkey(
               nqp::getattr(self,Map,'$!storage'),
               shaped.value,
               Pair.new(
                 shaped.key,
                 nqp::p6scalarfromdesc(nqp::getattr(self,Hash,'$!descriptor'))
                 = value
               )
             )
    }

    method ASSIGN-KEY(\SELF: \key, Mu \assignval) is raw {
        my \parts  := key-parts(key);
        my \shaped := shaped-key(parts, 'assign to');
        nqp::if(
          nqp::istype(shaped,Int),
          thread(parts, shaped, { SELF.ASSIGN-KEY($_, assignval) }),
          nqp::if(
            nqp::isconcrete(SELF),
            nqp::if(
              nqp::isnull(my \existing := nqp::atkey(
                (my \storage := nqp::getattr(self,Map,'$!storage')),
                shaped.value
              )),
              nqp::stmts(
                ((my \scalar := nqp::p6scalarfromdesc(    # assign before
                  nqp::getattr(self,Hash,'$!descriptor')  # binding to get
                )) = assignval),                          # type check
                nqp::bindkey(storage,shaped.value,Pair.new(shaped.key,scalar)),
                scalar
              ),
              (nqp::getattr(existing,Pair,'$!value') = assignval)
            ),
            nqp::findmethod(Hash,'VIVIFY-ASSIGN-KEY')(SELF, shaped.key, assignval)
          )
        )
    }

    method BIND-KEY(\SELF: \key, TValue \value) is raw {
        my \parts  := key-parts(key);
        my \shaped := shaped-key(parts, 'bind to');
        nqp::istype(shaped,Int)
          ?? thread(parts, shaped, { SELF.BIND-KEY($_, value) })
          !! nqp::isconcrete(SELF)
            ?? nqp::getattr(
                 nqp::bindkey(
                   nqp::getattr(self,Map,'$!storage'),
                   shaped.value,
                   Pair.new(shaped.key,value)
                 ),
                 Pair,
                 '$!value'
               )
            !! nqp::findmethod(Hash,'VIVIFY-BIND-KEY')(SELF, shaped.key, value)
    }

    method EXISTS-KEY(\key) {
        my \parts  := key-parts(key);
        my \shaped := shaped-key(parts, 'access');
        nqp::istype(shaped,Int)
          ?? thread(parts, shaped, { self.EXISTS-KEY($_) })
          !! nqp::hllbool(
               nqp::isconcrete(self)
                 && nqp::existskey(nqp::getattr(self,Map,'$!storage'),shaped.value)
             )
    }

    method DELETE-KEY(\key) {
        my \parts  := key-parts(key);
        my \shaped := shaped-key(parts, 'delete from');
        nqp::if(
          nqp::istype(shaped,Int),
          thread(parts, shaped, { self.DELETE-KEY($_) }),
          nqp::if(
            nqp::isconcrete(self),
            nqp::if(
              nqp::isnull(my \value := nqp::atkey(
                nqp::getattr(self,Map,'$!storage'),shaped.value
              )),
              nqp::getattr(self,Hash,'$!descriptor').default,
              nqp::stmts(
                nqp::deletekey(nqp::getattr(self,Map,'$!storage'),shaped.value),
                nqp::getattr(value,Pair,'$!value')
              )
            ),
            Nil
          )
        )
    }

    method is-generic {
        my int $generic = callsame()
          || TValue.^archetypes.generic
          || TDefault.^archetypes.generic;
        my int $i = -1;
        nqp::while(
          nqp::not_i($generic) && nqp::islt_i(++$i,$dims),
          ($generic = nqp::atpos($types,$i).HOW.archetypes(nqp::atpos($types,$i)).generic)
        );
        nqp::hllbool($generic)
    }

    multi method INSTANTIATE-GENERIC(::?CLASS:U: TypeEnv:D \type-environment --> Associative) is raw {
        self.^mro.first({ !.^is_mixin }).^parameterize:
            type-environment.instantiate(TValue),
            @KEYS.map({ type-environment.instantiate($_) }).List,
            TDefault.^archetypes.generic
              ?? type-environment.instantiate(TDefault)
              !! TDefault
    }

    multi method INSTANTIATE-GENERIC(::?CLASS:D: TypeEnv:D \type-environment --> Associative) is raw {
        # Dispatch to the :U candidate through .WHAT, as calling it on a
        # defined invocant would enter this candidate again forever
        my \ins-hash = self.WHAT.INSTANTIATE-GENERIC(type-environment);
        my Mu $descr := type-environment.instantiate( nqp::getattr(self, Hash, '$!descriptor') );
        nqp::p6bindattrinvres((self.elems ?? ins-hash.new(self) !! ins-hash.new), Hash, '$!descriptor', $descr )
    }

    multi method raku(::?CLASS:D \SELF:) {
        SELF.rakuseen('Hash', {
            my $shape := @KEYS.map({ .raku }).join(';');
            '$' x nqp::iscont(SELF)
              ~ (self.elems
                  ?? "(my {TValue.raku} %\{$shape\} = {
                        self.sort.map({.raku}).join(', ')
                     })"
                  !! "(my {TValue.raku} %\{$shape\})"
                )
        })
    }
}

# vim: expandtab shiftwidth=4
