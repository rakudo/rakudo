my role Hash::Typed[::TValue, ::TKey, ::TDefault = TValue] does Associative[TValue] {

    # make sure we get the right descriptor
    multi method new(::?CLASS:) {
        nqp::p6bindattrinvres(
          nqp::create(self),Hash,'$!descriptor',
          ContainerDescriptor.new(:of(TValue), :default(TDefault))
        )
    }

    method ASSIGN-KEY(::?CLASS:D: Mu \key, Mu \assignval) is raw {
        my \storage  := nqp::getattr(self, Map, '$!storage');
        my \dkey     := nqp::decont(key);
        my str $which = nqp::istype(dkey,Str) && nqp::isconcrete(dkey)
          ?? nqp::unbox_s(dkey)
          !! dkey.Str;
        my \existing := nqp::atkey(storage,$which);
        nqp::if(
          nqp::isnull(existing),
          nqp::bindkey(storage,$which,
            nqp::p6assign(
              nqp::p6scalarfromdesc(nqp::getattr(self,Hash,'$!descriptor')),
              assignval
            )
          ),
          nqp::if(
            nqp::iscont(existing),
            nqp::p6assign(existing,assignval),
            nqp::p6store(existing,assignval)
          )
        )
    }

    method BIND-KEY(Mu \key, TValue \value) is raw {
        my \dkey := nqp::decont(key);
        my str $which = nqp::istype(dkey,Str) && nqp::isconcrete(dkey)
          ?? nqp::unbox_s(dkey)
          !! dkey.Str;
        nqp::bindkey(nqp::getattr(self,Map,'$!storage'),$which,value)
    }

    method is-generic {
        nqp::hllbool(callsame() || nqp::istrue(TValue.^archetypes.generic))
    }

    multi method INSTANTIATE-GENERIC(::?CLASS:U: TypeEnv:D \type-environment --> Associative) is raw {
        self.^mro.first({ !(.^is_mixin && .is-generic) }).^parameterize: type-environment.instantiate(TValue)
    }
    multi method INSTANTIATE-GENERIC(::?CLASS:D: TypeEnv:D \type-environment --> Associative) is raw {
        # Dispatch to the :U candidate via .WHAT - calling
        # `self.INSTANTIATE-GENERIC` from a :D invocant would re-enter this
        # same :D candidate and spin forever.
        my \ins-hash = self.WHAT.INSTANTIATE-GENERIC(type-environment);
        my Mu $descr := type-environment.instantiate( nqp::getattr(self, Hash, '$!descriptor') );
        nqp::p6bindattrinvres((self.elems ?? ins-hash.new(self) !! ins-hash.new), Hash, '$!descriptor', $descr )
    }

    multi method raku(::?CLASS:D \SELF:) {
        SELF.rakuseen('Hash', {
            '$' x nqp::iscont(SELF)  # self is always deconted
            ~ (self.elems
               ?? "(my {TValue.raku} % = {
                    self.sort.map({.raku}).join(', ')
                   })"
               !! "(my {TValue.raku} %)"
              )
        })
    }
}

# vim: expandtab shiftwidth=4
