my role Hash::Typed[::TValue] does Associative[TValue] {

    # make sure we get the right descriptor
    multi method new(::?CLASS:) {
        nqp::p6bindattrinvres(
          nqp::create(self),Hash,'$!descriptor',
          ContainerDescriptor.new(:of(TValue), :default(TValue))
        )
    }

    method ASSIGN-KEY(::?CLASS:D: Mu \key, Mu \assignval) is raw {
        my \storage  := nqp::getattr(self, Map, '$!storage');
        my \which    := key.Str;
        my \existing := nqp::atkey(storage,which);
        nqp::if(
          nqp::isnull(existing),
          nqp::stmts(
            ((my \scalar := nqp::p6scalarfromdesc(    # assign before
              nqp::getattr(self,Hash,'$!descriptor')  # binding to get
            )) = assignval),                          # type check
            nqp::bindkey(storage,which,scalar)
          ),
          (existing = assignval)
        )
    }

    method BIND-KEY(Mu \key, TValue \value) is raw {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          key.Str,
          value
        )
    }

    method is-generic {
        nqp::hllbool(callsame() || nqp::istrue(TValue.^archetypes.generic))
    }

    multi method INSTANTIATE-GENERIC(::?CLASS:U: TypeEnv:D \type-environment --> Associative) is raw {
        self.^mro.first({ !(.^is_mixin && .is-generic) }).^parameterize: type-environment.instantiate(TValue)
    }
    multi method INSTANTIATE-GENERIC(::?CLASS:D: TypeEnv:D \type-environment --> Associative) is raw {
        my \ins-hash = self.INSTANTIATE-GENERIC(type-environment);
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
