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
    method BIND-KEY(\key, TValue \value) is raw {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          key.Str,
          value
        )
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
