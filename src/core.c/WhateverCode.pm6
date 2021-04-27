my class WhateverCode is Code {

    # helper method for array slicing
    multi method POSITIONS(WhateverCode:D: Failure:D \failure) { failure }
    multi method POSITIONS(WhateverCode:D $self: \list) {
        nqp::isconcrete(list)
          ?? nqp::iseq_i(
               (my \count := nqp::getattr(
                 nqp::getattr($self,Code,'$!signature'),
                 Signature,
                 '$!count'
               )),
               1
             )
            ?? $self(list.elems)
            !! $self(|(list.elems xx count))
          !! $self(0)
    }

    multi method ACCEPTS(WhateverCode:D: \value) is raw {
        nqp::call(nqp::getattr(self,Code,'$!do'),value)
    }
}

# vim: expandtab shiftwidth=4
