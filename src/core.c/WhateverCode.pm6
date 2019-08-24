my class WhateverCode is Code {

    # helper method for array slicing
    multi method POSITIONS(WhateverCode:D: Failure:D \failure) { failure }
    multi method POSITIONS(WhateverCode:D $self: \list) {
      my \signature := nqp::getattr($self,Code,'$!signature');
      my \count := nqp::getattr(signature,Signature,'$!count');
      nqp::if(
        nqp::iseq_i(count,1),
        $self(nqp::if(nqp::isconcrete(list),list.elems,0)),
        $self(|(nqp::if(nqp::isconcrete(list),list.elems,0) xx count))
      )
    }
}

# vim: ft=perl6 expandtab sw=4
