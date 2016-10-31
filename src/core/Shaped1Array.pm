    my role Shaped1Array[::TValue] does ShapedArray[TValue] {
        multi method AT-POS(Array:D: int \one) is raw {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc(
                  nqp::getattr(self,Array,'$!descriptor'))),
                Scalar,
                '$!whence',
                -> { nqp::bindpos(
                       nqp::getattr(self,List,'$!reified'),
                       one, v) }
              )
            )
        }
        multi method AT-POS(Array:D: Int:D \one) is raw {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc(
                  nqp::getattr(self,Array,'$!descriptor'))),
                Scalar,
                '$!whence',
                -> { nqp::bindpos(
                       nqp::getattr(self,List,'$!reified'),
                       one, v) }
              )
            )
        }
    }

# vim: ft=perl6 expandtab sw=4
