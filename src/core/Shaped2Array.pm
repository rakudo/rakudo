    my role Shaped2Array[::TValue] does ShapedArray[TValue] {
        multi method AT-POS(::?CLASS:D: int \one, int \two) is raw {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc(
                  nqp::getattr(self,Array,'$!descriptor'))),
                Scalar,
                '$!whence',
                -> { nqp::bindpos2d(
                       nqp::getattr(self,List,'$!reified'),
                       one, two, v) }
              )
            )
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two) is raw {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              nqp::p6bindattrinvres(
                (my \v := nqp::p6scalarfromdesc(
                  nqp::getattr(self,Array,'$!descriptor'))),
                Scalar,
                '$!whence',
                -> { nqp::bindpos2d(
                       nqp::getattr(self,List,'$!reified'),
                       one, two, v) }
              )
            )
        }
    }

# vim: ft=perl6 expandtab sw=4
