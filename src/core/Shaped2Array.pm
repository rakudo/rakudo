# this is actually part of the Array class

    my role Shaped2Array[::TValue] does ShapedArray[TValue] {
        multi method AT-POS(::?CLASS:D: int \one, int \two) is raw {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              self!AT-POS-CONTAINER(one, two)
            )
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two) is raw {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              self!AT-POS-CONTAINER(one, two)
            )
        }
        method !AT-POS-CONTAINER(int \one, int \two) is raw {
            nqp::p6bindattrinvres(
              (my $scalar := nqp::p6scalarfromdesc(
                nqp::getattr(self,Array,'$!descriptor'))),
              Scalar,
              '$!whence',
              -> { nqp::bindpos2d(
                     nqp::getattr(self,List,'$!reified'),
                     one, two, $scalar) }
            )
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, \value) {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              nqp::bindpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two,
                nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
            ) = value
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, \value) {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              nqp::bindpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two,
                nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
            ) = value
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two) {
            nqp::p6bool(
              nqp::stmts(
                (my $dims :=
                  nqp::dimensions(nqp::getattr(self,List,'$!reified'))),
                nqp::islt_i(one,nqp::atpos_i($dims,0))
                  && nqp::islt_i(two,nqp::atpos_i($dims,1))
                    && nqp::not_i(nqp::isnull(
                         nqp::atpos2d(
                           nqp::getattr(self,List,'$!reified'),
                           one, two)
                       ))
              )
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two) {
            nqp::p6bool(
              nqp::stmts(
                (my $dims :=
                  nqp::dimensions(nqp::getattr(self,List,'$!reified'))),
                nqp::islt_i(one,nqp::atpos_i($dims,0))
                  && nqp::islt_i(two,nqp::atpos_i($dims,1))
                    && nqp::not_i(nqp::isnull(
                         nqp::atpos2d(
                           nqp::getattr(self,List,'$!reified'),
                           one, two)
                       ))
              )
            )
        }
    }

# vim: ft=perl6 expandtab sw=4
