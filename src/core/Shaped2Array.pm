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
    }

# vim: ft=perl6 expandtab sw=4
