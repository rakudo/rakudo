# this is actually part of the Array class

    my role Shaped2Array does ShapedArray {
        multi method AT-POS(::?CLASS:D: int \one, int \two) is raw {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              AT-POS-CONTAINER(self,one, two)
            )
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two) is raw {
            nqp::ifnull(
              nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two),
              AT-POS-CONTAINER(self,one, two)
            )
        }
        sub AT-POS-CONTAINER(\array, int \one, int \two) is raw {
            nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPos2D.new(
               nqp::getattr(array, Array, '$!descriptor'),
               nqp::getattr(array, List, '$!reified'),
               one, two))
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

        multi method DELETE-POS(::?CLASS:D: int \one, int \two) is raw {
            nqp::if(
              nqp::isnull(my $value := nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two)),
              Nil,
              nqp::stmts(
                nqp::bindpos2d(
                  nqp::getattr(self,List,'$!reified'),
                  one, two, nqp::null),
                $value
              )
            )
        }
        multi method DELETE-POS(::?CLASS:D: Int:D \one, Int:D \two) is raw {
            nqp::if(
              nqp::isnull(my $value := nqp::atpos2d(
                nqp::getattr(self,List,'$!reified'),
                one, two)),
              Nil,
              nqp::stmts(
                nqp::bindpos2d(
                  nqp::getattr(self,List,'$!reified'),
                  one, two, nqp::null),
                $value
              )
            )
        }

        multi method BIND-POS(::?CLASS:D: int \one, int \two, \value) {
            nqp::bindpos2d(
              nqp::getattr(self,List,'$!reified'),
              one, two, value
            )
        }
        multi method BIND-POS(::?CLASS:D: Int:D \one, Int:D \two, \value) {
            nqp::bindpos2d(
              nqp::getattr(self,List,'$!reified'),
              one, two, value
            )
        }
    }

# vim: ft=perl6 expandtab sw=4
