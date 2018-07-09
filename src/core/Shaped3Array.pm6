# this is actually part of the Array class

    my role Shaped3Array does ShapedArray {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three) is raw {
            nqp::ifnull(
              nqp::atpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three),
              AT-POS-CONTAINER(self, one, two, three)
            )
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) is raw {
            nqp::ifnull(
              nqp::atpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three),
              AT-POS-CONTAINER(self, one, two, three)
            )
        }
        sub AT-POS-CONTAINER(\array, int \one, int \two, int \three) is raw {
            nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPos2D.new(
               nqp::getattr(array, Array, '$!descriptor'),
               nqp::getattr(array, List, '$!reified'),
               one, two, three))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, \value) {
            nqp::ifnull(
              nqp::atpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three),
              nqp::bindpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three,
                nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
            ) = value
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, \value) {
            nqp::ifnull(
              nqp::atpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three),
              nqp::bindpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three,
                nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
            ) = value
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three) {
            nqp::p6bool(
              nqp::stmts(
                (my $dims :=
                  nqp::dimensions(nqp::getattr(self,List,'$!reified'))),
                nqp::islt_i(one,nqp::atpos_i($dims,0))
                  && nqp::islt_i(two,nqp::atpos_i($dims,1))
                    && nqp::not_i(nqp::isnull(
                         nqp::atpos3d(
                           nqp::getattr(self,List,'$!reified'),
                           one, two, three)
                       ))
              )
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) {
            nqp::p6bool(
              nqp::stmts(
                (my $dims :=
                  nqp::dimensions(nqp::getattr(self,List,'$!reified'))),
                nqp::islt_i(one,nqp::atpos_i($dims,0))
                  && nqp::islt_i(two,nqp::atpos_i($dims,1))
                    && nqp::islt_i(three,nqp::atpos_i($dims,2))
                      && nqp::not_i(nqp::isnull(
                           nqp::atpos3d(
                             nqp::getattr(self,List,'$!reified'),
                             one, two, three)
                         ))
              )
            )
        }

        multi method DELETE-POS(::?CLASS:D: int \one, int \two, int \three) is raw {
            nqp::if(
              nqp::isnull(my $value := nqp::atpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three)),
              Nil,
              nqp::stmts(
                nqp::bindpos3d(
                  nqp::getattr(self,List,'$!reified'),
                  one, two, three, nqp::null),
                $value
              )
            )
        }
        multi method DELETE-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) is raw {
            nqp::if(
              nqp::isnull(my $value := nqp::atpos3d(
                nqp::getattr(self,List,'$!reified'),
                one, two, three)),
              Nil,
              nqp::stmts(
                nqp::bindpos3d(
                  nqp::getattr(self,List,'$!reified'),
                  one, two, three, nqp::null),
                $value
              )
            )
        }

        multi method BIND-POS(::?CLASS:D: int \one, int \two, int \three, \value) {
            nqp::bindpos3d(
              nqp::getattr(self,List,'$!reified'),
              one, two, three, value
            )
        }
        multi method BIND-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, \value) {
            nqp::bindpos3d(
              nqp::getattr(self,List,'$!reified'),
              one, two, three, value
            )
        }
    }

# vim: ft=perl6 expandtab sw=4
