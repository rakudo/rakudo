# this is actually part of the Array class

    my role Shaped1Array[::TValue] does ShapedArray[TValue] {
        multi method AT-POS(::?CLASS:D: int \one) is raw {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              self!AT-POS-CONTAINER(one)
            )
        }
        multi method AT-POS(::?CLASS:D: Int:D \one) is raw {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              self!AT-POS-CONTAINER(one)
            )
        }
        method !AT-POS-CONTAINER(int \one) is raw {
            nqp::p6bindattrinvres(
              (my $scalar := nqp::p6scalarfromdesc(
                nqp::getattr(self,Array,'$!descriptor'))),
              Scalar,
              '$!whence',
              -> { nqp::bindpos(
                     nqp::getattr(self,List,'$!reified'),
                     one, $scalar) }
            )
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, \value) {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              nqp::bindpos(
                nqp::getattr(self,List,'$!reified'),
                one,
                nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
            ) = value
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, \value) {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              nqp::bindpos(
                nqp::getattr(self,List,'$!reified'),
                one,
                nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
            ) = value
        }

        multi method EXISTS-POS(::?CLASS:D: int \one) {
            nqp::p6bool(
              nqp::islt_i(one,nqp::elems(nqp::getattr(self,List,'$!reified')))
                && nqp::not_i(nqp::isnull(
                     nqp::atpos(nqp::getattr(self,List,'$!reified'),one)
              ))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one) {
            nqp::p6bool(
              nqp::islt_i(one,nqp::elems(nqp::getattr(self,List,'$!reified')))
                && nqp::not_i(nqp::isnull(
                     nqp::atpos(nqp::getattr(self,List,'$!reified'),one)
              ))
            )
        }

        multi method DELETE-POS(::?CLASS:D: int \one) is raw {
            nqp::if(
              nqp::isnull(my $value := nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one)),
              Nil,
              nqp::stmts(
                nqp::bindpos(
                  nqp::getattr(self,List,'$!reified'),
                  one, nqp::null),
                $value
              )
            )
        }
        multi method DELETE-POS(::?CLASS:D: Int:D \one) is raw {
            nqp::if(
              nqp::isnull(my $value := nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one)),
              Nil,
              nqp::stmts(
                nqp::bindpos(
                  nqp::getattr(self,List,'$!reified'),
                  one, nqp::null),
                $value
              )
            )
        }

        multi method BIND-POS(::?CLASS:D: int \one, \value) {
            nqp::bindpos(
              nqp::getattr(self,List,'$!reified'),
              one, value
            )
        }
        multi method BIND-POS(::?CLASS:D: Int:D \one, \value) {
            nqp::bindpos(
              nqp::getattr(self,List,'$!reified'),
              one, value
            )
        }

        multi method STORE(::?CLASS:D: Iterable:D \in) {
            nqp::stmts(
              (my \list := nqp::getattr(self,List,'$!reified')),
              (my \desc := nqp::getattr(self,Array,'$!descriptor')),
              (my \iter := in.iterator),
              (my int $elems = nqp::elems(list)),
              (my int $i = -1),
              nqp::until(
                nqp::eqaddr((my $pulled := iter.pull-one),IterationEnd)
                  || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                nqp::ifnull(
                  nqp::atpos(list,$i),
                  nqp::bindpos(list,$i,nqp::p6scalarfromdesc(desc))
                ) = $pulled
              ),
              nqp::unless(
                nqp::islt_i($i,$elems) || iter.is-lazy,
                nqp::atpos(list,$i) # too many values on non-lazy iter, error
              ),
              self
            )
        }
        multi method STORE(::?CLASS:D: Mu \item) {
            nqp::ifnull(
              nqp::atpos(nqp::getattr(self,List,'$!reified'),0),
              nqp::bindpos(nqp::getattr(self,List,'$!reified'),0,
                nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
            ) = item
        }

        multi method keys(::?CLASS:D:) {
            Seq.new(
              Rakudo::Internals.IntRangeIterator(0,self.shape.AT-POS(0) - 1))
        }
        method reverse(::?CLASS:D:) {
            Rakudo::Internals.ReverseListToList(
              self, self.new(:shape(self.shape)))
        }
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1) {
            Rakudo::Internals.RotateListToList(
              self, $rotate, self.new(:shape(self.shape)))
        }
    }

# vim: ft=perl6 expandtab sw=4
