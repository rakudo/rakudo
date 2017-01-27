# this is actually part of the Array class

    my role Shaped1Array does ShapedArray {
        multi method AT-POS(::?CLASS:D: int \one) is raw {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              AT-POS-CONTAINER(self,one)
            )
        }
        multi method AT-POS(::?CLASS:D: Int:D \one) is raw {
            nqp::ifnull(
              nqp::atpos(
                nqp::getattr(self,List,'$!reified'),
                one),
              AT-POS-CONTAINER(self,one)
            )
        }
        sub AT-POS-CONTAINER(\array, int \one) is raw {
            nqp::p6bindattrinvres(
              (my $scalar := nqp::p6scalarfromdesc(
                nqp::getattr(array,Array,'$!descriptor'))),
              Scalar,
              '$!whence',
              -> { nqp::bindpos(
                     nqp::getattr(array,List,'$!reified'),
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

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::stmts(
              (my $to   := nqp::getattr(self,List,'$!reified')),
              (my $from := nqp::getattr(from,List,'$!reified')),
              nqp::if(
                nqp::iseq_i(
                  (my int $elems = nqp::elems($to)),nqp::elems($from)),
                nqp::stmts(
                  (my $desc := nqp::getattr(self,Array,'$!descriptor')),
                  (my int $i = -1),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                    # always create a new container in case the from list
                    # contains containers already existing in the to list
                    # e.g. after having done a .reverse or .rotate
                    nqp::bindpos($to,$i,nqp::p6scalarfromdesc($desc)) =
                      nqp::atpos($from,$i)
                  ),
                  self
                ),
                X::Assignment::ArrayShapeMismatch.new(
                  source-shape => from.shape,
                  target-shape => self.shape
                ).throw
              )
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
            nqp::stmts(
              (nqp::ifnull(
                nqp::atpos(nqp::getattr(self,List,'$!reified'),0),
                nqp::bindpos(nqp::getattr(self,List,'$!reified'),0,
                  nqp::p6scalarfromdesc(
                    nqp::getattr(self,Array,'$!descriptor')))
              ) = item),
              self
            )
        }

        multi method keys(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.IntRange(0,self.shape.AT-POS(0) - 1))
        }
        multi method kv(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.KeyValue(self.iterator))
        }
        multi method pairs(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.Pair(self.iterator))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }

        method iterator(::?CLASS:D:) {
            class :: does Iterator {
                has Mu $!reified;
                has Mu $!desc;
                has int $!pos;
                method !SET-SELF(Mu \list) {
                    nqp::stmts(
                      ($!reified := nqp::getattr(list,List,'$!reified')),
                      ($!desc    := nqp::getattr(list,Array,'$!descriptor')),
                      ($!pos = -1),
                      self
                    )
                }
                method new(Mu \list) { nqp::create(self)!SET-SELF(list) }
                method pull-one() is raw {
                    nqp::if(
                      nqp::islt_i(
                        ($!pos = nqp::add_i($!pos,1)),
                        nqp::elems($!reified)
                      ),
                      nqp::ifnull(
                        nqp::atpos($!reified,$!pos),
                        nqp::p6bindattrinvres(
                          (my $scalar := nqp::p6scalarfromdesc($!desc)),
                          Scalar,
                          '$!whence',
                          -> { nqp::bindpos($!reified,$!pos,$scalar) }
                        )
                      ),
                      IterationEnd
                    )
                }
                method push-all($target --> IterationEnd) {
                    nqp::stmts(
                      (my int $elems = nqp::elems($!reified)),
                      (my int $i = $!pos),
                      nqp::while(
                        nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                        $target.push(
                          nqp::ifnull(
                            nqp::atpos($!reified,$i),
                            nqp::p6bindattrinvres(
                              (my $scalar := nqp::p6scalarfromdesc($!desc)),
                              Scalar,
                              '$!whence',
                              -> { nqp::bindpos($!reified,$i,$scalar) }
                            )
                          )
                        )
                      ),
                      ($!pos = $i)  # mark as done
                    )
                }
                method count-only() { nqp::p6box_i(nqp::elems($!reified)) }
                method bool-only()  { nqp::p6bool(nqp::elems($!reified)) }
                method sink-all(--> IterationEnd) {
                    $!pos = nqp::elems($!reified)
                }
            }.new(self)
        }
        method reverse(::?CLASS:D:) is nodal {
            Rakudo::Internals.ReverseListToList(
              self, self.new(:shape(self.shape)))
        }
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1) is nodal {
            Rakudo::Internals.RotateListToList(
              self, $rotate, self.new(:shape(self.shape)))
        }
        method sum() is nodal { self.List::sum }
    }

# vim: ft=perl6 expandtab sw=4
