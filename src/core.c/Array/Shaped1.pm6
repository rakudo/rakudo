my role Array::Shaped1 does Array::Shaped {
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
        nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPos.new(
           nqp::getattr(array, Array, '$!descriptor'),
           nqp::getattr(array, List, '$!reified'),
           one))
    }

    multi method ASSIGN-POS(::?CLASS:D: int \one, \value) {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::ifnull(
          nqp::atpos(reified,one),
          nqp::bindpos(
            reified,
            one,
            nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
        ) = value
    }
    multi method ASSIGN-POS(::?CLASS:D: Int:D \one, \value) {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::ifnull(
          nqp::atpos(reified,one),
          nqp::bindpos(
            reified,
            one,
            nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor')))
        ) = value
    }

    multi method EXISTS-POS(::?CLASS:D: int \one --> Bool:D) {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::hllbool(
          nqp::islt_i(one,nqp::elems(reified))
            && nqp::not_i(nqp::isnull(nqp::atpos(reified,one)
          ))
        )
    }
    multi method EXISTS-POS(::?CLASS:D: Int:D \one --> Bool:D) {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::hllbool(
          nqp::islt_i(one,nqp::elems(reified))
            && nqp::not_i(nqp::isnull(nqp::atpos(reified,one)
          ))
        )
    }

    multi method DELETE-POS(::?CLASS:D: int \one) is raw {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::if(
          nqp::isnull(my \value := nqp::atpos(reified,one)),
          Nil,
          nqp::stmts(
            nqp::bindpos(reified,one,nqp::null),
            value
          )
        )
    }
    multi method DELETE-POS(::?CLASS:D: Int:D \one) is raw {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::if(
          nqp::isnull(my \value := nqp::atpos(reified,one)),
          Nil,
          nqp::stmts(
            nqp::bindpos(reified,one,nqp::null),
            value
          )
        )
    }

    multi method BIND-POS(::?CLASS:D: int \one, \value) {
        nqp::bindpos(nqp::getattr(self,List,'$!reified'),one,value)
    }
    multi method BIND-POS(::?CLASS:D: Int:D \one, \value) {
        nqp::bindpos(nqp::getattr(self,List,'$!reified'),one,value)
    }

    method !RE-INITIALIZE(::?CLASS:D:) {
        my \list := nqp::getattr(self,List,'$!reified');
        nqp::bind(   # rebind newly created list
          list,
          nqp::bindattr(
            self,List,'$!reified',
            nqp::setelems(nqp::create(list),nqp::elems(list))
          )
        )
    }

    proto method STORE(::?CLASS:D: |) {*}
    multi method STORE(::?CLASS:D: ::?CLASS:D \from-array) {
        my \to   := nqp::getattr(self,List,'$!reified');
        my \from := nqp::getattr(from-array,List,'$!reified');

        nqp::if(
          nqp::iseq_i(
            (my int $elems = nqp::elems(to)),nqp::elems(from)),
          nqp::stmts(
            (my \desc := nqp::getattr(self,Array,'$!descriptor')),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              # always create a new container in case the from list
              # contains containers already existing in the to list
              # e.g. after having done a .reverse or .rotate
              nqp::bindpos(to,$i,nqp::p6scalarfromdesc(desc)) =
                nqp::atpos(from,$i)
            ),
            self
          ),
          X::Assignment::ArrayShapeMismatch.new(
            source-shape => from-array.shape,
            target-shape => self.shape
          ).throw
        )
    }
    multi method STORE(::?CLASS:D: Iterable:D \in, :$INITIALIZE) {
        my \list := $INITIALIZE
          ?? nqp::getattr(self,List,'$!reified')
          !! self!RE-INITIALIZE;
        my \desc := nqp::getattr(self,Array,'$!descriptor');
        my \iter := in.iterator;
        my int $i = -1;
        my int $elems = nqp::elems(list);
        nqp::until(
          nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
            || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
          nqp::ifnull(
            nqp::atpos(list,$i),
            nqp::bindpos(list,$i,nqp::p6scalarfromdesc(desc))
          ) = pulled
        );
        nqp::atpos(list,$i) # too many values on non-lazy iter, error
          unless nqp::islt_i($i,$elems) || iter.is-lazy;
        self
    }
    multi method STORE(::?CLASS:D: Mu \item, :$INITIALIZE) {
        my \list := $INITIALIZE
          ?? nqp::getattr(self,List,'$!reified')
          !! self!RE-INITIALIZE;
        nqp::bindpos(list,0,
          nqp::p6scalarfromdesc(nqp::getattr(self,Array,'$!descriptor'))
        ) = item;
        self
    }

    multi method keys(::?CLASS:D:) {
        Seq.new(Rakudo::Iterator.IntRange(0,self.shape.AT-POS(0) - 1))
    }
    multi method kv(::?CLASS:D:) {
        Seq.new(Rakudo::Iterator.KeyValue(self.iterator))
    }
    multi method pairs(::?CLASS:D:) {
        Seq.new(Rakudo::Iterator.Pairs(self.iterator))
    }
    multi method antipairs(::?CLASS:D:) {
        Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
    }

    my class Iterate does PredictiveIterator {
        has Mu $!reified;
        has Mu $!desc;
        has int $!pos;
        method !SET-SELF(Mu \list) {
            $!reified := nqp::getattr(list,List,'$!reified');
            $!desc    := nqp::getattr(list,Array,'$!descriptor');
            $!pos = -1;
            self
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
                nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPos.new(
                  $!desc, $!reified, $!pos))
              ),
              IterationEnd
            )
        }
        method skip-one() {
            nqp::islt_i(($!pos = nqp::add_i($!pos,1)),nqp::elems($!reified))
        }
        method push-all(\target --> IterationEnd) {
            my int $elems = nqp::elems($!reified);
            my int $i = $!pos;

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              target.push(
                nqp::ifnull(
                  nqp::atpos($!reified,$i),
                  nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPos.new(
                    $!desc, $!reified, $i))
                )
              )
            );
            $!pos = $i;  # mark as done
        }
        method count-only(--> Int:D) {
            nqp::p6box_i(
              nqp::elems($!reified)
                - $!pos
                - nqp::islt_i($!pos,nqp::elems($!reified)
              )
            )
        }
        method sink-all(--> IterationEnd) {
            $!pos = nqp::elems($!reified)
        }
    }
    method iterator(::?CLASS:D: --> Iterator:D) { Iterate.new(self) }

    method reverse(::?CLASS:D: --> Seq:D) is nodal {
        Seq.new: nqp::elems(nqp::getattr(self,List,'$!reified'))
          ?? Rakudo::Iterator.ReifiedReverse(
               self, nqp::getattr(self,Array,'$!descriptor'))
          !! Rakudo::Iterator.Empty
    }

    method rotate(::?CLASS:D: Int(Cool) $rotate = 1 --> Seq:D) is nodal {
        Seq.new: Rakudo::Iterator.ReifiedRotate(
          $rotate, self, nqp::getattr(self,Array,'$!descriptor')
        )
    }
    multi method sum(::?CLASS:D:) { self.List::sum }
}

# vim: expandtab shiftwidth=4
