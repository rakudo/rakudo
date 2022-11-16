# for our tantrums
my class X::Assignment::ArrayShapeMismatch { ... };
my class X::NotEnoughDimensions { ... };

# stub what we need now
my class array is repr('VMArray') { ... };

my role Array::Shaped does Rakudo::Internals::ShapedArrayCommon {
    has $.shape;

    # Handle dimensions > 3 or more indices than dimensions.
    # If dimensions <= 3, then custom AT-POS should have caught
    # correct number of indices already.
    multi method AT-POS(::?CLASS:D: **@indices) is raw {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::if(
          nqp::islt_i(
            @indices.elems,                    # reifies
            (my int $numdims = nqp::numdimensions(reified))
          ),
          NYI("Partially dimensioned views of shaped arrays").throw,
          nqp::stmts(
            (my \indices := nqp::getattr(@indices,List,'$!reified')),
            (my \idxs := nqp::list_i),
            nqp::while(                        # native index list
              nqp::isge_i(--$numdims,0),
              nqp::push_i(idxs,nqp::shift(indices))
            ),
            (my \element := nqp::ifnull(
              nqp::atposnd(reified,idxs),      # found it
              nqp::p6scalarfromdesc(
                ContainerDescriptor::BindArrayPosND.new(
                  nqp::getattr(self, Array, '$!descriptor'),
                  reified, idxs
                )
              )
            )),
            nqp::if(
              nqp::elems(indices),
              element.AT-POS(|@indices),       # index further
              element                          # we're done!
            )
          )
        )
    }

    multi method ASSIGN-POS(::?CLASS:D: **@indices-value) {
        my \value   := @indices-value.pop;   # reifies
        my \indices := nqp::getattr(@indices-value,List,'$!reified');
        my \reified := nqp::getattr(self,List,'$!reified');

        nqp::if(
          nqp::isge_i(
            (my int $numind  = nqp::elems(indices)),
            (my int $numdims = nqp::numdimensions(reified))
          ),
          nqp::stmts(                          # more than enough indices
            (my \idxs := nqp::list_i),
            nqp::while(                        # native index list
              nqp::isge_i(--$numdims,0),
              nqp::push_i(idxs,nqp::shift(indices))
            ),
            (my \element := nqp::ifnull(
              nqp::atposnd(reified,idxs),      # found it!
              nqp::bindposnd(reified,idxs,     # create new scalar
                nqp::p6scalarfromdesc(
                  nqp::getattr(self,Array,'$!descriptor')))
            )),
            nqp::if(
              nqp::elems(indices),
              element.AT-POS(|@indices-value), # go deeper
              element                          # this is it
            ) = value                         # and assign
          ),
          X::NotEnoughDimensions.new(          # too few indices
            operation         => 'assign to',
            got-dimensions    => $numind,
            needed-dimensions => $numdims
          ).throw
        )
    }

    multi method EXISTS-POS(::?CLASS:D: **@indices --> Bool:D) {
        nqp::hllbool(
          nqp::stmts(
            (my int $numind = @indices.elems),     # reifies
            (my \indices := nqp::getattr(@indices,List,'$!reified')),
            (my \reified := nqp::getattr(self,List,'$!reified')),
            (my \dims    := nqp::dimensions(reified)),
            (my int $i = -1),
            nqp::if(
              nqp::isge_i(
                $numind,
                (my int $numdims = nqp::numdimensions(reified)),
              ),
              nqp::stmts(                          # same or more indices
                (my \idxs := nqp::list_i),
                nqp::while(
                  nqp::islt_i(++$i,$numind)        # still indices left
                    && nqp::islt_i(                # within range?
                         (my $idx = nqp::shift(indices)),
                         nqp::atpos_i(dims,$i)),
                  nqp::push_i(idxs,$idx)
                ),
                nqp::if(
                  nqp::iseq_i($i,$numind)
                    && nqp::not_i(
                         nqp::isnull(nqp::atposnd(reified,idxs))),
                  nqp::unless(                     # base pos exists
                    nqp::not_i(nqp::elems(indices)),
                    nqp::atposnd(reified,idxs).EXISTS-POS(|@indices)
                  )
                )
              ),
              nqp::stmts(                          # fewer inds than dims
                nqp::while(
                  nqp::islt_i(++$i,$numind)
                    && nqp::islt_i(
                         nqp::atpos(indices,$i),
                         nqp::atpos_i(dims,$i)),
                  nqp::null
                ),
                nqp::iseq_i($i,$numind)            # all clear or oor
              )
            )
          )
        )
    }

    proto method DELETE-POS(|) {*}
    multi method DELETE-POS(::?CLASS:U: |c) {
        self.Any::DELETE-POS(|c)
    }
    multi method DELETE-POS(::?CLASS:D:) is raw {
        die "Must specify at least one index with DELETE-POS"
    }

    multi method DELETE-POS(::?CLASS:D: **@indices) {
        my int $numind = @indices.elems;     # reifies
        my \indices := nqp::getattr(@indices,List,'$!reified');
        my \reified := nqp::getattr(self,List,'$!reified');
        my int $i = -1;

        nqp::if(
          nqp::isge_i(
            $numind,
            (my int $numdims = nqp::numdimensions(reified)),
          ),
          nqp::stmts(                          # same or more indices
            (my \idxs := nqp::list_i),
            nqp::while(
              nqp::islt_i(++$i,$numind),       # still indices left
              nqp::push_i(idxs,nqp::shift(indices)),
            ),
            nqp::if(
              nqp::isnull(my \value := nqp::atposnd(reified,idxs)),
              Nil,                             # nothing here
              nqp::if(
                nqp::elems(indices),
                value.DELETE-POS(|@indices),   # delete at deeper level
                nqp::stmts(                    # found it, nullify here
                  nqp::bindposnd(reified,idxs,nqp::null),
                  value
                )
              )
            )
          ),
          X::NotEnoughDimensions.new(          # fewer inds than dims
            operation         => 'delete from',
            got-dimensions    => $numind,
            needed-dimensions => $numdims
          ).throw
        )
    }

    proto method BIND-POS(|) is raw {*}
    multi method BIND-POS(::?CLASS:U: |c) is raw {
        self.Any::BIND-POS(|c)
    }
    multi method BIND-POS(::?CLASS:D:) {
        die "Must specify at least one index and a value with BIND-POS"
    }
    multi method BIND-POS(::?CLASS:D: $) {
        die "Must specify at least one index and a value with BIND-POS"
    }

    multi method BIND-POS(::?CLASS:D: **@indices) is raw {
        my \value   := nqp::decont(@indices.pop); # reifies
        my \indices := nqp::getattr(@indices,List,'$!reified');
        my \reified := nqp::getattr(self,List,'$!reified');
        my int $i = -1;

        nqp::if(
          nqp::isge_i(
            (my int $numind  = nqp::elems(indices)),
            (my int $numdims = nqp::numdimensions(reified)),
          ),
          nqp::stmts(                               # same or more indices
            (my \idxs := nqp::list_i),
            nqp::while(
              nqp::islt_i(++$i,$numind),            # still indices left
              nqp::push_i(idxs,nqp::shift(indices))
            ),
            nqp::if(
              nqp::elems(indices),
              nqp::atposnd(reified,idxs)            # bind at deeper level
                .BIND-POS(|@indices,value),
              nqp::bindposnd(reified,idxs,value)    # found it, bind here
            )
          ),
          X::NotEnoughDimensions.new(               # fewer inds than dims
            operation         => 'bind to',
            got-dimensions    => $numind,
            needed-dimensions => $numdims
          ).throw
        )
    }

    # NOTE: This wraps should an iterator produce more elements than the shape
    # can itself. Consider truncating input beforehand, e.g. via push-exactly.
    my class RingShapedReificationTarget {
        has $!framed;
        has $!window;
        has $!target;
        has $!descriptor;

        method new(::?CLASS: Mu $target is raw, Mu $descriptor is raw, List:D $shape --> ::?CLASS:D) {
            nqp::bindattr((my $self := nqp::create(self)),$?CLASS,'$!descriptor',$descriptor);
            nqp::bindattr($self,$?CLASS,'$!target',$target);
            my $buffer := nqp::getattr($shape.eager,List,'$!reified');
            my $framed := nqp::list_i();
            my int $dim = nqp::elems($buffer);
            nqp::while(($dim--),nqp::bindpos_i($framed,$dim,nqp::atpos($buffer,$dim)));
            nqp::bindattr($self,$?CLASS,'$!window',nqp::setelems(nqp::list_i(),nqp::elems($framed)));
            nqp::p6bindattrinvres($self,$?CLASS,'$!framed',$framed)
        }

        # Sanitizes the window, making it the target's next pushable offset.
        # Expects an increment at the tail of the window to bump its offset.
        method !unveil(::?CLASS:D: --> Nil) {
            nqp::stmts(
              (my int $marked = nqp::sub_i(nqp::elems($!window),1)),
              nqp::while(
                nqp::if(
                  nqp::isge_i($marked,0),
                  nqp::isge_i(
                    nqp::atpos_i($!window,$marked),
                    nqp::atpos_i($!framed,$marked))),
                nqp::stmts(
                  nqp::bindpos_i($!window,($marked--),0),
                  nqp::if(
                    nqp::isge_i($marked,0),
                    nqp::bindpos_i($!window,$marked,
                      nqp::add_i(nqp::atpos_i($!window,$marked),1))))));
        }

        method push(::?CLASS:D: Mu $value is raw --> Nil) {
            nqp::stmts(
              (self!unveil),
              nqp::bindposnd($!target,$!window,
                nqp::p6scalarwithvalue($!descriptor,$value)),
              nqp::bindpos_i($!window,(my int $marked = nqp::sub_i(nqp::elems($!window),1)),
                nqp::add_i(nqp::atpos_i($!window,$marked),1)))
        }

        method append(::?CLASS:D: Mu $buffer is raw --> Nil) {
            nqp::while( # Lift the unshaped.
              nqp::islt_i((my int $cursor),nqp::elems($buffer)),
              nqp::stmts(
                (self!unveil),
                nqp::bindposnd($!target,$!window,
                  nqp::p6scalarwithvalue($!descriptor,
                    nqp::atpos($buffer,$cursor++))),
                nqp::bindpos_i($!window,(my int $marked = nqp::sub_i(nqp::elems($!window),1)),
                  nqp::add_i(nqp::atpos_i($!window,$marked),1))))
        }
    }

    method reification-target(::?CLASS:D: --> RingShapedReificationTarget:D) {
        RingShapedReificationTarget.new:
            nqp::ifnull(nqp::getattr(self,List,'$!reified'),(self!RE-INITIALIZE)),
            nqp::getattr(self,Array,'$!descriptor'),
            $!shape
    }

    method make-iterator(::?CLASS:D: Iterator:D $iterator --> ::?CLASS:D) {
        nqp::unless(
          (nqp::eqaddr(
            ($iterator.push-exactly: self.reification-target, $!shape.reduce: * * *),
            IterationEnd) || nqp::eqaddr(($iterator.pull-one),IterationEnd)),
          (X::Assignment::ToShaped.new(:$!shape).throw));
        self
    }

    method make-itemized(::?CLASS:D: Mu $item is raw --> ::?CLASS:D) {
        X::Assignment::ToShaped.new(:$!shape).throw;
        self
    }

    method make-iterable(::?CLASS:D: Mu $iterable --> ::?CLASS:D) {
        Rakudo::Iterator::FrameIterable.new(self, $iterable).sink-all;
        self
    }

    my package Copy { # is a class stub with a stash
        role Object does Rakudo::Iterator::ShapeLeaf {
            has $!desc;
            has $!from;
            method !INIT(Mu \to, List:D \from) {
                $!desc := nqp::getattr(to,Array,'$!descriptor');
                $!from := nqp::getattr(from,List,'$!reified');
                self!SET-SELF(to)
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
        }

        role Native does Rakudo::Iterator::ShapeLeaf {
            has $!desc;
            has $!from;
            method !INIT(Mu \to, array:D \from) {
                $!desc := nqp::getattr(to,Array,'$!descriptor');
                $!from := from;
                self!SET-SELF(to)
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
        }
    }

    my class Copy:<obj> does Copy::Object {
        method result(--> Nil) {
            nqp::bindposnd($!list,$!indices,
              nqp::p6scalarwithvalue($!desc,
                nqp::ifnull(nqp::atposnd($!from,$!indices),Nil)))
        }
    }

    my class Copy:<int> does Copy::Native {
        method result(--> Nil) {
            nqp::bindposnd($!list,$!indices,
              nqp::p6scalarwithvalue($!desc,
                nqp::atposnd_i($!from,$!indices)))
        }
    }

    my class Copy:<uint> does Copy::Native {
        method result(--> Nil) {
            nqp::bindposnd($!list,$!indices,
              nqp::p6scalarwithvalue($!desc,
                nqp::atposnd_u($!from,$!indices)))
        }
    }

    my class Copy:<num> does Copy::Native {
        method result(--> Nil) {
            nqp::bindposnd($!list,$!indices,
              nqp::p6scalarwithvalue($!desc,
                nqp::atposnd_n($!from,$!indices)))
        }
    }

    my class Copy:<str> does Copy::Native {
        method result(--> Nil) {
            nqp::bindposnd($!list,$!indices,
              nqp::p6scalarwithvalue($!desc,
                nqp::atposnd_s($!from,$!indices)))
        }
    }

    my class Copy {
        method ^parameterize(Mu, Mu:U \T --> Iterator:U) is raw {
            nqp::iseq_i((my int $spec = nqp::objprimspec(T)),0)
              ?? Copy:<obj>
              !! nqp::iseq_i($spec,2)
                ?? Copy:<num>
                !! nqp::iseq_i($spec,3)
                  ?? Copy:<str>
                  !! nqp::isge_i($spec,7)
                    ?? Copy:<uint>
                    !! Copy:<int>
        }
    }

    method !RE-INITIALIZE(::?CLASS:D:) is raw {
        nqp::bindattr(self,List,'$!reified',
          (Rakudo::Internals.SHAPED-ARRAY-STORAGE: $!shape, nqp::knowhow, Mu))
    }

    proto method STORE(::?CLASS:D: |) {*}
    multi method STORE(::?CLASS:D: ::?CLASS:D \in, :$INITIALIZE) {
        nqp::if(
          in.shape eqv self.shape,
          nqp::stmts(
            nqp::unless($INITIALIZE,self!RE-INITIALIZE),
            Copy:<obj>.new(self,in).sink-all,  # VM-supported memcpy-like thing?
            self
          ),
          X::Assignment::ArrayShapeMismatch.new(
            source-shape => in.shape,
            target-shape => self.shape
          ).throw
        )
    }
    multi method STORE(::?CLASS:D: array:D \in, :$INITIALIZE) {
        nqp::if(
          in.shape eqv self.shape,
          nqp::stmts(
            nqp::unless($INITIALIZE,self!RE-INITIALIZE),
            Copy[in.of].new(self,in).sink-all,
            self
          ),
          X::Assignment::ArrayShapeMismatch.new(
            source-shape => in.shape,
            target-shape => self.shape
          ).throw
        )
    }
    multi method STORE(::?CLASS:D: Iterable:D $iterable, :$INITIALIZE) {
        self!RE-INITIALIZE unless $INITIALIZE;
        self.make-iterable: $iterable
    }
    multi method STORE(::?CLASS:D: Iterator:D $iterator, :$INITIALIZE) {
        self!RE-INITIALIZE unless $INITIALIZE;
        self.make-iterator: $iterator
    }
    multi method STORE(::?CLASS:D: Mu \item --> Nil) {
        self.make-itemized: item
    }

    my class KV does Rakudo::Iterator::ShapeLeaf {
        has int $!on-key;
        method result() is raw {
            nqp::if(
              ($!on-key = nqp::not_i($!on-key)),
              nqp::stmts(
                (my \result := self.indices),
                (nqp::bindpos_i($!indices,$!maxdim,  # back 1 for next
                  nqp::sub_i(nqp::atpos_i($!indices,$!maxdim),1))),
                result
              ),
              nqp::atposnd($!list,$!indices)
            )
        }
        # needs its own push-all since it fiddles with $!indices
        method push-all(\target --> IterationEnd) {
            nqp::until(
              nqp::eqaddr((my \pulled := self.pull-one),IterationEnd),
              target.push(pulled)
            )
        }
    }
    multi method kv(::?CLASS:D:) { Seq.new(KV.new(self)) }

    my class Pairs does Rakudo::Iterator::ShapeLeaf {
        has Mu $!desc;
        method !INIT(\list) {
            $!desc := nqp::getattr(list,Array,'$!descriptor');
            self!SET-SELF(list)
        }
        method new(Mu \list) { nqp::create(self)!INIT(list) }
        method result() {
            Pair.new(
              self.indices,
              nqp::ifnull(
                nqp::atposnd($!list,$!indices),
                # By the time the block gets executed, the $!indices
                # may be at the next iteration already or even reset
                # because we reached the end.  So we need to make
                # a copy of the indices now.
                nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPosND.new(
                  $!desc, $!list, nqp::clone($!indices)))
              )
            )
        }
    }
    multi method pairs(::?CLASS:D:) { Seq.new(Pairs.new(self)) }

    my class AntiPairs does Rakudo::Iterator::ShapeLeaf {
        method result() {
            Pair.new(nqp::atposnd($!list,$!indices),self.indices)
        }
    }
    multi method antipairs(::?CLASS:D:) {
        Seq.new(AntiPairs.new(self))
    }

    multi method List(::?CLASS:D: --> List:D) {
        my \buf := nqp::create(IterationBuffer);
        self.iterator.push-all(buf);
        buf.List
    }

    multi method Array(::?CLASS:D: --> Array:D) {
        my @Array := nqp::eqaddr(self.of,Mu)
          ?? Array.new
          !! Array[self.of].new;
        self.iterator.push-all(@Array);
        @Array
    }

    my class Iterate does Rakudo::Iterator::ShapeLeaf {
        has Mu $!desc;
        method !INIT(\list) {
            $!desc := nqp::getattr(list,Array,'$!descriptor');
            self!SET-SELF(list)
        }
        method new(Mu \list) { nqp::create(self)!INIT(list) }
        method result() is raw {
            nqp::ifnull(
              nqp::atposnd($!list,$!indices),
              # By the time the block gets executed, the $!indices
              # may be at the next iteration already or even reset
              # because we reached the end.  So we need to make
              # a copy of the indices now.
              nqp::p6scalarfromdesc(ContainerDescriptor::BindArrayPosND.new(
                $!desc, $!list, nqp::clone($!indices)))
           )
        }
    }
    method iterator(::?CLASS:D: --> Iterator:D) { Iterate.new(self) }

    # A shaped array isn't lazy, these methods don't need to go looking
    # into the "todo".
    method eager() { self }

    multi method sum(::?CLASS:D:) { self.Any::sum }
    multi method elems(::?CLASS:D:) {
        nqp::elems(nqp::getattr(self,List,'$!reified'))
    }

    method rub(::?CLASS: --> ::?CLASS:D) {
        nqp::if(
          nqp::isconcrete(self),
          nqp::stmts(
            nqp::bindattr((my $shaped := callsame),List,'$!reified',
              (Rakudo::Internals.SHAPED-ARRAY-STORAGE: $!shape, nqp::knowhow, Mu)),
            nqp::p6bindattrinvres($shaped,$?CLASS,'$!shape',$!shape)),
          (callsame))
    }
}

# vim: expandtab shiftwidth=4
