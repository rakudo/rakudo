# for our tantrums
my class X::Assignment::ArrayShapeMismatch { ... };
my class X::NotEnoughDimensions { ... };

# stub what we need now
my class array is repr('VMArray') { ... };

my role Array::Shaped does Rakudo::Internals::ShapedArrayCommon {
    has $.shape;

    multi method new(::?CLASS:D:) {
        nqp::istype(self,Array::Typed)
          ?? Array[self.of].new(:$!shape)
          !! Array.new(:$!shape)
    }

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
          X::NYI.new(
            feature => "Partially dimensioned views of shaped arrays").throw,
          nqp::stmts(
            (my \indices := nqp::getattr(@indices,List,'$!reified')),
            (my \idxs := nqp::list_i),
            nqp::while(                        # native index list
              nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
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
              nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
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
                  nqp::islt_i(                     # still indices left
                    ($i = nqp::add_i($i,1)),
                    $numind)
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
                  nqp::islt_i(($i = nqp::add_i($i,1)),$numind)
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
              nqp::islt_i(                     # still indices left
                ($i = nqp::add_i($i,1)),$numind),
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
              nqp::islt_i(                          # still indices left
                ($i = nqp::add_i($i,1)),$numind),
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

    my class MemCopy does Rakudo::Iterator::ShapeLeaf {
        has $!from;
        has $!desc;
        method !INIT(Mu \to, Mu \from) {
            $!from := nqp::getattr(from,List,'$!reified');
            $!desc := nqp::getattr(from,Array,'$!descriptor');
            self!SET-SELF(to)
        }
        method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
        method result(--> Nil) {
            nqp::ifnull(
              nqp::atposnd($!list,$!indices),
              nqp::bindposnd($!list,$!indices,
                nqp::p6scalarfromdesc($!desc))
            ) = nqp::ifnull(
                  nqp::atposnd($!from,$!indices),
                  nqp::p6scalarfromdesc($!desc)
                )
        }
    }
    sub MEMCPY(Mu \to, Mu \from) { MemCopy.new(to,from).sink-all }

    my class IntCopy does Rakudo::Iterator::ShapeLeaf {
        has $!from;
        method !INIT(Mu \to, Mu \from) {
            $!from := from;
            self!SET-SELF(to)
        }
        method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
        method result(--> Nil) {
            nqp::ifnull(
              nqp::atposnd($!list,$!indices),
              nqp::bindposnd($!list,$!indices,nqp::p6scalarfromdesc(Mu))
              ) = nqp::multidimref_i($!from,$!indices)
        }
    }
    sub INTCPY(Mu \to, Mu \from) { IntCopy.new(to,from).sink-all }

    my class NumCopy does Rakudo::Iterator::ShapeLeaf {
        has $!from;
        method !INIT(Mu \to, Mu \from) {
            $!from := from;
            self!SET-SELF(to)
        }
        method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
        method result(--> Nil) {
            nqp::ifnull(
              nqp::atposnd($!list,$!indices),
              nqp::bindposnd($!list,$!indices,nqp::p6scalarfromdesc(Mu))
              ) = nqp::multidimref_n($!from,$!indices)
        }
    }
    sub NUMCPY(Mu \to, Mu \from) { NumCopy.new(to,from).sink-all }

    method !RE-INITIALIZE(::?CLASS:D: --> Nil) {
        nqp::bindattr(  # this is a yucky way to re-init, but it works
          self,List,'$!reified',
          nqp::getattr(self.new(:shape(self.shape)),List,'$!reified')
        )
    }

    proto method STORE(::?CLASS:D: |) {*}
    multi method STORE(::?CLASS:D: ::?CLASS:D \in, :$INITIALIZE) {
        nqp::if(
          in.shape eqv self.shape,
          nqp::stmts(
            nqp::unless($INITIALIZE,self!RE-INITIALIZE),
            MEMCPY(self,in),     # VM-supported memcpy-like thing?
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
            nqp::if(
              nqp::istype(in.of,Int),
              INTCPY(self,in),     # copy from native int
              NUMCPY(self,in)      # copy from native num
            ),
            self
          ),
          X::Assignment::ArrayShapeMismatch.new(
            source-shape => in.shape,
            target-shape => self.shape
          ).throw
        )
    }

    my class StoreIterable does Rakudo::Iterator::ShapeBranch {
        has $!iterators;
        has $!desc;
        method !INIT(\to,\from) {
            self!SET-SELF(to);
            $!desc := nqp::getattr(to,Array,'$!descriptor');
            $!iterators := nqp::setelems(
                nqp::list(from.iterator),
                nqp::add_i($!maxdim,1)
            );
            self
        }
        method new(\to,\from) { nqp::create(self)!INIT(to,from) }
        method done(--> Nil) {
            nqp::unless(                        # verify lowest
              nqp::atpos($!iterators,0).is-lazy # finite iterator
                || nqp::eqaddr(                 # and something there
                     nqp::atpos($!iterators,0).pull-one,IterationEnd),
              nqp::atposnd($!list,$!indices)    # boom!
            )
        }
        method process(--> Nil) {
            my int $i = $!level;
            nqp::while(
              nqp::isle_i(($i = nqp::add_i($i,1)),$!maxdim),
              nqp::if(
                nqp::eqaddr((my $item :=      # exhausted ?
                  nqp::atpos($!iterators,nqp::sub_i($i,1)).pull-one),
                  IterationEnd
                ),
                nqp::bindpos($!iterators,$i,  # add an empty one
                  Rakudo::Iterator.Empty),
                nqp::if(                      # is it an iterator?
                  nqp::istype($item,Iterable) && nqp::isconcrete($item),
                  nqp::bindpos($!iterators,$i,$item.iterator),
                  X::Assignment::ToShaped.new(shape => self.dims).throw
                )
              )
            );

            my $iter := nqp::atpos($!iterators,$!maxdim);
            nqp::until(                       # loop over highest dim
              nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd)
                || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
              nqp::stmts(
                (nqp::ifnull(                 # containerize if needed
                  nqp::atposnd($!list,$!indices),
                  nqp::bindposnd($!list,$!indices,
                    nqp::p6scalarfromdesc($!desc))
                ) = $pulled),
                nqp::bindpos_i($!indices,$!maxdim,  # increment index
                  nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
              )
            );

            nqp::unless(
              nqp::eqaddr($pulled,IterationEnd) # if not exhausted
                || nqp::isle_i(                 # and index too high
                     nqp::atpos_i($!indices,$!maxdim),$!maxind)
                || $iter.is-lazy,               # and not lazy
              nqp::atposnd($!list,$!indices)    # error
            )
        }
    }
    multi method STORE(::?CLASS:D: Iterable:D \in, :$INITIALIZE) {
        self!RE-INITIALIZE unless $INITIALIZE;
        StoreIterable.new(self,in).sink-all;
        self
    }

    my class StoreIterator does Rakudo::Iterator::ShapeLeaf {
        has Mu $!iterator;
        has Mu $!desc;
        method !INIT(\list,\iterator) {
            $!iterator := iterator;
            $!desc := nqp::getattr(list,Array,'$!descriptor');
            self!SET-SELF(list)
        }
        method new(\list,\iter) { nqp::create(self)!INIT(list,iter) }
        method result(--> Nil) {
            nqp::unless(
              nqp::eqaddr(
                (my \pulled := $!iterator.pull-one),IterationEnd),
              nqp::ifnull(
                nqp::atposnd($!list,$!indices),
                nqp::bindposnd($!list,$!indices,
                  nqp::p6scalarfromdesc($!desc))
              ) = pulled
            )
        }
    }
    multi method STORE(::?CLASS:D: Iterator:D \iterator, :$INITIALIZE) {
        self!RE-INITIALIZE unless $INITIALIZE;
        StoreIterator.new(self,iterator).sink-all;
        self
    }

    multi method STORE(::?CLASS:D: Mu \item --> Nil) {
        X::Assignment::ToShaped.new(shape => self.shape).throw
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

    method clone(::?CLASS:D:) {
        my \obj := nqp::create(self);
        nqp::bindattr(obj,Array,'$!descriptor',
          nqp::getattr(self,Array,'$!descriptor'));
        nqp::bindattr(obj,::?CLASS,'$!shape',
          nqp::getattr(self,::?CLASS,'$!shape'));
        obj.STORE(self);
        obj
    }
}

# vim: expandtab shiftwidth=4
