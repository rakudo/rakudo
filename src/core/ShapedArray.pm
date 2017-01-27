# this is actually part of the Array class

    my role ShapedArray does Rakudo::Internals::ShapedArrayCommon {
        has $.shape;

        # Handle dimensions > 3 or more indices than dimensions.
        # If dimensions <= 3, then custom AT-POS should have caught
        # correct number of indices already.
        multi method AT-POS(::?CLASS:D: **@indices) is raw {
            nqp::stmts(
              (my $reified := nqp::getattr(self,List,'$!reified')),
              nqp::if(
                nqp::islt_i(
                  @indices.elems,                    # reifies
                  (my int $numdims = nqp::numdimensions($reified))
                ),
                X::NYI.new(
                  feature => "Partially dimensioned views of arrays").throw,
                nqp::stmts(
                  (my $indices := nqp::getattr(@indices,List,'$!reified')),
                  (my $idxs := nqp::list_i),
                  nqp::while(                        # native index list
                    nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                    nqp::push_i($idxs,nqp::shift($indices))
                  ),
                  (my $element := nqp::ifnull(
                    nqp::atposnd($reified,$idxs),    # found it
                    nqp::p6bindattrinvres(           # create container
                      (my $scalar := nqp::p6scalarfromdesc(
                        nqp::getattr(self,Array,'$!descriptor'))),
                      Scalar,
                      '$!whence',
                      -> { nqp::bindposnd($reified,$idxs,$scalar) }
                    )
                  )),
                  nqp::if(
                    nqp::elems($indices),
                    $element.AT-POS(|@indices),      # index further
                    $element                         # we're done!
                  )
                )
              )
            )
        }

        multi method ASSIGN-POS(::?CLASS:D: **@indices) {
            nqp::stmts(
              (my $value   := @indices.pop),         # reifies
              (my $indices := nqp::getattr(@indices,List,'$!reified')),
              (my $reified := nqp::getattr(self,List,'$!reified')),
              nqp::if(
                nqp::isge_i(
                  (my int $numind  = nqp::elems($indices)),
                  (my int $numdims = nqp::numdimensions($reified))
                ),
                nqp::stmts(                          # more than enough indices
                  (my $idxs := nqp::list_i),
                  nqp::while(                        # native index list
                    nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                    nqp::push_i($idxs,nqp::shift($indices))
                  ),
                  (my $element := nqp::ifnull(
                    nqp::atposnd($reified,$idxs),    # found it!
                    nqp::bindposnd($reified,$idxs,   # create new scalar
                      nqp::p6scalarfromdesc(
                        nqp::getattr(self,Array,'$!descriptor')))
                  )),
                  nqp::if(
                    nqp::elems($indices),
                    $element.AT-POS(|@indices),      # go deeper
                    $element                         # this is it
                  ) = $value                         # and assign
                ),
                X::NotEnoughDimensions.new(          # too few indices
                  operation         => 'assign to',
                  got-dimensions    => $numind,
                  needed-dimensions => $numdims
                ).throw
              )
            )
        }

        multi method EXISTS-POS(::?CLASS:D: **@indices) {
            nqp::p6bool(
              nqp::stmts(
                (my int $numind = @indices.elems),     # reifies
                (my $indices := nqp::getattr(@indices,List,'$!reified')),
                (my $reified := nqp::getattr(self,List,'$!reified')),
                (my $dims    := nqp::dimensions($reified)),
                (my int $i = -1),
                nqp::if(
                  nqp::isge_i(
                    $numind,
                    (my int $numdims = nqp::numdimensions($reified)),
                  ),
                  nqp::stmts(                          # same or more indices
                    (my $idxs := nqp::list_i),
                    nqp::while(
                      nqp::islt_i(                     # still indices left
                        ($i = nqp::add_i($i,1)),
                        $numind)
                        && nqp::islt_i(                # within range?
                             (my $idx = nqp::shift($indices)),
                             nqp::atpos_i($dims,$i)),
                      nqp::push_i($idxs,$idx)
                    ),
                    nqp::if(
                      nqp::iseq_i($i,$numind)
                        && nqp::not_i(
                             nqp::isnull(nqp::atposnd($reified,$idxs))),
                      nqp::unless(                     # base pos exists
                        nqp::not_i(nqp::elems($indices)),
                        nqp::atposnd($reified,$idxs).EXISTS-POS(|@indices)
                      )
                    )
                  ),
                  nqp::stmts(                          # fewer inds than dims
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$numind)
                        && nqp::islt_i(
                             nqp::atpos($indices,$i),
                             nqp::atpos_i($dims,$i)),
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
            nqp::stmts(
              (my int $numind = @indices.elems),     # reifies
              (my $indices := nqp::getattr(@indices,List,'$!reified')),
              (my $reified := nqp::getattr(self,List,'$!reified')),
              (my int $i = -1),
              nqp::if(
                nqp::isge_i(
                  $numind,
                  (my int $numdims = nqp::numdimensions($reified)),
                ),
                nqp::stmts(                          # same or more indices
                  (my $idxs := nqp::list_i),
                  nqp::while(
                    nqp::islt_i(                     # still indices left
                      ($i = nqp::add_i($i,1)),$numind),
                    nqp::push_i($idxs,nqp::shift($indices)),
                  ),
                  nqp::if(
                    nqp::isnull(my $value := nqp::atposnd($reified,$idxs)),
                    Nil,                             # nothing here
                    nqp::if(
                      nqp::elems($indices),
                      $value.DELETE-POS(|@indices),  # delete at deeper level
                      nqp::stmts(                    # found it, nullify here
                        nqp::bindposnd($reified,$idxs,nqp::null),
                        $value
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
            nqp::stmts(
              (my $value   := nqp::decont(@indices.pop)), # reifies
              (my $indices := nqp::getattr(@indices,List,'$!reified')),
              (my $reified := nqp::getattr(self,List,'$!reified')),
              (my int $i = -1),
              nqp::if(
                nqp::isge_i(
                  (my int $numind  = nqp::elems($indices)),
                  (my int $numdims = nqp::numdimensions($reified)),
                ),
                nqp::stmts(                               # same or more indices
                  (my $idxs := nqp::list_i),
                  nqp::while(
                    nqp::islt_i(                          # still indices left
                      ($i = nqp::add_i($i,1)),$numind),
                    nqp::push_i($idxs,nqp::shift($indices))
                  ),
                  nqp::if(
                    nqp::elems($indices),
                    nqp::atposnd($reified,$idxs)          # bind at deeper level
                      .BIND-POS(|@indices,$value),
                    nqp::bindposnd($reified,$idxs,        # found it, bind here
                      $value)
                  )
                ),
                X::NotEnoughDimensions.new(               # fewer inds than dims
                  operation         => 'bind to',
                  got-dimensions    => $numind,
                  needed-dimensions => $numdims
                ).throw
              )
            )
        }

        sub MEMCPY(Mu \to, Mu \from) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has $!from;
                has $!desc;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := nqp::getattr(from,List,'$!reified')),
                      ($!desc := nqp::getattr(from,Array,'$!descriptor')),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) { nqp::create(self).INIT(to,from) }
                method result(--> Nil) {
                    nqp::ifnull(
                      nqp::atposnd($!list,$!indices),
                      nqp::bindposnd($!list,$!indices,
                        nqp::p6scalarfromdesc($!desc))
                    ) = nqp::atposnd($!from,$!indices)
                }
            }.new(to,from).sink-all
        }
        sub INTCPY(Mu \to, Mu \from) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := from),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) { nqp::create(self).INIT(to,from) }
                method result(--> Nil) {
                    nqp::ifnull(
                      nqp::atposnd($!list,$!indices),
                      nqp::bindposnd($!list,$!indices,nqp::p6scalarfromdesc(Mu))
#?if moar
                      ) = nqp::multidimref_i($!from,$!indices)
#?endif
#?if !moar
                      ) = nqp::atposnd_i($!from,$!indices)
#?endif
                }
            }.new(to,from).sink-all
        }
        sub NUMCPY(Mu \to, Mu \from) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := from),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) { nqp::create(self).INIT(to,from) }
                method result(--> Nil) {
                    nqp::ifnull(
                      nqp::atposnd($!list,$!indices),
                      nqp::bindposnd($!list,$!indices,nqp::p6scalarfromdesc(Mu))
#?if moar
                      ) = nqp::multidimref_n($!from,$!indices)
#?endif
#?if !moar
                      ) = nqp::atposnd_n($!from,$!indices)
#?endif
                }
            }.new(to,from).sink-all
        }

        proto method STORE(|) { * }
        multi method STORE(::?CLASS:D: ::?CLASS:D \in) {
            nqp::if(
              in.shape eqv self.shape,
              nqp::stmts(
                MEMCPY(self,in),     # VM-supported memcpy-like thing?
                self
              ),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => in.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: array:D \in) {
            nqp::if(
              in.shape eqv self.shape,
              nqp::stmts(
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
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            class :: does Rakudo::Iterator::ShapeBranch {
                has $!iterators;
                has $!desc;
                method INIT(\to,\from) {
                    nqp::stmts(
                      self.SET-SELF(to),
                      ($!desc := nqp::getattr(to,Array,'$!descriptor')),
                      ($!iterators := nqp::setelems(
                        nqp::list(from.iterator),
                        nqp::add_i($!maxdim,1)
                      )),
                      self
                    )
                }
                method new(\to,\from) { nqp::create(self).INIT(to,from) }
                method done(--> Nil) {
                    nqp::unless(                        # verify lowest
                      nqp::atpos($!iterators,0).is-lazy # finite iterator
                        || nqp::eqaddr(                 # and something there
                             nqp::atpos($!iterators,0).pull-one,IterationEnd),
                      nqp::atposnd($!list,$!indices)    # boom!
                    )
                }
                method process(--> Nil) {
                    nqp::stmts(
                      (my int $i = $!level),
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
                      ),
                      (my $iter := nqp::atpos($!iterators,$!maxdim)),
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
                      ),
                      nqp::unless(
                        nqp::eqaddr($pulled,IterationEnd) # if not exhausted
                          || nqp::isle_i(                 # and index too high
                               nqp::atpos_i($!indices,$!maxdim),$!maxind)
                          || $iter.is-lazy,               # and not lazy
                        nqp::atposnd($!list,$!indices)    # error
                      )
                    )
                }
            }.new(self,in).sink-all;
            self
        }
        multi method STORE(::?CLASS:D: Iterator:D \iterator) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!iterator;
                has Mu $!desc;
                method INIT(\list,\iterator) {
                    nqp::stmts(
                      ($!iterator := iterator),
                      ($!desc := nqp::getattr(list,Array,'$!descriptor')),
                      self.SET-SELF(list)
                    )
                }
                method new(\list,\iter) { nqp::create(self).INIT(list,iter) }
                method result(--> Nil) {
                    nqp::unless(
                      nqp::eqaddr(
                        (my $pulled := $!iterator.pull-one),IterationEnd),
                      nqp::ifnull(
                        nqp::atposnd($!list,$!indices),
                        nqp::bindposnd($!list,$!indices, 
                          nqp::p6scalarfromdesc($!desc))
                      ) = $pulled
                    )
                }
            }.new(self,iterator).sink-all;
            self
        }
        multi method STORE(::?CLASS:D: Mu \item) {
            X::Assignment::ToShaped.new(shape => self.shape).throw
        }

        multi method kv(::?CLASS:D:) {
            Seq.new(class :: does Rakudo::Iterator::ShapeLeaf {
                has int $!on-key;
                method result() is raw {
                    nqp::if(
                      ($!on-key = nqp::not_i($!on-key)),
                      nqp::stmts(
                        (my $result := self.indices),
                        (nqp::bindpos_i($!indices,$!maxdim,  # back 1 for next
                          nqp::sub_i(nqp::atpos_i($!indices,$!maxdim),1))),
                        $result  
                      ),
                      nqp::atposnd($!list,$!indices)
                    )
                }
                # needs its own push-all since it fiddles with $!indices
                method push-all($target --> IterationEnd) {
                    nqp::until(
                      nqp::eqaddr((my $pulled := self.pull-one),IterationEnd),
                      $target.push($pulled)
                    )
                }
            }.new(self))
        }
        multi method pairs(::?CLASS:D:) {
            Seq.new(class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!desc;
                method !INIT(\list) {
                    nqp::stmts(
                      ($!desc := nqp::getattr(list,Array,'$!descriptor')),
                      self.SET-SELF(list)
                    )
                }
                method new(Mu \list) { nqp::create(self)!INIT(list) }
                method result() {
                    Pair.new(
                      self.indices,
                      nqp::ifnull(
                        nqp::atposnd($!list,$!indices),
                        nqp::stmts(
                          # By the time the block gets executed, the $!indices
                          # may be at the next iteration already or even reset
                          # because we reached the end.  So we need to make
                          # a copy of the indices now.
                          (my $indices := nqp::clone($!indices)),
                          nqp::p6bindattrinvres(
                            (my $scalar := nqp::p6scalarfromdesc($!desc)),
                            Scalar,
                           '$!whence',
                            -> { nqp::bindposnd($!list,$indices,$scalar) }
                          )
                        )
                      )
                    )
                }
            }.new(self))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(class :: does Rakudo::Iterator::ShapeLeaf {
                method result() {
                    Pair.new(nqp::atposnd($!list,$!indices),self.indices)
                }
            }.new(self))
        }

        multi method List(::?CLASS:D:) {
            nqp::stmts(
              self.iterator.push-all(
                (my $list := nqp::create(IterationBuffer))),
              nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$list)
            )
        }

        method iterator(::?CLASS:D:) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!desc;
                method !INIT(\list) {
                    nqp::stmts(
                      ($!desc := nqp::getattr(list,Array,'$!descriptor')),
                      self.SET-SELF(list)
                    )
                }
                method new(Mu \list) { nqp::create(self)!INIT(list) }
                method result() is raw {
                    nqp::ifnull(
                      nqp::atposnd($!list,$!indices),
                      nqp::stmts(
                        # By the time the block gets executed, the $!indices
                        # may be at the next iteration already or even reset
                        # because we reached the end.  So we need to make
                        # a copy of the indices now.
                        (my $indices := nqp::clone($!indices)),
                        nqp::p6bindattrinvres(
                          (my $scalar := nqp::p6scalarfromdesc($!desc)),
                          Scalar,
                         '$!whence',
                          -> { nqp::bindposnd($!list,$indices,$scalar) }
                       )
                     )
                   )
                }
            }.new(self)
        }

        # A shaped array isn't lazy, these methods don't need to go looking
        # into the "todo".
        method eager() { self }

        method sum() is nodal { self.Any::sum }
        multi method elems(::?CLASS:D:) {
            nqp::elems(nqp::getattr(self,List,'$!reified'))
        }

        method clone() {
            my \obj := nqp::create(self);
            nqp::bindattr(obj,Array,'$!descriptor',
              nqp::getattr(self,Array,'$!descriptor'));
            nqp::bindattr(obj,::?CLASS,'$!shape',
              nqp::getattr(self,::?CLASS,'$!shape'));
            nqp::p6bindattrinvres(obj,List,'$!reified',
              nqp::clone(nqp::getattr(self,List,'$!reified')))
        }
    }

# vim: ft=perl6 expandtab sw=4
