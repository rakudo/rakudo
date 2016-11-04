# this is actually part of the Array class

    my role ShapedArray[::TValue] does Positional[TValue] does Rakudo::Internals::ShapedArrayCommon {
        has $.shape;

        proto method AT-POS(|) is raw {*}
        multi method AT-POS(Array:U: |c) is raw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(Array:D:) is raw {
            die "Must specify at least one index with AT-POS"
        }

        # Handle dimensions > 3 or more indices than dimensions.
        # If dimensions <= 3, then custom AT-POS should have caught
        # correct number of indices already.
        multi method AT-POS(Array:D: **@indices) is raw {
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

        proto method ASSIGN-POS(|) {*}
        multi method ASSIGN-POS(Array:U: |c) {
            self.Any::ASSIGN-POS(|c)
        }
        multi method ASSIGN-POS(Array:D:) {
            die "Must specify at least one index and a value with ASSIGN-POS"
        }
        multi method ASSIGN-POS(Array:D: $) {
            die "Must specify at least one index and a value with ASSIGN-POS"
        }

        multi method ASSIGN-POS(**@indices) {
            nqp::stmts(
              (my \value = @indices.pop),            # reifies
              (my $indices := nqp::getattr(@indices,List,'$!reified')),
              (my $reified := nqp::getattr(self,List,'$!reified')),
              nqp::if(
                nqp::islt_i(
                  (my int $numind  = nqp::elems($indices)),
                  (my int $numdims = nqp::numdimensions($reified))
                ),
                X::NotEnoughDimensions.new(          # too few indices
                  operation         => 'assign to',
                  got-dimensions    => $numind,
                  needed-dimensions => $numdims
                ).throw,
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
                  ) = value                          # and assign
                )
              )
            )
        }

        proto method EXISTS-POS(|) {*}
        multi method EXISTS-POS(Array:U: |c) {
            self.Any::EXISTS-POS(|c)
        }
        multi method EXISTS-POS(Array:D:) is raw {
            die "Must specify at least one index with EXISTS-POS"
        }

        multi method EXISTS-POS(**@indices) {
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
        multi method DELETE-POS(Array:U: |c) {
            self.Any::DELETE-POS(|c)
        }
        multi method DELETE-POS(**@indices) {
#say "dimmed EXISTS-POS";
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                nqp::push_i($idxs, @indices.shift)
                  while nqp::isge_i(--$numdims,0);

                my \value = nqp::ifnull(nqp::atposnd($storage, $idxs), Nil);
                if @indices {
                    value.DELETE-POS(|@indices)
                }
                else {
                    nqp::bindposnd($storage, $idxs, nqp::null());
                    value
                }
            }
            else {
                # Not enough dimensions, cannot delete
                X::NotEnoughDimensions.new(
                    operation => 'delete from',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
        }

        proto method BIND-POS(|) is raw {*}
        multi method BIND-POS(Array:U: |c) is raw {
            self.Any::BIND-POS(|c)
        }
        multi method BIND-POS(Array:D: **@indices is raw) is raw {
#say "dimmed BIND-POS";
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems - 1;
            my \value = @indices.AT-POS($numind);
            if $numind >= $numdims {
                # At least enough indices that binding will work out or we can
                # pass the bind target on down the chain.
                my $idxs := nqp::list_i();
                my int $i = -1;
                nqp::push_i($idxs, @indices.AT-POS($i))
                  while nqp::islt_i(++$i,$numdims);
                $numind == $numdims
                    ?? nqp::bindposnd($storage, $idxs, value)
                    !! nqp::atposnd($storage, $idxs).BIND-POS(|@indices[$numdims..*])
            }
            else {
                # Not enough dimensions, cannot possibly assign here
                X::NotEnoughDimensions.new(
                    operation => 'assign to',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
        }

        proto method STORE(|) { * }
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            my \in-shape = nqp::can(in, 'shape') ?? in.shape !! Nil;
            if in-shape && !nqp::istype(in-shape.AT-POS(0), Whatever) {
                if self.shape eqv in-shape {
                    # Can do a VM-supported memcpy-like thing in the future
                    self.ASSIGN-POS(|$_, in.AT-POS(|$_)) for self.keys;
                }
                else {
                    X::Assignment::ArrayShapeMismatch.new(
                        source-shape => in-shape,
                        target-shape => self.shape
                    ).throw
                }
            }
            else {
                self!STORE-PATH((), self.shape, in)
            }
        }
        multi method STORE(::?CLASS:D: Mu \item) {
            self.STORE((item,))
        }

        method reverse(::?CLASS:D:) {
            X::IllegalOnFixedDimensionArray.new(operation => 'reverse').throw
        }
        method rotate(::?CLASS:D: Cool \n) {
            X::IllegalOnFixedDimensionArray.new(operation => 'rotate').throw
        }

        # A shaped array isn't lazy, these methods don't need to go looking
        # into the "todo".
        method eager() { self }
        method is-lazy() { False }

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
