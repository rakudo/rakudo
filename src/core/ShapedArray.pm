    my role ShapedArray[::TValue] does Positional[TValue] does Rakudo::Internals::ShapedArrayCommon {
        has $.shape;

        proto method AT-POS(|) is raw {*}
        multi method AT-POS(Array:U: |c) is raw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(Array:D: **@indices) is raw {
#say "dimmed AT-POS";
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                nqp::push_i($idxs, @indices.shift)
                  while nqp::isge_i(--$numdims,0);

                my \elem = nqp::ifnull(
                    nqp::atposnd($storage, $idxs),
                    nqp::p6bindattrinvres(
                        (my \v := nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor'))),
                        Scalar,
                        '$!whence',
                        -> { nqp::bindposnd($storage, $idxs, v) }));
                @indices ?? elem.AT-POS(|@indices) !! elem
            }
            else {
                X::NYI.new(feature => "Partially dimensioned views of arrays").throw
            }
        }

        proto method ASSIGN-POS(|) {*}
        multi method ASSIGN-POS(Array:U: |c) {
            self.Any::ASSIGN-POS(|c)
        }
        multi method ASSIGN-POS(**@indices) {
#say "dimmed ASSIGN-POS";
            my \value = @indices.pop;
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            if $numind == $numdims {
                # Dimension counts match, so fast-path it
                my $idxs := nqp::list_i();
                nqp::push_i($idxs, @indices.shift)
                  while nqp::isge_i(--$numdims,0);
                nqp::ifnull(
                    nqp::atposnd($storage, $idxs),
                    nqp::bindposnd($storage, $idxs,
                        nqp::p6scalarfromdesc(nqp::getattr(self, Array, '$!descriptor')))
                    ) = value
            }
            elsif $numind > $numdims {
                # More than enough dimensions; may work, fall to slow path
                self.AT-POS(@indices) = value
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

        proto method EXISTS-POS(|) {*}
        multi method EXISTS-POS(Array:U: |c) {
            self.Any::EXISTS-POS(|c)
        }
        multi method EXISTS-POS(**@indices) {
#say "dimmed EXISTS-POS";
            my Mu $storage := nqp::getattr(self, List, '$!reified');
            my $dims       := nqp::dimensions($storage);
            my int $numdims = nqp::numdimensions($storage);
            my int $numind  = @indices.elems;
            my int $i = -1;

            if $numind >= $numdims {
                my $idxs := nqp::list_i();
                my int $idx;

                ($idx = @indices.shift) >= nqp::atpos_i($dims,$i)
                  ?? return False
                  !! nqp::push_i($idxs, $idx)
                  while nqp::islt_i(++$i,$numind);

                nqp::isnull(nqp::atposnd($storage, $idxs))
                  ?? False
                  !! @indices
                     ?? nqp::atposnd($storage, $idxs).EXISTS-POS(|@indices)
                     !! True
            }
            else {
                return False
                  if @indices[$i] >= nqp::atpos_i($dims,$i)
                  while nqp::islt_i(++$i,$numind);

                True
            }
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

        # A shaped array isn't lazy, we these methods don't need to go looking
        # into the "todo".
        multi method elems(::?CLASS:D:) is nodal {
            nqp::elems(nqp::getattr(self, List, '$!reified'))
        }
        method eager() { self }
        method is-lazy() { False }

        multi method elems(::?CLASS:D:) {
            nqp::elems(nqp::getattr(self,List,'$!reified'))
        }
    }

    role Shaped1Array { ... }
    role Shaped2Array { ... }
    role Shaped3Array { ... }

    sub set-shape(\arr, \values, \shape) {
        my $shape := Metamodel::EnumHOW.ACCEPTS(shape.HOW)
          ?? shape.^elems
          !! shape;
        if $shape.DEFINITE {
            my \list-shape = nqp::istype($shape,List) ?? $shape !! $shape.list;
            nqp::bindattr(arr,List,'$!reified',
              Rakudo::Internals.SHAPED-ARRAY-STORAGE(
                list-shape,nqp::knowhow,Mu));
            my int $dimensions = list-shape.elems;
            arr does
              nqp::if(nqp::iseq_i($dimensions,1),
                Shaped1Array[Mu],
                nqp::if(nqp::iseq_i($dimensions,2),
                  Shaped2Array[Mu],
                  nqp::if(nqp::iseq_i($dimensions,3),
                    Shaped3Array[Mu],
                    ShapedArray[Mu]
                  )
                )
              );
            arr.^set_name('Array');
            nqp::bindattr(arr, arr.WHAT, '$!shape', list-shape);
            arr.STORE(values) if values;
        }
        else {
            arr.STORE(values);
        }
        arr
    }

# vim: ft=perl6 expandtab sw=4
