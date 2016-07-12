my class X::TypeCheck { ... };
my class X::Subscript::Negative { ... };
my class X::NotEnoughDimensions { ... };
my class X::Assignment::ArrayShapeMismatch { ... };

# An Array is a List that ensures every item added to it is in a Scalar
# container. It also supports push, pop, shift, unshift, splice, BIND-POS,
# and so forth.
my class Array { # declared in BOOTSTRAP
    # class Array is List {
    #     has Mu $!descriptor;

    my class ArrayReificationTarget {
        has $!target;
        has $!descriptor;

        method new(\target, Mu \descriptor) {
            my \rt = nqp::create(self);
            nqp::bindattr(rt, self, '$!target', target);
            nqp::bindattr(rt, self, '$!descriptor', descriptor);
            rt
        }

        method push(Mu \value) {
            nqp::push($!target,
                nqp::assign(nqp::p6scalarfromdesc($!descriptor), value));
        }
    }

    my class ListReificationTarget {
        has $!target;

        method new(\target) {
            nqp::p6bindattrinvres(nqp::create(self), self, '$!target', target);
        }

        method push(Mu \value) {
            nqp::push($!target,
                nqp::decont(value));
        }
    }

    method cow() {
        my int $elems = self.elems; # ensure reification
        my $src  := nqp::getattr(self,List,'$!reified');
        my $cow  := nqp::setelems(nqp::list,$elems);
        my int $i = -1;

        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos($cow,$i,nqp::p6scalarfromdesc($!descriptor) =
            nqp::decont(nqp::atpos($src,$i)))
        );

        nqp::p6bindattrinvres(nqp::create(Array),List,'$!reified',$cow)
    }

    method iterator(Array:D:) {

        # something to iterate over in the future
        if nqp::getattr(self,List,'$!todo').DEFINITE {
            self!ensure-allocated;
            class :: does Iterator {
                has int $!i;
                has $!array;
                has $!reified;
                has $!todo;
                has $!descriptor;

                method !SET-SELF(\array) {
                    $!i           = -1;
                    $!array      := array;
                    $!reified    := nqp::getattr(array, List,  '$!reified');
                    $!todo       := nqp::getattr(array, List,  '$!todo');
                    $!descriptor := nqp::getattr(array, Array, '$!descriptor');
                    self
                }
                method new(\array) { nqp::create(self)!SET-SELF(array) }

                method pull-one() is raw {
                    nqp::ifnull(
                      nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
                      nqp::islt_i($!i,nqp::elems($!reified))
                        ?? self!found-hole
                        !! $!todo.DEFINITE
                          ?? nqp::islt_i($!i,$!todo.reify-at-least(nqp::add_i($!i,1)))
                            ?? nqp::atpos($!reified,$!i) # cannot be nqp::null
                            !! self!done
                          !! IterationEnd
                    )
                }
                method !found-hole() {
                   nqp::p6bindattrinvres(
                     (my \v := nqp::p6scalarfromdesc($!descriptor)),
                     Scalar,
                     '$!whence',
                     -> { nqp::bindpos($!reified,$!i,v) }
                   )
                }
                method !done() is raw {
                    $!todo := nqp::bindattr($!array,List,'$!todo',Mu);
                    IterationEnd
                }

                method push-until-lazy($target) {
                    if $!todo.DEFINITE {
                        my int $elems = $!todo.reify-until-lazy;
                        nqp::while(   # doesn't sink
                          nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                          $target.push(nqp::atpos($!reified,$!i))
                        );
                        nqp::if(
                          $!todo.fully-reified,
                          self!done,
                          nqp::stmts(
                            ($!i = nqp::sub_i($elems,1)),
                            Mu
                          )
                        )
                    }
                    else {
                        my int $elems = nqp::elems($!reified);
                        nqp::while(   # doesn't sink
                          nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                          $target.push(
                            nqp::ifnull(
                              nqp::atpos($!reified,$!i),
                              nqp::p6bindattrinvres(
                                (my \v := nqp::p6scalarfromdesc($!descriptor)),
                                Scalar,
                                '$!whence',
                                -> { nqp::bindpos($!reified,$!i,v) }
                              )
                            )
                          )
                        );
                        IterationEnd
                    }
                }

                method is-lazy() { $!todo.DEFINITE && $!todo.is-lazy }
            }.new(self)
        }

        # everything we need is already there
        elsif nqp::getattr(self,List,'$!reified').DEFINITE {
            class :: does Iterator {
                has int $!i;
                has $!reified;
                has $!descriptor;

                method !SET-SELF(\array) {
                    $!i           = -1;
                    $!reified    := nqp::getattr(array, List,  '$!reified');
                    $!descriptor := nqp::getattr(array, Array, '$!descriptor');
                    self
                }
                method new(\array) { nqp::create(self)!SET-SELF(array) }

                method pull-one() is raw {
                    nqp::ifnull(
                      nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
                      nqp::islt_i($!i,nqp::elems($!reified)) # found a hole
                        ?? nqp::p6bindattrinvres(
                             (my \v := nqp::p6scalarfromdesc($!descriptor)),
                             Scalar,
                             '$!whence',
                             -> { nqp::bindpos($!reified,$!i,v) }
                           )
                        !! IterationEnd
                    )
                }

                method push-all($target) {
                    my int $elems = nqp::elems($!reified);
                    nqp::while(   # doesn't sink
                      nqp::islt_i($!i = nqp::add_i($!i,1),$elems),
                      $target.push(nqp::ifnull(
                        nqp::atpos($!reified,$!i),
                        nqp::p6bindattrinvres(
                          (my \v := nqp::p6scalarfromdesc($!descriptor)),
                          Scalar,
                          '$!whence',
                          -> { nqp::bindpos($!reified,$!i,v) }
                        )
                      ))
                    );
                    IterationEnd
                }
            }.new(self)
        }

        # nothing now or in the future to iterate over
        else {
            Rakudo::Internals.EmptyIterator
        }
    }
    method from-iterator(Array:U: Iterator $iter) {
        my \result := nqp::create(self);
        my \buffer := nqp::create(IterationBuffer);
        my \todo := nqp::create(List::Reifier);
        nqp::bindattr(result, List, '$!reified', buffer);
        nqp::bindattr(result, List, '$!todo', todo);
        nqp::bindattr(todo, List::Reifier, '$!reified', buffer);
        nqp::bindattr(todo, List::Reifier, '$!current-iter', $iter);
        nqp::bindattr(todo, List::Reifier, '$!reification-target',
            result.reification-target());
        todo.reify-until-lazy();
        result
    }

    sub allocate-shaped-storage(\arr, @dims) {
        nqp::bindattr(arr, List, '$!reified',
            Rakudo::Internals.SHAPED-ARRAY-STORAGE(@dims, nqp::knowhow(), Mu));
        arr
    }

    my role ShapedArray[::TValue] does Positional[TValue] does Rakudo::Internals::ShapedArrayCommon {
        has $.shape;

        proto method AT-POS(|) is raw {*}
        multi method AT-POS(Array:U: |c) is raw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(Array:D: **@indices) is raw {
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
            allocate-shaped-storage(self, self.shape);
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
            self.shape.elems == 1
                ?? self.new(:shape(self.shape), self.List.reverse())
                !! X::IllegalOnFixedDimensionArray.new(operation => 'reverse').throw
        }

        method rotate(::?CLASS:D: Cool \n) {
            self.shape.elems == 1
                ?? self.new(:shape(self.shape), self.List.rotate(n))
                !! X::IllegalOnFixedDimensionArray.new(operation => 'rotate').throw
        }

        # A shaped array isn't lazy, we these methods don't need to go looking
        # into the "todo".
        multi method elems(::?CLASS:D:) is nodal {
            nqp::elems(nqp::getattr(self, List, '$!reified'))
        }
        method eager() { self }
        method is-lazy() { False }
    }

    proto method new(|) { * }
    multi method new(:$shape!) {
        self!new-internal(my @, $shape)
    }
    multi method new() {
        nqp::create(self)
    }
    multi method new(Mu:D \values, :$shape!) {
        self!new-internal(values, $shape)
    }
    multi method new(Mu:D \values) {
        nqp::create(self).STORE(values)
    }
    multi method new(**@values is raw, :$shape!) {
        self!new-internal(@values, $shape)
    }
    multi method new(**@values is raw) {
        nqp::create(self).STORE(@values)
    }

    method !new-internal(\values, \shape) {
        my \arr = nqp::create(self);
        if shape.DEFINITE {
            my \list-shape = nqp::istype(shape, List) ?? shape !! shape.list;
            allocate-shaped-storage(arr, list-shape);
            arr does ShapedArray[Mu];
            arr.^set_name('Array');
            nqp::bindattr(arr, arr.WHAT, '$!shape', list-shape);
            arr.STORE(values) if values;
        }
        else {
            arr.STORE(values);
        }
        arr
    }

    method !ensure-allocated(--> Nil) {
        nqp::bindattr(self, List, '$!reified', nqp::create(IterationBuffer))
            unless nqp::getattr(self, List, '$!reified').DEFINITE;
    }

    proto method STORE(|) { * }
    multi method STORE(Array:D: Iterable:D \iterable) {
        nqp::iscont(iterable)
            ?? self!STORE-ONE(iterable)
            !! self!STORE-ITERABLE(iterable)
    }
    multi method STORE(Array:D: Mu \item) {
        self!STORE-ONE(item)
    }
    method !STORE-ITERABLE(\iterable) {
        my \new-storage = nqp::create(IterationBuffer);
        my \iter = iterable.iterator;
        my \target = ArrayReificationTarget.new(new-storage,
            nqp::decont($!descriptor));
        if iter.push-until-lazy(target) =:= IterationEnd {
            nqp::bindattr(self, List, '$!todo', Mu);
        }
        else {
            my \new-todo = nqp::create(List::Reifier);
            nqp::bindattr(new-todo, List::Reifier, '$!reified', new-storage);
            nqp::bindattr(new-todo, List::Reifier, '$!current-iter', iter);
            nqp::bindattr(new-todo, List::Reifier, '$!reification-target', target);
            nqp::bindattr(self, List, '$!todo', new-todo);
        }
        nqp::bindattr(self, List, '$!reified', new-storage);
        self
    }
    method !STORE-ONE(Mu \item) {
        my \new-storage = nqp::create(IterationBuffer);
        nqp::push(new-storage,
            nqp::assign(nqp::p6scalarfromdesc($!descriptor), item));
        nqp::bindattr(self, List, '$!reified', new-storage);
        nqp::bindattr(self, List, '$!todo', Mu);
        self
    }

    method reification-target() {
        ArrayReificationTarget.new(
            nqp::getattr(self, List, '$!reified'),
            nqp::decont($!descriptor))
    }

    multi method flat(Array:U:) { self }
    multi method flat(Array:D:) { Seq.new(self.iterator) }

    multi method List(Array:D:) {
        self!ensure-allocated;
        X::Cannot::Lazy.new(:action<List>).throw if self.is-lazy;
        my \retval := nqp::create(List);
        my \reified := nqp::create(IterationBuffer);
        nqp::bindattr(retval, List, '$!reified', reified);
        my \target := ListReificationTarget.new(reified);
        self.iterator.push-all(target);
        retval
    }

    method shape() { (*,) }

    multi method AT-POS(Array:D: int $pos) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'),:got($pos),:range<0..Inf>)),
          nqp::if(
            (my $reified := nqp::getattr(self,List,'$!reified')).DEFINITE,
            nqp::ifnull(
              nqp::atpos($reified,$pos),           # found it!
              nqp::if(
                nqp::islt_i(
                  $pos,nqp::elems(nqp::getattr(self,List,'$!reified'))),
                self!AT-POS-CONTAINER($pos),       # it's a hole
                nqp::if(                           # too far out, try reifying
                  (my $todo := nqp::getattr(self,List,'$!todo')).DEFINITE,
                  nqp::stmts(
                    $todo.reify-at-least(nqp::add_i($pos,1)),
                    nqp::ifnull(
                      nqp::atpos($reified,$pos),   # reified ok
                      self!AT-POS-CONTAINER($pos)  # reifier didn't reach
                    )
                  ),
                  self!AT-POS-CONTAINER($pos)      # create an outlander
                )
              )
            ),
            # no reified, implies no todo
            nqp::stmts(                            # create reified
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer)),
              self!AT-POS-CONTAINER($pos)          # create an outlander
            )
          )
        )
    }
    # because this is a very hot path, we copied the code from the int candidate
    multi method AT-POS(Array:D: Int:D $pos) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'),:got($pos),:range<0..Inf>)),
          nqp::if(
            (my $reified := nqp::getattr(self,List,'$!reified')).DEFINITE,
            nqp::ifnull(
              nqp::atpos($reified,$pos),           # found it!
              nqp::if(
                nqp::islt_i(
                  $pos,nqp::elems(nqp::getattr(self,List,'$!reified'))),
                self!AT-POS-CONTAINER($pos),       # it's a hole
                nqp::if(                           # too far out, try reifying
                  (my $todo := nqp::getattr(self,List,'$!todo')).DEFINITE,
                  nqp::stmts(
                    $todo.reify-at-least(nqp::add_i($pos,1)),
                    nqp::ifnull(
                      nqp::atpos($reified,$pos),   # reified ok
                      self!AT-POS-CONTAINER($pos)  # reifier didn't reach
                    )
                  ),
                  self!AT-POS-CONTAINER($pos)      # create an outlander
                )
              )
            ),
            # no reified, implies no todo
            nqp::stmts(                            # create reified
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer)),
              self!AT-POS-CONTAINER($pos)          # create an outlander
            )
          )
        )
    }
    method !AT-POS-CONTAINER(int $pos) is raw {
        nqp::p6bindattrinvres(
          (my $scalar := nqp::p6scalarfromdesc($!descriptor)),
          Scalar,
          '$!whence',
          -> { nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,$scalar) }
        )
    }

    multi method ASSIGN-POS(Array:D: int $pos, Mu \assignee) {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'),:got($pos),:range<0..Inf>)),
          nqp::if(
            (my $reified := nqp::getattr(self,List,'$!reified')).DEFINITE,
            nqp::if(
              nqp::existspos($reified,$pos),
              (nqp::atpos($reified,$pos) = assignee),         # found it!
              nqp::if(
                nqp::islt_i($pos,nqp::elems($reified)),       # it's a hole
                (nqp::bindpos($reified,$pos,
                  nqp::p6scalarfromdesc($!descriptor)) = assignee),
                nqp::if(
                  (my $todo := nqp::getattr(self,List,'$!todo')).DEFINITE,
                  nqp::stmts(                                 # can reify
                    $todo.reify-at-least(nqp::add_i($pos,1)),
                    nqp::if(
                      nqp::existspos($reified,$pos),
                      (nqp::atpos($reified,$pos) = assignee), # reified
                      (nqp::bindpos($reified,$pos,            # outlander
                        nqp::p6scalarfromdesc($!descriptor)) = assignee),
                    )
                  ),
                  (nqp::bindpos($reified,$pos,                # outlander
                    nqp::p6scalarfromdesc($!descriptor)) = assignee)
                )
              )
            ),
            nqp::stmts(                                       # new outlander
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer)),
              (nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,
                nqp::p6scalarfromdesc($!descriptor)) = assignee)
            )
          )
        )
    }
    # because this is a very hot path, we copied the code from the int candidate
    multi method ASSIGN-POS(Array:D: Int:D $pos, Mu \assignee) {
        nqp::if(
          nqp::islt_i($pos,0),
          Failure.new(X::OutOfRange.new(
            :what($*INDEX // 'Index'),:got($pos),:range<0..Inf>)),
          nqp::if(
            (my $reified := nqp::getattr(self,List,'$!reified')).DEFINITE,
            nqp::if(
              nqp::existspos($reified,$pos),
              (nqp::atpos($reified,$pos) = assignee),         # found it!
              nqp::if(
                nqp::islt_i($pos,nqp::elems($reified)),       # it's a hole
                (nqp::bindpos($reified,$pos,
                  nqp::p6scalarfromdesc($!descriptor)) = assignee),
                nqp::if(
                  (my $todo := nqp::getattr(self,List,'$!todo')).DEFINITE,
                  nqp::stmts(                                 # can reify
                    $todo.reify-at-least(nqp::add_i($pos,1)),
                    nqp::if(
                      nqp::existspos($reified,$pos),
                      (nqp::atpos($reified,$pos) = assignee), # reified
                      (nqp::bindpos($reified,$pos,            # outlander
                        nqp::p6scalarfromdesc($!descriptor)) = assignee),
                    )
                  ),
                  (nqp::bindpos($reified,$pos,                # outlander
                    nqp::p6scalarfromdesc($!descriptor)) = assignee)
                )
              )
            ),
            nqp::stmts(                                       # new outlander
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer)),
              (nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,
                nqp::p6scalarfromdesc($!descriptor)) = assignee)
            )
          )
        )
    }

    multi method BIND-POS(Array:D: Int $pos, Mu \bindval) is raw {
        self!ensure-allocated();
        my int $ipos = $pos;
        my $todo := nqp::getattr(self, List, '$!todo');
        $todo.reify-at-least($ipos + 1) if $todo.DEFINITE;
        nqp::bindpos(nqp::getattr(self, List, '$!reified'), $ipos, bindval);
    }
    multi method BIND-POS(Array:D: int $pos, Mu \bindval) is raw {
        self!ensure-allocated();
        my $todo := nqp::getattr(self, List, '$!todo');
        $todo.reify-at-least($pos + 1) if $todo.DEFINITE;
        nqp::bindpos(nqp::getattr(self, List, '$!reified'), $pos, bindval);
    }

    multi method DELETE-POS(\pos, :$SINK) {
        fail X::Subscript::Negative.new(index => pos, type => self.WHAT) if pos < 0;

        my $value := self.AT-POS(pos); # needed for reification
        my $items := nqp::getattr(self, List, '$!reified');
        my $end   := self.end;

        pos <= $end
          ?? nqp::bindpos($items, pos, nqp::null())
          !! return self.default;

        if pos == $end {
            my int $pos = pos;
            nqp::pop($items);
            nqp::pop($items)
              while nqp::isge_i(--$pos,0)
                && nqp::isnull(nqp::atpos($items,$pos));
        }
        $value;
    }

    # MUST have a separate Slip variant to have it slip
    multi method push(Array:D: Slip \value) {
        self.is-lazy
          ?? Failure.new(X::Cannot::Lazy.new(action => 'push to'))
          !! self!append-list(value)
    }
    multi method push(Array:D: \value) {
        nqp::if(
          self.is-lazy,
          Failure.new(X::Cannot::Lazy.new(action => 'push to')),
          nqp::stmts(
            nqp::push(
              nqp::if(
                nqp::getattr(self,List,'$!reified').DEFINITE,
                nqp::getattr(self,List,'$!reified'),
                nqp::bindattr(self,List,'$!reified',
                  nqp::create(IterationBuffer))
              ),
              nqp::assign(nqp::p6scalarfromdesc($!descriptor),value)
            ),
            self
          )
        )
    }
    multi method push(Array:D: **@values is raw) {
        self.is-lazy
          ?? Failure.new(X::Cannot::Lazy.new(action => 'push to'))
          !! self!append-list(@values)
    }

    multi method append(Array:D: \value) {
        nqp::if(
          self.is-lazy,
          Failure.new(X::Cannot::Lazy.new(action => 'append to')),
          nqp::if(
            (nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable))),
            nqp::stmts(
              nqp::push(
                nqp::if(
                  nqp::getattr(self,List,'$!reified').DEFINITE,
                  nqp::getattr(self,List,'$!reified'),
                  nqp::bindattr(self,List,'$!reified',
                    nqp::create(IterationBuffer))
                ),
                nqp::assign(nqp::p6scalarfromdesc($!descriptor),value)
              ),
              self
            ),
            self!append-list(value.list)
          )
        )
    }
    multi method append(Array:D: **@values is raw) {
        self.is-lazy
          ?? Failure.new(X::Cannot::Lazy.new(action => 'append to'))
          !! self!append-list(@values)
    }
    method !append-list(@values) {
        nqp::if(
          nqp::eqaddr(
            @values.iterator.push-until-lazy(
              ArrayReificationTarget.new(
                nqp::if(
                  nqp::getattr(self,List,'$!reified').DEFINITE,
                  nqp::getattr(self,List,'$!reified'),
                  nqp::bindattr(self,List,'$!reified',
                    nqp::create(IterationBuffer))
                ),
                nqp::decont($!descriptor)
              )
            ),
            IterationEnd
          ),
          self,
          Failure.new(X::Cannot::Lazy.new(:action<push>,:what(self.^name)))
        )
    }

    multi method unshift(Array:D: Slip \value) {
        self!prepend-list(value)
    }
    multi method unshift(Array:D: \value) {
        nqp::stmts(
          nqp::unshift(
            nqp::if(
              nqp::getattr(self,List,'$!reified').DEFINITE,
              nqp::getattr(self,List,'$!reified'),
              nqp::bindattr(self,List,'$!reified',
                nqp::create(IterationBuffer))
            ),
            nqp::assign(nqp::p6scalarfromdesc($!descriptor),value)
          ),
          self
        )
    }
    multi method unshift(Array:D: **@values is raw) {
        self!prepend-list(@values)
    }
    multi method prepend(Array:D: \value) {
        nqp::if(
          (nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable))),
          nqp::stmts(
            nqp::unshift(
              nqp::if(
                nqp::getattr(self,List,'$!reified').DEFINITE,
                nqp::getattr(self,List,'$!reified'),
                nqp::bindattr(self,List,'$!reified',
                  nqp::create(IterationBuffer))
              ),
              nqp::assign(nqp::p6scalarfromdesc($!descriptor),value)
            ),
            self
          ),
          self!prepend-list(value.list)
        )
    }
    multi method prepend(Array:D: **@values is raw) {
        self!prepend-list(@values)
    }
    method !prepend-list(@values) {
        nqp::stmts(
          nqp::if(
            nqp::getattr(self,List,'$!reified').DEFINITE,
            nqp::splice(nqp::getattr(self,List,'$!reified'), # prepend existing
              nqp::stmts(
                @values.iterator.push-all(
                  ArrayReificationTarget.new(
                    (my $containers := nqp::create(IterationBuffer)),
                    nqp::decont($!descriptor)
                  )
                ),
                $containers
              ),
              0,
              0
            ),
            @values.iterator.push-all(        # no list yet, make this it
              ArrayReificationTarget.new(
                nqp::bindattr(self,List,'$!reified',
                  nqp::create(IterationBuffer)),
                nqp::decont($!descriptor)
              )
            )
          ),
          self
        )
    }

    method pop(Array:D:) is raw is nodal {
        nqp::if(
          self.is-lazy,
          Failure.new(X::Cannot::Lazy.new(action => 'pop from')),
          nqp::if(
            (nqp::getattr(self,List,'$!reified').DEFINITE
              && nqp::elems(nqp::getattr(self,List,'$!reified'))),
            nqp::pop(nqp::getattr(self,List,'$!reified')),
            Failure.new(X::Cannot::Empty.new(:action<pop>,:what(self.^name)))
          )
        )
    }

    method shift(Array:D:) is raw is nodal {
        nqp::if(
          nqp::getattr(self,List,'$!reified').DEFINITE
            && nqp::elems(nqp::getattr(self,List,'$!reified')),
          nqp::ifnull(  # handle holes
            nqp::shift(nqp::getattr(self,List,'$!reified')),
            Nil
          ),
          nqp::if(
            (nqp::getattr(self,List,'$!todo').DEFINITE
              && nqp::getattr(self,List,'$!todo').reify-at-least(1)),
            nqp::shift(nqp::getattr(self,List,'$!reified')),
            Failure.new(X::Cannot::Empty.new(:action<shift>,:what(self.^name)))
          )
        )
    }

    proto method splice(|) is nodal { * }
    multi method splice(Array:D \SELF: :$SINK) {
        if $SINK {
            SELF.STORE(());
            Nil
        }
        else {
            my @ret := SELF.of =:= Mu ?? Array.new !! Array[SELF.of].new;
            @ret.STORE(SELF);
            SELF.STORE(());
            @ret
        }
    }
    multi method splice(Array:D: $offset=0, $size=Whatever, @values?, :$SINK) {
        self!splice-list($offset, $size, @values, :$SINK)
    }
    multi method splice(Array:D: $offset=0, $size=Whatever, **@values, :$SINK) {
        self!splice-list($offset, $size, @values, :$SINK)
    }
    method !splice-list($offset, $size, @values, :$SINK) {
        my \splice-buffer = IterationBuffer.new;
        unless @values.iterator.push-until-lazy(splice-buffer) =:= IterationEnd {
            fail X::Cannot::Lazy.new(:action('splice in'));
        }

        my $todo = nqp::getattr(self, List, '$!todo');
        my $lazy;
        $lazy = !($todo.reify-until-lazy() =:= IterationEnd)
          if $todo.DEFINITE;

        my int $o = nqp::istype($offset,Callable)
          ?? $offset(self.elems)
          !! nqp::istype($offset,Whatever)
            ?? self.elems
            !! $offset.Int;
        X::OutOfRange.new(
          :what('Offset argument to splice'),
          :got($o),
          :range("0..{self.elems}"),
        ).fail if $o < 0 || (!$lazy && $o > self.elems); # one after list allowed for "push"
    
        my int $s = nqp::istype($size, Callable)
          ?? $size(self.elems - $o)
          !! !defined($size) || nqp::istype($size,Whatever)
             ?? self.elems - ($o min self.elems)
             !! $size.Int;
        X::OutOfRange.new(
          :what('Size argument to splice'),
          :got($s),
          :range("0..^{self.elems - $o}"),
        ).fail if $s < 0;

        # need to enforce type checking
        my $expected := self.of;
        unless self.of =:= Mu {
            my int $i = -1;
            my int $n = nqp::elems(splice-buffer);
            fail X::TypeCheck::Splice.new(
              :action<splice>,
              :got($_.WHAT),
              :$expected,
            ) unless nqp::istype(nqp::atpos(splice-buffer,$i),$expected)
              while nqp::islt_i(++$i,$n);
        }

        $todo.reify-at-least($o + $s) if $lazy;
        if $SINK {
            nqp::splice(nqp::getattr(self, List, '$!reified'),
                splice-buffer, $o, $s);
            Nil;
        }
        else {
            my @ret := $expected =:= Mu ?? Array.new !! Array[$expected].new;
            @ret = self[lazy $o..($o + $s - 1)] if $s;
            nqp::splice(nqp::getattr(self, List, '$!reified'),
                splice-buffer, $o, $s);
            @ret;
        }
    }

    # introspection
    method name() {
        nqp::isnull($!descriptor) ?? Nil !! $!descriptor.name
    }
    method of() {
        nqp::isnull($!descriptor) ?? Mu !! $!descriptor.of
    }
    method default() {
        nqp::isnull($!descriptor) ?? Any !! $!descriptor.default
    }
    method dynamic() {
        nqp::isnull($!descriptor) ?? Nil !! so $!descriptor.dynamic
    }
    multi method perl(Array:D \SELF:) {
        SELF.perlseen('Array', {
             '$' x nqp::iscont(SELF)  # self is always deconted
             ~ '['
             ~ self.map({nqp::decont($_).perl}).join(', ')
             ~ ',' x (self.elems == 1 && nqp::istype(self.AT-POS(0),Iterable))
             ~ ']'
        })
    }
    multi method gist(Array:D:) {
        self.gistseen('Array', { '[' ~ self.map({.gist}).join(' ') ~ ']' } )
    }
    multi method WHICH(Array:D:) { self.Mu::WHICH }

    my role TypedArray[::TValue] does Positional[TValue] {
        proto method new(|) { * }
        multi method new(**@values is raw, :$shape) {
            self!new-internal(@values, $shape);
        }
        multi method new(\values, :$shape) {
            self!new-internal(values, $shape);
        }

        method !new-internal(\values, \shape) {
            my \arr = nqp::create(self);
            nqp::bindattr(
              arr,
              Array,
              '$!descriptor',
              Perl6::Metamodel::ContainerDescriptor.new(
                :of(TValue), :rw(1), :default(TValue))
            );
            if shape.DEFINITE {
                my \list-shape = nqp::istype(shape, List) ?? shape !! shape.list;
                allocate-shaped-storage(arr, list-shape);
                arr does ShapedArray[Mu];
                arr.^set_name('Array');
                nqp::bindattr(arr, arr.WHAT, '$!shape', list-shape);
                arr.STORE(values) if values;
            } else {
                arr.STORE(values);
            }
            arr
        }

        proto method BIND-POS(|) { * }
        multi method BIND-POS(Array:D: Int $pos, TValue \bindval) is raw {
            my int $ipos = $pos;
            my $todo := nqp::getattr(self, List, '$!todo');
            $todo.reify-at-least($ipos + 1) if $todo.DEFINITE;
            nqp::bindpos(nqp::getattr(self, List, '$!reified'), $ipos, bindval)
        }
        multi method BIND-POS(Array:D: int $pos, TValue \bindval) is raw {
            my $todo := nqp::getattr(self, List, '$!todo');
            $todo.reify-at-least($pos + 1) if $todo.DEFINITE;
            nqp::bindpos(nqp::getattr(self, List, '$!reified'), $pos, bindval)
        }
        multi method perl(::?CLASS:D \SELF:) {
            my $args = self.map({ ($_ // TValue).perl(:arglist) }).join(', ');
            'Array[' ~ TValue.perl ~ '].new(' ~ $args ~ ')';
        }
    }
    method ^parameterize(Mu:U \arr, Mu:U \t, |c) {
        if c.elems == 0 {
            my $what := arr.^mixin(TypedArray[t]);
            # needs to be done in COMPOSE phaser when that works
            $what.^set_name("{arr.^name}[{t.^name}]");
            $what;
        }
        else {
            die "Can only type-constrain Array with [ValueType]"
        }
    }
}

# The [...] term creates an Array.
proto circumfix:<[ ]>(|) { * }
multi circumfix:<[ ]>() {
    nqp::create(Array)
}
multi circumfix:<[ ]>(Iterable:D \iterable) {
    my $reified;
    nqp::if(
      nqp::iscont(iterable),
      nqp::p6bindattrinvres(
        nqp::create(Array),List,'$!reified',
        nqp::stmts(
          nqp::push(
            ($reified := nqp::create(IterationBuffer)),
            nqp::assign(nqp::p6scalarfromdesc(nqp::null),iterable)
          ),
          $reified
        )
      ),
      nqp::if(
        nqp::eqaddr(iterable.WHAT,List),
        nqp::if(
          iterable.is-lazy,
          Array.from-iterator(iterable.iterator),
          nqp::stmts(     # immutable List
            (my int $elems = iterable.elems),  # reifies
            (my $params  := nqp::getattr(iterable,List,'$!reified')),
            (my int $i    = -1),
            ($reified := nqp::setelems(nqp::create(IterationBuffer),$elems)),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos($reified,$i,nqp::assign(
                nqp::p6scalarfromdesc(nqp::null),nqp::atpos($params,$i))
              )
            ),
            nqp::p6bindattrinvres(nqp::create(Array),List,'$!reified',$reified)
          ),
        ),
        Array.from-iterator(iterable.iterator)
      )
    )
}
multi circumfix:<[ ]>(Mu \x) {   # really only for [$foo]
    nqp::p6bindattrinvres(
      nqp::create(Array),List,'$!reified',
      nqp::stmts(
        nqp::push(
          (my $reified := nqp::create(IterationBuffer)),
          nqp::assign(nqp::p6scalarfromdesc(nqp::null),x)
        ),
        $reified
      )
    )
}

proto sub pop(@) {*}
multi sub pop(@a) { @a.pop }

proto sub shift(@) {*}
multi sub shift(@a) { @a.shift }

sub push   (\a, |elems) { a.push:    |elems }
sub append (\a, |elems) { a.append:  |elems }
sub unshift(\a, |elems) { a.unshift: |elems }
sub prepend(\a, |elems) { a.prepend: |elems }

sub splice(@arr, |c)         { @arr.splice(|c) }

# vim: ft=perl6 expandtab sw=4
