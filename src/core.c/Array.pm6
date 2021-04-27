# for our tantrums
my class X::TypeCheck { ... };
my class X::TypeCheck::Splice { ... }
my class X::Subscript::Negative { ... };

# An Array is a List that ensures every item added to it is in a Scalar
# container. It also supports push, pop, shift, unshift, splice, BIND-POS,
# and so forth.
my class Array { # declared in BOOTSTRAP
    # class Array is List
    #     has Mu $!descriptor;

    my class ArrayReificationTarget {
        has $!target;
        has $!descriptor;

        method new(\target, Mu \descriptor) {
            nqp::bindattr((my \rt = nqp::create(self)),self,'$!target',target);
            nqp::p6bindattrinvres(rt,self,'$!descriptor',descriptor)
        }

        method push(Mu \value --> Nil) {
            nqp::push($!target, nqp::p6scalarwithvalue($!descriptor, value));
        }

        method append(IterationBuffer:D \buffer --> Nil) {
            nqp::while(
              nqp::elems(buffer),
              nqp::push($!target,
                nqp::p6scalarwithvalue($!descriptor,nqp::shift(buffer))
              )
            );
        }
    }

    my class ListReificationTarget {
        has $!target;

        method new(\target) {
            nqp::p6bindattrinvres(nqp::create(self), self, '$!target', target);
        }

        method push(Mu \value --> Nil) {
            nqp::push($!target,nqp::decont(value));
        }

        method append(IterationBuffer:D \buffer --> Nil) {
            nqp::splice($!target,buffer,nqp::elems($!target),0)
        }
    }

    multi method clone(Array:D: --> Array:D) {
        my \iter := self.iterator;
        my \result := nqp::p6bindattrinvres(
          nqp::create(self),
          Array,
          '$!descriptor',
          nqp::isnull($!descriptor) ?? (nqp::null) !! nqp::clone($!descriptor)
        );

        nqp::if(
          nqp::eqaddr(
            IterationEnd,
            iter.push-until-lazy:
              my \target := ArrayReificationTarget.new(
                (my \buffer := nqp::create(IterationBuffer)),
                nqp::clone($!descriptor))),
          nqp::p6bindattrinvres(result, List, '$!reified', buffer),
          nqp::stmts(
            nqp::bindattr(result, List, '$!reified', buffer),
            nqp::bindattr((my \todo := nqp::create(List::Reifier)),
              List::Reifier,'$!current-iter', iter),
            nqp::bindattr(todo,
              List::Reifier,'$!reified', buffer),
            nqp::bindattr(todo,
              List::Reifier,'$!reification-target', target),
            nqp::p6bindattrinvres(result, List, '$!todo', todo)
          )
        )
    }

    my class Todo does Iterator {
        has int $!i;
        has $!array;
        has $!reified;
        has $!todo;
        has $!descriptor;

        method !SET-SELF(\array) {
            $!i           = -1;
            $!array      := array;
            $!reified    :=
              nqp::ifnull(
                nqp::getattr( array,List,'$!reified'),
                nqp::bindattr(array,List,'$!reified',
                  nqp::create(IterationBuffer))
              );
            $!todo       := nqp::getattr(array,List, '$!todo');
            $!descriptor := nqp::getattr(array,Array,'$!descriptor');
            self
        }
        method new(\array) { nqp::create(self)!SET-SELF(array) }

        method pull-one() is raw {
            nqp::ifnull(
              nqp::atpos($!reified,$!i = nqp::add_i($!i,1)),
              nqp::if(
                nqp::islt_i($!i,nqp::elems($!reified)),
                self!hole($!i),
                nqp::if(
                  nqp::isconcrete($!todo),
                  nqp::if(
                    nqp::islt_i(
                      $!i,
                      $!todo.reify-at-least(nqp::add_i($!i,1))
                    ),
                    nqp::atpos($!reified,$!i), # cannot be nqp::null
                    self!done
                  ),
                  IterationEnd
                )
              )
            )
        }
        method !hole(int $i) is raw {
            nqp::p6scalarfromcertaindesc(
              ContainerDescriptor::BindArrayPos.new($!descriptor,$!reified,$i)
            )
        }
        method !done() is raw {
            $!todo := nqp::bindattr($!array,List,'$!todo',Mu);
            IterationEnd
        }

        method push-until-lazy(\target) {
            nqp::if(
              nqp::isconcrete($!todo),
              nqp::stmts(
                (my int $elems = $!todo.reify-until-lazy),
                (my int $i = $!i),   # lexicals faster than attributes
                nqp::while(   # doesn't sink
                  nqp::islt_i($i = nqp::add_i($i,1),$elems),
                  target.push(nqp::atpos($!reified,$i))
                ),
                nqp::if(
                  $!todo.fully-reified,
                  nqp::stmts(
                    ($!i = $i),
                    self!done
                  ),
                  nqp::stmts(
                    ($!i = nqp::sub_i($elems,1)),
                    Mu
                  )
                )
              ),
              nqp::stmts(
                ($elems = nqp::elems($!reified)),
                ($i = $!i),
                nqp::while(   # doesn't sink
                  nqp::islt_i($i = nqp::add_i($i,1),$elems),
                  target.push(
                    nqp::ifnull(nqp::atpos($!reified,$i),self!hole($i))
                  )
                ),
                ($!i = $i),
                IterationEnd
              )
            )
        }

        method is-lazy() { $!todo.DEFINITE && $!todo.is-lazy }
    }
    multi method iterator(Array:D: --> Iterator:D) {
        nqp::isconcrete(nqp::getattr(self,List,'$!todo'))
          ?? Todo.new(self)                      # something to iterate over
          !! nqp::isconcrete(nqp::getattr(self,List,'$!reified'))
            ?? Rakudo::Iterator.ReifiedArray(    # everything is already there
                 self,
                 nqp::getattr(self,Array,'$!descriptor')
               )
            !! Rakudo::Iterator.Empty            # nothing now or in the future
    }
    method from-iterator(Array:U: Iterator $iter --> Array:D) {
        nqp::if(
          nqp::eqaddr(
            $iter.push-until-lazy(
              my \target := ArrayReificationTarget.new(
                (my \buffer := nqp::create(IterationBuffer)),
                BEGIN nqp::getcurhllsym('default_cont_spec')
              )
            ),
            IterationEnd
          ),
          nqp::p6bindattrinvres(nqp::create(self),List,'$!reified',buffer),
          nqp::stmts(
            nqp::bindattr((my \result := nqp::create(self)),
              List,'$!reified',buffer),
            nqp::bindattr((my \todo := nqp::create(List::Reifier)),
              List::Reifier,'$!current-iter',$iter),
            nqp::bindattr(todo,
              List::Reifier,'$!reified',buffer),
            nqp::bindattr(todo,
              List::Reifier,'$!reification-target',target),
            nqp::p6bindattrinvres(result,List,'$!todo',todo)
          )
        )
    }
    method from-list(Array:U: Mu \list --> Array:D) {
        my \params   := nqp::getattr(list,List,'$!reified');
        my int $elems = list.elems;  # reifies
        my int $i     = -1;
        my \reified  := nqp::create(IterationBuffer);
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::bindpos(
            reified, $i,
            nqp::p6scalarwithvalue(
              (BEGIN nqp::getcurhllsym('default_cont_spec')),
              nqp::decont(nqp::atpos(params,$i))
            )
          )
        );
        nqp::p6bindattrinvres(nqp::create(Array),List,'$!reified',reified)
    }

    # handle non-straightforward shapes
    method !difficult-shape(\shape --> Array:D) {
        nqp::if(
          Metamodel::EnumHOW.ACCEPTS(shape.HOW),
          self.set-shape(shape.^elems),
          nqp::stmts(
            warn("Ignoring [{ shape.^name }] as shape specification, did you mean 'my { shape.^name } @foo' ?"),
            nqp::create(self)
          )
        )
    }

    proto method new(|) {*}
    multi method new(Array: :$shape! --> Array:D) {
        nqp::isconcrete($shape)
          ?? self.set-shape($shape)
          !! self!difficult-shape($shape)
    }
    multi method new(Array: --> Array:D) {
        nqp::create(self)
    }
    multi method new(Array: \values, :$shape! --> Array:D) {
        (nqp::isconcrete($shape)
          ?? self.set-shape($shape)
          !! self!difficult-shape($shape)
        ).STORE(values)
    }
    multi method new(Array: \values --> Array:D) {
        nqp::create(self).STORE(values)
    }
    multi method new(Array: **@values is raw, :$shape! --> Array:D) {
        (nqp::isconcrete($shape)
          ?? self.set-shape($shape)
          !! self!difficult-shape($shape)
        ).STORE(@values)
    }
    multi method new(Array: **@values is raw --> Array:D) {
        nqp::create(self).STORE(@values)
    }

    proto method STORE(Array:D: |) {*}
    multi method STORE(Array:D: Iterable:D \iterable --> Array:D) {
        my \buffer = nqp::create(IterationBuffer);
        nqp::if(
          nqp::iscont(iterable),
          nqp::stmts(                          # only a single element
            nqp::push(
              buffer,
              nqp::p6scalarwithvalue($!descriptor,iterable)
            ),
            nqp::bindattr(self,List,'$!todo',Mu)
          ),
          nqp::if(                             # a real iterator with N elems
            nqp::eqaddr(
              (my \iter = iterable.iterator).push-until-lazy(
                (my \target = ArrayReificationTarget.new(
                  buffer,nqp::decont($!descriptor)
                ))
              ),
              IterationEnd
            ),
            nqp::bindattr(self,List,'$!todo',Mu),  # exhausted
            nqp::stmts(                            # still left to do
              nqp::bindattr(self,List,'$!todo',
                my \todo = nqp::create(List::Reifier)),
              nqp::bindattr(todo,List::Reifier,'$!reified',buffer),
              nqp::bindattr(todo,List::Reifier,'$!current-iter',iter),
              nqp::bindattr(todo,List::Reifier,'$!reification-target',target),
            )
          )
        );
        nqp::p6bindattrinvres(self,List,'$!reified',buffer)
    }
    multi method STORE(Array:D: QuantHash:D \qh --> Array:D) {
        my \buffer = nqp::create(IterationBuffer);
        nqp::iscont(qh)
          ?? nqp::push(buffer,nqp::p6scalarwithvalue($!descriptor,qh))
          !! qh.iterator.push-all(
               ArrayReificationTarget.new(buffer,nqp::decont($!descriptor))
             );
        nqp::bindattr(self,List,'$!todo',Mu);  # exhausted
        nqp::p6bindattrinvres(self,List,'$!reified',buffer)
    }
    multi method STORE(Array:D: Mu \item --> Array:D) {
        nqp::push(
          (my \buffer = nqp::create(IterationBuffer)),
          nqp::p6scalarwithvalue($!descriptor, item)
        );
        nqp::bindattr(self,List,'$!todo',Mu);
        nqp::p6bindattrinvres(self,List,'$!reified',buffer)
    }

    method reification-target(Array:D: --> ArrayReificationTarget:D) {
        ArrayReificationTarget.new(
            nqp::getattr(self, List, '$!reified'),
            nqp::decont($!descriptor))
    }

    multi method Slip(Array:D: --> Slip:D) {

       # A Slip-With-Descripto is a special kind of Slip that also has a
       # descriptor to be able to generate containers for null elements that
       # have type and default information.
        my class Slip-With-Descriptor is Slip {
            has $!descriptor;

            method iterator() {
                Rakudo::Iterator.ReifiedArray(self,$!descriptor)
            }

            multi method AT-POS(Int:D \pos) {
                nqp::ifnull(
                  nqp::atpos(nqp::getattr(self,List,'$!reified'),pos),
                  nqp::p6scalarfromcertaindesc(ContainerDescriptor::BindArrayPos.new(
                    $!descriptor, nqp::getattr(self,List,'$!reified'), pos))
                )
            }
            method default() { $!descriptor.default }
        }
        BEGIN Slip-With-Descriptor.^set_name("Slip");

        nqp::isconcrete(nqp::getattr(self,List,'$!todo'))
             # We're not fully reified, and so have internal mutability still.
             # The safe thing to do is to take an iterator of ourself and build
             # the Slip out of that.
          ?? Slip.from-iterator(self.iterator)
             # We're fully reified.  Make a Slip that shares our reified buffer
             # but that will fill in default values for nulls.
          !! nqp::isconcrete(nqp::getattr(self,List,'$!reified'))
            ?? nqp::p6bindattrinvres(
                 nqp::p6bindattrinvres(
                   nqp::create(Slip-With-Descriptor),
                   Slip-With-Descriptor,
                   '$!descriptor',
                   $!descriptor
                 ),
                 List,
                 '$!reified',
                 nqp::clone(nqp::getattr(self,List,'$!reified'))
               )
            !! nqp::create(Slip)
    }

    method FLATTENABLE_LIST() is implementation-detail {
        nqp::if(
          nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
          nqp::stmts(
            nqp::getattr(self,List,'$!todo').reify-all,
            nqp::getattr(self,List,'$!reified')
          ),
          nqp::if(
            nqp::isconcrete(my $reified := nqp::getattr(self,List,'$!reified')),
            nqp::stmts(
              nqp::if(
                (my int $elems = nqp::elems($reified)),
                nqp::stmts(
                  (my int $i = -1),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                    nqp::if(
                      nqp::isnull(nqp::atpos($reified,$i)),
                      nqp::bindpos(
                        $reified,
                        $i,
                        nqp::p6scalarfromcertaindesc($!descriptor)
                      )
                    )
                  )
                )
              ),
              nqp::getattr(self,List,'$!reified')
            ),
            nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
          )
        )
    }

    multi method flat(Array:U:) { self }
    multi method flat(Array:D:) { Seq.new(self.iterator) }

    method reverse(Array:D: --> Seq:D) is nodal {
        self.is-lazy    # reifies
          ?? self.fail-iterator-cannot-be-lazy('.reverse')
          !! Seq.new: nqp::getattr(self,List,'$!reified')
            ?? Rakudo::Iterator.ReifiedReverse(self, $!descriptor)
            !! Rakudo::Iterator.Empty
    }

    method rotate(List:D: Int(Cool) $rotate = 1 --> Seq:D) is nodal {
        self.is-lazy    # reifies
          ?? self.fail-iterator-cannot-be-lazy('.rotate')
          !! Seq.new: nqp::getattr(self,List,'$!reified')
            ?? Rakudo::Iterator.ReifiedRotate($rotate, self, $!descriptor)
            !! Rakudo::Iterator.Empty
    }

    multi method List(Array:D: :$view --> List:D) {  # :view is implementation-detail
        nqp::if(
          self.is-lazy,                           # can't make a List
          self.throw-iterator-cannot-be-lazy('.List'),

          nqp::if(                                # all reified
            nqp::isconcrete(my $reified := nqp::getattr(self,List,'$!reified')),
            nqp::if(
              $view,                              # assume no change in array
              $reified.List,
              nqp::stmts(                         # make cow copy
                (my int $elems = nqp::elems($reified)),
                (my $cow := nqp::setelems(nqp::create(IterationBuffer),$elems)),
                (my int $i = -1),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos($cow,$i,nqp::ifnull(nqp::decont(nqp::atpos($reified,$i)),Nil)),
                ),
                $cow.List
              )
            ),
            nqp::create(List)                     # was empty, is empty
          )
        )
    }

    method shape(Array: --> List:D) { (*,) }  # should probably be Array:D:

    multi method AT-POS(Array:D: Int:D $pos) is raw {
        my $reified := nqp::getattr(self, List, '$!reified');
        my $result := nqp::bitand_i(nqp::isge_i($pos, 0), nqp::isconcrete($reified))
            ?? nqp::atpos($reified, $pos)
            !! nqp::null;
        nqp::ifnull($result, self!AT_POS_SLOW($pos))
    }

    # handle any lookup that's not simple
    method !AT_POS_SLOW(int $pos) is raw {
        nqp::if(
          nqp::islt_i($pos, 0),
          self!INDEX_OOR($pos),
          nqp::if(
            nqp::isconcrete(my $reified := nqp::getattr(self,List,'$!reified')),
            nqp::if(
              nqp::islt_i($pos,nqp::elems($reified)),
              self!AT_POS_CONTAINER($pos),        # it's a hole
              nqp::if(                           # too far out, try reifying
                nqp::isconcrete(my $todo := nqp::getattr(self,List,'$!todo')),
                nqp::stmts(
                  $todo.reify-at-least(nqp::add_i($pos,1)),
                  nqp::ifnull(
                    nqp::atpos($reified,$pos),   # reified ok
                    self!AT_POS_CONTAINER($pos)  # reifier didn't reach
                  )
                ),
                self!AT_POS_CONTAINER($pos)      # create an outlander
              )
            ),
            # no reified, implies no todo
            nqp::stmts(                          # create reified
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer)),
              self!AT_POS_CONTAINER($pos)        # create an outlander
            )
          )
        )
    }
    method !AT_POS_CONTAINER(int $pos) is raw {
        my $desc := $!descriptor;
        my $scalar := nqp::create(Scalar);
        nqp::bindattr($scalar, Scalar, '$!value', nqp::isnull($desc)
            ?? Any
            !! nqp::getattr($desc, ContainerDescriptor, '$!default'));
        nqp::bindattr($scalar, Scalar, '$!descriptor',
            ContainerDescriptor::BindArrayPos.new(
                $desc, nqp::getattr(self,List,'$!reified'), $pos));
        $scalar
    }

    multi method ASSIGN-POS(Array:D: Int:D $pos, Mu \assignee) is raw {
        nqp::isge_i($pos,0)
          ?? nqp::bitand_i(
               nqp::isconcrete(my \reified := nqp::getattr(self,List,'$!reified')),
               nqp::not_i(nqp::isconcrete(nqp::getattr(self,List,'$!todo'))),
             )
            ?? nqp::p6assign(
                 nqp::ifnull(
                   nqp::atpos(reified, $pos),
                   nqp::bindpos(
                     reified,
                     $pos,
                     nqp::p6bindattrinvres(
                       nqp::create(Scalar),Scalar,'$!descriptor',$!descriptor
                     )
                   )
                 ),
                 nqp::decont(assignee)
               )
            !! self!ASSIGN_POS_SLOW_PATH($pos, assignee)
          !! self!INDEX_OOR($pos)
    }

    method !ASSIGN_POS_SLOW_PATH(Array:D: int $pos, Mu \assignee) is raw {
        my \reified := nqp::getattr(self,List,'$!reified');
        nqp::p6assign(
          nqp::if(
            nqp::isconcrete(reified),
            nqp::ifnull(
              nqp::atpos(reified,$pos),
              nqp::if(
                nqp::islt_i($pos,nqp::elems(reified)), # it's a hole
                nqp::bindpos(
                  reified,
                  $pos,
                  nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!descriptor', $!descriptor)
                ),
                nqp::if(
                  nqp::isconcrete(my \todo := nqp::getattr(self,List,'$!todo')),
                  nqp::stmts(                    # can reify
                    todo.reify-at-least(nqp::add_i($pos,1)),
                    nqp::ifnull(
                      nqp::atpos(reified,$pos),  # reified
                      nqp::bindpos(              # outlander
                        reified,
                        $pos,
                        nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!descriptor', $!descriptor)
                      )
                    )
                  ),
                  nqp::bindpos(                  # outlander without todo
                    reified,
                    $pos,
                    nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!descriptor', $!descriptor)
                  )
                )
              )
            ),
            nqp::bindpos(                        # new outlander without reified
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer)),
              $pos,
              nqp::p6bindattrinvres(nqp::create(Scalar), Scalar, '$!descriptor', $!descriptor)
            )
          ),
          nqp::decont(assignee)
        )
    }

    multi method BIND-POS(Array:D: Int:D $pos, Mu \bindval) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          self!INDEX_OOR($pos),
          nqp::stmts(
            nqp::if(
              nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
              nqp::if(
                nqp::isge_i(
                  $pos,
                  nqp::elems(nqp::getattr(self,List,'$!reified'))
                ) && nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
                nqp::getattr(self,List,'$!todo').reify-at-least(
                  nqp::add_i($pos,1)),
              ),
              nqp::bindattr(self,List,'$!reified',nqp::create(IterationBuffer))
            ),
            nqp::bindpos(nqp::getattr(self,List,'$!reified'),$pos,bindval)
          )
        )
    }

    multi method DELETE-POS(Array:D: Int:D $pos) is raw {
        nqp::if(
          nqp::islt_i($pos,0),
          self!INDEX_OOR($pos),
          nqp::if(
            nqp::isconcrete(my $reified := nqp::getattr(self,List,'$!reified')),
            nqp::stmts(
              nqp::if(
                nqp::isconcrete(my $todo := nqp::getattr(self,List,'$!todo')),
                $todo.reify-at-least(nqp::add_i($pos,1)),
              ),
              nqp::if(
                nqp::isle_i(                               # something to delete
                  $pos,my int $end = nqp::sub_i(nqp::elems($reified),1)),
                nqp::stmts(
                  (my $value := nqp::ifnull(               # save the value
                    nqp::atpos($reified,$pos),
                    self.default
                  )),
                  nqp::bindpos($reified,$pos,nqp::null),   # remove this one
                  nqp::if(
                    nqp::iseq_i($pos,$end) && nqp::not_i(nqp::defined($todo)),
                    nqp::stmts(                            # shorten from end
                      (my int $i = $pos),
                      nqp::while(
                        (nqp::isge_i(($i = nqp::sub_i($i,1)),0)
                          && nqp::not_i(nqp::existspos($reified,$i))),
                        nqp::null
                      ),
                      nqp::setelems($reified,nqp::add_i($i,1))
                    ),
                  ),
                  $value                                   # value, if any
                ),
                self.default                               # outlander
              ),
            ),
            self.default                                 # no elements
          )
        )
    }

    method !INDEX_OOR($pos) {
      Failure.new(X::OutOfRange.new(
          :what($*INDEX // 'Index'), :got($pos), :range<0..^Inf>
      ))
    }

    # MUST have a separate Slip variant to have it slip
    multi method push(Array:D: Slip \value --> Array:D) {
        self.is-lazy
          ?? self.throw-iterator-cannot-be-lazy('push to')
          !! self!append-list(value)
    }
    multi method push(Array:D: \value --> Array:D) {
        nqp::if(
          self.is-lazy,
          self.throw-iterator-cannot-be-lazy('push to'),
          nqp::stmts(
            nqp::push(
              nqp::if(
                nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
                nqp::getattr(self,List,'$!reified'),
                nqp::bindattr(self,List,'$!reified',
                  nqp::create(IterationBuffer))
              ),
              nqp::p6scalarwithvalue($!descriptor,value)
            ),
            self
          )
        )
    }
    multi method push(Array:D: **@values is raw --> Array:D) {
        self.is-lazy
          ?? self.throw-iterator-cannot-be-lazy('push to')
          !! self!append-list(@values)
    }

    multi method append(Array:D: \value --> Array:D) {
        nqp::if(
          self.is-lazy,
          self.throw-iterator-cannot-be-lazy('append to'),
          nqp::if(
            (nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable))),
            nqp::stmts(
              nqp::push(
                nqp::if(
                  nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
                  nqp::getattr(self,List,'$!reified'),
                  nqp::bindattr(self,List,'$!reified',
                    nqp::create(IterationBuffer))
                ),
                nqp::p6scalarwithvalue($!descriptor,value)
              ),
              self
            ),
            self!append-list(value.list)
          )
        )
    }
    multi method append(Array:D: **@values is raw --> Array:D) {
        self.is-lazy
          ?? self.throw-iterator-cannot-be-lazy('append to')
          !! self!append-list(@values)
    }
    method !append-list(Array:D: @values --> Array:D) {
        nqp::if(
          nqp::eqaddr(
            @values.iterator.push-until-lazy(
              ArrayReificationTarget.new(
                nqp::if(
                  nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
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
          self.throw-iterator-cannot-be-lazy('push')
        )
    }

    multi method unshift(Array:D: Slip \value --> Array:D) {
        self!prepend-list(value)
    }
    multi method unshift(Array:D: \value --> Array:D) {
        nqp::stmts(
          nqp::unshift(
            nqp::if(
              nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
              nqp::getattr(self,List,'$!reified'),
              nqp::bindattr(self,List,'$!reified',
                nqp::create(IterationBuffer))
            ),
            nqp::p6scalarwithvalue($!descriptor,value)
          ),
          self
        )
    }
    multi method unshift(Array:D: **@values is raw --> Array:D) {
        self!prepend-list(@values)
    }
    multi method prepend(Array:D: \value --> Array:D) {
        nqp::if(
          (nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable))),
          nqp::stmts(
            nqp::unshift(
              nqp::if(
                nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
                nqp::getattr(self,List,'$!reified'),
                nqp::bindattr(self,List,'$!reified',
                  nqp::create(IterationBuffer))
              ),
              nqp::p6scalarwithvalue($!descriptor,value)
            ),
            self
          ),
          self!prepend-list(value.list)
        )
    }
    multi method prepend(Array:D: **@values is raw --> Array:D) {
        self!prepend-list(@values)
    }
    method !prepend-list(Array:D: @values --> Array:D) {
        nqp::if(
          nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
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
        );
        self
    }

    method pop(Array:D:) is nodal {
        self.is-lazy
          ?? self.fail-iterator-cannot-be-lazy('pop from')
          !! nqp::isconcrete(nqp::getattr(self,List,'$!reified'))
               && nqp::elems(nqp::getattr(self,List,'$!reified'))
            ?? nqp::pop(nqp::getattr(self,List,'$!reified'))
            !! self.fail-cannot-be-empty('pop')
    }

    method shift(Array:D:) is nodal {
        nqp::isconcrete(nqp::getattr(self,List,'$!reified'))
          && nqp::elems(nqp::getattr(self,List,'$!reified'))
          ?? nqp::ifnull(  # handle holes
               nqp::shift(nqp::getattr(self,List,'$!reified')),
               Nil
             )
          !! nqp::isconcrete(nqp::getattr(self,List,'$!todo'))
               && nqp::getattr(self,List,'$!todo').reify-at-least(1)
            ?? nqp::shift(nqp::getattr(self,List,'$!reified'))
            !! self.fail-cannot-be-empty('shift')
    }

    my $empty := nqp::create(IterationBuffer); # splicing in without values
    #------ splice() candidates
    multi method splice(Array:D \SELF: --> Array:D) {
        nqp::if(
          nqp::isconcrete(nqp::getattr(SELF,List,'$!reified')),
          nqp::stmts(
            (my $result := nqp::create(SELF)),
            nqp::bindattr($result,Array,'$!descriptor',$!descriptor),
            nqp::stmts(       # transplant the internals
              nqp::bindattr($result,List,'$!reified',
                nqp::getattr(SELF,List,'$!reified')),
              nqp::if(
                nqp::isconcrete(nqp::getattr(SELF,List,'$!todo')),
                nqp::bindattr($result,List,'$!todo',
                  nqp::getattr(SELF,List,'$!todo')),
              )
            ),
            (SELF = nqp::create(SELF)),  # XXX this preserves $!descriptor ??
            $result
          ),
          nqp::p6bindattrinvres(   # nothing to return, so create new one
            nqp::create(SELF),Array,'$!descriptor',$!descriptor)
        )
    }

    #------ splice(offset) candidates
    multi method splice(Array:D: Whatever $ --> Array:D) {
        nqp::p6bindattrinvres(     # nothing to return, so create new one
          nqp::create(self),Array,'$!descriptor',$!descriptor)
    }
    multi method splice(Array:D: Callable:D $offset --> Array:D) {
        self.splice($offset(self.elems))
    }
    multi method splice(Array:D: Int:D $offset --> Array:D) {
        nqp::if(
          $offset,
          nqp::if(
            nqp::islt_i(nqp::unbox_i($offset),0),
            self!splice-offset-fail($offset),
            nqp::if(
              nqp::isconcrete(my $todo := nqp::getattr(self,List,'$!todo')),
              nqp::if(
                nqp::isge_i(
                  $todo.reify-at-least($offset),nqp::unbox_i($offset)),
                self!splice-offset(nqp::unbox_i($offset)),
                self!splice-offset-fail($offset)
              ),
              nqp::if(
                nqp::isconcrete(nqp::getattr(self,List,'$!reified'))
                  && nqp::isge_i(
                    nqp::elems(nqp::getattr(self,List,'$!reified')),
                    nqp::unbox_i($offset)
                ),
                self!splice-offset(nqp::unbox_i($offset)),
                self!splice-offset-fail($offset)
              )
            )
          ),
          self.splice       # offset 0, take the quick route out
        )
    }
    method !splice-offset(Array:D: int $offset --> Array:D) {
        my $reified := nqp::getattr(self,List,'$!reified');
        my int $elems = nqp::elems($reified);
        my $result:= nqp::create(self);

        nqp::unless(
          nqp::iseq_i($offset,$elems),
          nqp::stmts(
            nqp::bindattr($result,List,'$!reified',nqp::slice($reified,$offset,-1)),
            nqp::splice(
              $reified,
              $empty,
              $offset,
              nqp::sub_i(nqp::elems($reified),$offset)
            ),
          )
        );
        nqp::p6bindattrinvres($result,Array,'$!descriptor',$!descriptor)
    }
    method !splice-offset-fail(Array:D: $got) {
        X::OutOfRange.new(
          :what('Offset argument to splice'), :$got, :range("0..{self.elems}")
        ).throw
    }

    #------ splice(offset,size) candidates
    multi method splice(Array:D: Whatever $, Whatever $ --> Array:D) {
        nqp::p6bindattrinvres(     # nothing to return, so create new one
          nqp::create(self),Array,'$!descriptor',$!descriptor)
    }
    multi method splice(Array:D: Whatever $, Int:D $size --> Array:D) {
        self.splice(self.elems,$size)
    }
    multi method splice(Array:D: Whatever $, Callable:D $size --> Array:D) {
        my int $elems = self.elems;
        self.splice($elems,$size(nqp::sub_i($elems,$elems)));
    }
    multi method splice(Array:D:
      Callable:D $offset, Callable:D $size
    --> Array:D) {
        my int $elems = self.elems;
        my int $from  = $offset($elems);
        self.splice($from,$size(nqp::sub_i($elems,$from)))
    }
    multi method splice(Array:D: Callable:D $offset, Whatever $ --> Array:D) {
        self.splice($offset(self.elems))
    }
    multi method splice(Array:D: Callable:D $offset, Int:D $size --> Array:D) {
        self.splice($offset(self.elems),$size)
    }
    multi method splice(Array:D: Int:D $offset, Whatever $ --> Array:D) {
        self.splice($offset)
    }
    multi method splice(Array:D: Int:D $offset, Callable:D $size --> Array:D) {
        self.splice($offset,$size(self.elems - $offset))
    }
    multi method splice(Array:D: Int:D $offset, Int:D $size --> Array:D) {
        nqp::if(
          nqp::islt_i(nqp::unbox_i($offset),0),
          self!splice-offset-fail($offset),
          nqp::if(
            nqp::islt_i(nqp::unbox_i($size),0),
            self!splice-size-fail($size,$offset),
            nqp::if(
              nqp::isconcrete(my $todo := nqp::getattr(self,List,'$!todo')),
              nqp::if(
                nqp::isge_i(
                  $todo.reify-at-least(
                    nqp::add_i(nqp::unbox_i($offset),nqp::unbox_i($size))
                  ),nqp::unbox_i($offset)),
                self!splice-offset-size(
                  nqp::unbox_i($offset),nqp::unbox_i($size)),
                self!splice-size-fail($size,$offset)
              ),
              nqp::if(
                nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
                nqp::if(
                  nqp::isge_i(
                    nqp::elems(nqp::getattr(self,List,'$!reified')),
                    nqp::unbox_i($offset)),
                  self!splice-offset-size(
                    nqp::unbox_i($offset),nqp::unbox_i($size)),
                  self!splice-size-fail($size,$offset)
                ),
                nqp::if(
                  nqp::iseq_i(nqp::unbox_i($offset),0),
                  nqp::p6bindattrinvres(     # nothing to return, create new
                    nqp::create(self),Array,'$!descriptor',$!descriptor),
                  self!splice-offset-fail($offset)
                )
              )
            )
          )
        )
    }
    method !splice-offset-size(Array:D: int $offset,int $size --> Array:D) {
        my $result := self!splice-save($offset,$size,my int $removed);
        nqp::splice(
          nqp::getattr(self,List,'$!reified'),$empty,$offset,$removed
        );
        $result
    }
    method !splice-save(Array:D: int $offset,int $size, \removed --> Array:D) {
        my $reified := nqp::getattr(self,List,'$!reified');
        my $result:= nqp::create(self);
        nqp::if(
          (removed = nqp::if(
            nqp::isgt_i(nqp::add_i($offset,$size),nqp::elems($reified)),
            nqp::sub_i(nqp::elems($reified),$offset),
            $size
          )),
          nqp::bindattr(
            $result,
            List,
            '$!reified',
            nqp::slice($reified,$offset,nqp::sub_i(nqp::add_i($offset,removed),1))
          )
        );
        nqp::p6bindattrinvres($result,Array,'$!descriptor',$!descriptor)
    }
    method !splice-size-fail(Array:D: $got,$offset) {
        $offset > self.elems
          ?? self!splice-offset-fail($offset)
          !! X::OutOfRange.new(
               :what('Size argument to splice'),
               :$got,
               :range("0..^{self.elems - $offset}")
             ).throw
    }
    #------ splice(offset,size,array) candidates

    # we have these 9 multies to avoid infiniloop when incorrect types are
    # given to $offset/$size. Other attempts to resolve this showed 30%+
    # performance decreases
    multi method splice(Array:D:
      Whatever $offset, Whatever $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Whatever $offset, Callable:D $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Whatever $offset, Int:D $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Callable:D $offset, Whatever $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Callable:D $offset, Callable:D $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Callable:D $offset, Int:D $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Int:D $offset, Whatever $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Int:D $offset, Callable:D $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }
    multi method splice(Array:D:
      Int:D $offset, Int:D $size, **@new
    --> Array:D) {
        self.splice($offset, $size, @new)
    }

    multi method splice(Array:D: Whatever $, Whatever $, @new --> Array:D) {
        self.splice(self.elems,0,@new)
    }
    multi method splice(Array:D: Whatever $, Int:D $size, @new --> Array:D) {
        self.splice(self.elems,$size,@new)
    }
    multi method splice(Array:D:
      Whatever $, Callable:D $size, @new
    --> Array:D) {
        my int $elems = self.elems;
        self.splice($elems,$size(nqp::sub_i($elems,$elems)),@new);
    }
    multi method splice(Array:D:
      Callable:D $offset, Callable:D $size, @new
    --> Array:D) {
        my int $elems = self.elems;
        my int $from  = $offset($elems);
        self.splice($from,$size(nqp::sub_i($elems,$from)),@new)
    }
    multi method splice(Array:D:
      Callable:D $offset, Whatever $, @new
    --> Array:D) {
        my int $elems = self.elems;
        my int $from  = $offset($elems);
        self.splice($from,nqp::sub_i($elems,$from),@new)
    }
    multi method splice(Array:D:
      Callable:D $offset, Int:D $size, @new
    --> Array:D) {
        self.splice($offset(self.elems),$size,@new)
    }
    multi method splice(Array:D:
      Int:D $offset, Whatever $, @new
    --> Array:D) {
        self.splice($offset,self.elems - $offset,@new)
    }
    multi method splice(Array:D:
      Int:D $offset, Callable:D $size, @new
    --> Array:D) {
        self.splice($offset,$size(self.elems - $offset),@new)
    }
    multi method splice(Array:D:
      Int:D $offset, Int:D $size, @new
    --> Array:D) {
        nqp::if(
          nqp::islt_i(nqp::unbox_i($offset),0),
          self!splice-offset-fail($offset),
          nqp::if(
            nqp::islt_i(nqp::unbox_i($size),0),
            self!splice-size-fail($size,$offset),
            nqp::if(
              nqp::isconcrete(my $todo := nqp::getattr(self,List,'$!todo')),
              nqp::if(
                nqp::isge_i(
                  $todo.reify-at-least(
                    nqp::add_i(nqp::unbox_i($offset),nqp::unbox_i($size))
                  ),nqp::unbox_i($offset)),
                self!splice-offset-size-new(
                  nqp::unbox_i($offset),nqp::unbox_i($size),@new),
                self!splice-size-fail($size,$offset)
              ),
              nqp::if(
                nqp::isge_i(
                  nqp::elems(nqp::if(
                    nqp::isconcrete(nqp::getattr(self,List,'$!reified')),
                    nqp::getattr(self,List,'$!reified'),
                    nqp::bindattr(self,List,'$!reified',
                      nqp::create(IterationBuffer))
                  )),
                  nqp::unbox_i($offset),
                ),
                self!splice-offset-size-new(
                  nqp::unbox_i($offset),nqp::unbox_i($size),@new),
                self!splice-offset-fail($offset)
              )
            )
          )
        )
    }
    method !splice-offset-size-new(Array:D:
      int $offset,int $size,@new
    --> Array:D) {
        nqp::if(
          nqp::eqaddr(@new.iterator.push-until-lazy(
            (my $new := nqp::create(IterationBuffer))),IterationEnd),
          nqp::if(      # reified all values to splice in
            (nqp::isnull($!descriptor) || nqp::eqaddr(self.of,Mu)),
            nqp::stmts( # no typecheck needed
              (my $result := self!splice-save($offset,$size,my int $removed)),
              nqp::splice(
                nqp::getattr(self,List,'$!reified'),$new,$offset,$removed),
              $result
            ),
            nqp::stmts( # typecheck the values first
              (my $expected := self.of),
              (my int $elems = nqp::elems($new)),
              (my int $i = -1),
              nqp::while(
                (nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                  && nqp::istype(nqp::atpos($new,$i),$expected)),
                nqp::null
              ),
              nqp::if(
                nqp::islt_i($i,$elems),   # exited loop because of wrong type
                X::TypeCheck::Splice.new(
                  :action<splice>,
                  :got(nqp::atpos($new,$i).WHAT),
                  :$expected
                ).throw,
                nqp::stmts(
                  ($result := self!splice-save($offset,$size,$removed)),
                  nqp::splice(
                    nqp::getattr(self,List,'$!reified'),$new,$offset,$removed),
                  $result
                )
              )
            )
          ),
          self.throw-iterator-cannot-be-lazy('splice in')
        )
    }

    multi method tail(Array:D: $n) {
        nqp::if(
          nqp::isconcrete(nqp::getattr(self,List,'$!todo')),
          self.Any::tail($n),
          Seq.new(
            nqp::if(
              nqp::isconcrete(
                my $reified := nqp::getattr(self,List,'$!reified')
              ) && nqp::elems($reified),
              nqp::stmts(
                (my $iterator := Rakudo::Iterator.ReifiedArray(
                  self,
                  nqp::getattr(self,Array,'$!descriptor')
                )),
                nqp::if(
                  nqp::istype($n,Callable)
                    && nqp::isgt_i((my $skip := -($n(0).Int)),0),
                  $iterator.skip-at-least($skip),
                  nqp::unless(
                    nqp::istype($n,Whatever) || $n == Inf,
                    $iterator.skip-at-least(nqp::elems($reified) - $n)
                  )
                ),
                $iterator
              ),
              Rakudo::Iterator.Empty
            )
          )
        )
    }

    proto method grab(|) {*}
    multi method grab(Array:D:) {
        self.is-lazy
          ?? self.throw-iterator-cannot-be-lazy('grab from')  # can't make List
          !! self.elems  # reifies
            ?? self.GRAB_ONE
            !! Nil
    }
    multi method grab(Array:D: Callable:D $calculate) {
        self.grab($calculate(self.elems))
    }
    multi method grab(Array:D: Whatever --> Seq:D) {
        self.grab(Inf)
    }

    my class GrabN does Iterator {
        has $!array;
        has int $!count;

        method !SET-SELF(\array,\count) {
            my int $elems = nqp::elems(nqp::getattr(array,List,'$!reified'));
            $!array := array;

            nqp::if(
              count == Inf,
              ($!count = $elems),
              nqp::if(
                nqp::isgt_i(($!count = count.Int),$elems),
                ($!count = $elems)
              )
            );
            self

        }
        method new(\a,\c) { nqp::create(self)!SET-SELF(a,c) }
        method pull-one() {
            nqp::if(
              $!count && nqp::elems(nqp::getattr($!array,List,'$!reified')),
              nqp::stmts(
                ($!count = nqp::sub_i($!count,1)),
                $!array.GRAB_ONE
              ),
              IterationEnd
            )
        }
        method is-deterministic(--> False) { }
    }
    multi method grab(Array:D: \count --> Seq:D) {
        Seq.new(
          self.elems                         # reifies
          ?? GrabN.new(self,count)
          !! Rakudo::Iterator.Empty
        )
    }

    method GRAB_ONE(Array:D:) is implementation-detail {
        my $reified := nqp::getattr(self,List,'$!reified');
        my $value := nqp::atpos(
          $reified,
          (my int $pos = nqp::floor_n(nqp::rand_n(nqp::elems($reified)))),
        );
        nqp::splice($reified,$empty,$pos,1);
        $value
    }

    # introspection
    method name() {
        nqp::isnull($!descriptor) ?? Nil !! $!descriptor.name
    }

    proto method of() {*}
    multi method of(Array:U:) { Mu }
    multi method of(Array:D:) {
        nqp::isnull($!descriptor) ?? Mu !! $!descriptor.of
    }

    method default() {
        nqp::isnull($!descriptor) ?? Any !! $!descriptor.default
    }
    method dynamic() {
        nqp::isnull($!descriptor) ?? False !! so $!descriptor.dynamic
    }
    multi method raku(Array:D \SELF: --> Str:D) {
        SELF.rakuseen('Array', {
             '$' x nqp::iscont(SELF)  # self is always deconted
             ~ '['
             ~ self.map({nqp::decont($_).raku}).join(', ')
             ~ ',' x (self.elems == 1 && nqp::istype(self.AT-POS(0),Iterable))
             ~ ']'
        })
    }
    multi method WHICH(Array:D: --> ObjAt:D) { self.Mu::WHICH }

    my constant \dim2role =
      nqp::list(Array::Shaped,Array::Shaped1,Array::Shaped2,Array::Shaped3);

    proto method set-shape(|) is implementation-detail {*}
    multi method set-shape(Whatever) is raw {
        nqp::create(self.WHAT)
    }
    multi method set-shape(\shape) is raw {
        self.set-shape(shape.List)
    }
    multi method set-shape(List:D \shape) is raw {
        my int $dims = shape.elems;  # reifies
        my $reified := nqp::getattr(nqp::decont(shape),List,'$!reified');

        # just a list with Whatever, so no shape
        if nqp::iseq_i($dims,1)
          && nqp::istype(nqp::atpos($reified,0),Whatever) {
            nqp::create(self.WHAT)
        }

        # we haz dimensions
        elsif $dims {
            my $what := self.WHAT.^mixin(
              nqp::atpos(dim2role,nqp::isle_i($dims,3) && $dims)
            );
            $what.^set_name(self.^name)           # correct name if needed
              if nqp::isne_s($what.^name,self.^name);

            my $array := nqp::p6bindattrinvres(
              nqp::create($what),List,'$!reified',
              Rakudo::Internals.SHAPED-ARRAY-STORAGE(shape,nqp::knowhow,Mu)
            );
            nqp::p6bindattrinvres($array,$what,'$!shape',nqp::decont(shape))
        }

        # flatland
        else {
            X::NotEnoughDimensions.new(
              operation         => 'create',
              got-dimensions    => 0,
              needed-dimensions => '',
            ).throw
        }
    }

    method ^parameterize(Mu:U \arr, Mu \of) {
        if nqp::isconcrete(of) {
            die "Can not parameterize {arr.^name} with {of.raku}"
        }
        else {
            my $what := arr.^mixin(Array::Typed[of]);
            # needs to be done in COMPOSE phaser when that works
            $what.^set_name("{arr.^name}[{of.^name}]");
            $what
        }
    }
}

# vim: expandtab shiftwidth=4
