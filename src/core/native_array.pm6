my class X::MustBeParametric  { ... }
my class X::TooManyDimensions { ... }
my class X::TypeCheck::Assignment { ... }

my class array does Iterable {

    multi method new(array:)      { self!create }
    multi method new(array: @v)   { self!create.STORE(@v) }
    multi method new(array: **@v) { self!create.STORE(@v) }

    multi method new(array: :$shape!)       { self!create-ws($shape) }
    multi method new(array: @v, :$shape!)   { self!create-ws($shape).STORE(@v) }
    multi method new(array: **@v, :$shape!) { self!create-ws($shape).STORE(@v) }

    method !create() {
        nqp::isnull(nqp::typeparameterized(self))
         ?? X::MustBeParametric.new(:type(self)).throw
         !! nqp::create(self)
    }
    method !create-ws($shape) {
        nqp::isnull(nqp::typeparameterized(self))
          ?? X::MustBeParametric.new(:type(self)).throw
          !! nqp::isconcrete($shape)
            ?? self!shaped($shape)
            !! Metamodel::EnumHOW.ACCEPTS($shape.HOW)
              ?? self!shaped($shape.^elems)
              !! nqp::create(self)
    }

    proto method STORE(|) {*}
    multi method STORE(array:D: *@values) { self.STORE(@values) }

    multi method push(array:D:    **@values) { self.append(@values) }
    multi method append(array:D:   *@values) { self.append(@values) }
    multi method unshift(array:D: **@values) { self.unshift(@values) }
    multi method prepend(array:D:  *@values) { self.unshift(@values) }

    sub EQV_DIMENSIONS(Mu \one, Mu \two) is raw {
        nqp::iseq_i(     # much faster than one.shape eqv two.shape
          (my int $dims = nqp::elems(
            my $onedims := nqp::dimensions(one)
          )),
          nqp::elems(my $twodims := nqp::dimensions(two))
        ) && nqp::stmts(
          (my int $i = -1),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$dims)
              && nqp::iseq_i(
                   nqp::atpos_i($onedims,$i),
                   nqp::atpos_i($twodims,$i)
            ),
            nqp::null
          ),
          nqp::iseq_i($i,$dims)
        )
    }

    sub CLONE_SLICE(\array, int $offset, int $size) {
        nqp::if(
          nqp::islt_i($offset,0)
            || nqp::isgt_i($offset,(my int $elems = nqp::elems(array))),
          Failure.new(X::OutOfRange.new(
            :what('Offset argument to splice'),
            :got($offset),
            :range("0..{nqp::elems(array)}")
          )),
          nqp::if(
            nqp::islt_i($size,0),
            Failure.new(X::OutOfRange.new(
              :what('Size argument to splice'),
              :got($size),
              :range("0..^{$elems - $offset}")
            )),
            nqp::if(
              nqp::iseq_i($offset,$elems) || nqp::iseq_i($size,0),
              nqp::create(array),
              nqp::if(
                nqp::isge_i(
                  (my int $end = nqp::sub_i(nqp::add_i($offset,$size),1)),
                  $elems
                ),
                nqp::slice(array,$offset,-1),
                nqp::slice(array,$offset,$end)
              )
            )
          )
        )
    }

    my role strarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of strarray role -----------------------------------
#- Generated on 2018-05-09T14:49:37+02:00 by tools/build/makeNATIVE_ARRAY.pl6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method AT-POS(strarray:D: int $idx) is raw {
            nqp::atposref_s(self, $idx)
        }
        multi method AT-POS(strarray:D: Int:D $idx) is raw {
            nqp::atposref_s(self, $idx)
        }

        multi method ASSIGN-POS(strarray:D: int $idx, str $value) {
            nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: Int:D $idx, str $value) {
            nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: int $idx, Str:D $value) {
            nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: Int:D $idx, Str:D $value) {
            nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: Any $idx, Mu \value) {
            X::TypeCheck.new(
                operation => "assignment to str array element #$idx",
                got       => value,
                expected  => T,
            ).throw;
        }

        multi method STORE(strarray:D: $value) {
            nqp::setelems(self,1);
            nqp::bindpos_s(self, 0, nqp::unbox_s($value));
            self
        }
        multi method STORE(strarray:D: strarray:D \values) {
            nqp::setelems(self,nqp::elems(values));
            nqp::splice(self,values,0,nqp::elems(values))
        }
        multi method STORE(strarray:D: Seq:D \seq) {
            nqp::if(
              (my $iterator := seq.iterator).is-lazy,
              Failure.new(X::Cannot::Lazy.new(
                :action<store>, :what(self.^name)
              )),
              nqp::stmts(
                $iterator.push-all(self),
                self
              )
            )
        }
        multi method STORE(strarray:D: List:D \values) {
            my int $elems = values.elems;    # reifies
            my $reified := nqp::getattr(values,List,'$!reified');
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_s(self, $i,
                nqp::unbox_s(nqp::atpos($reified,$i)))
            );
            self
        }
        multi method STORE(strarray:D: @values) {
            my int $elems = @values.elems;   # reifies
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_s(self, $i,
                nqp::unbox_s(@values.AT-POS($i)))
            );
            self
        }

        multi method push(strarray:D: str $value) {
            nqp::push_s(self, $value);
            self
        }
        multi method push(strarray:D: Str:D $value) {
            nqp::push_s(self, $value);
            self
        }
        multi method push(strarray:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'push to str array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method append(strarray:D: str $value) {
            nqp::push_s(self, $value);
            self
        }
        multi method append(strarray:D: Str:D $value) {
            nqp::push_s(self, $value);
            self
        }
        multi method append(strarray:D: strarray:D $values) is default {
            nqp::splice(self,$values,nqp::elems(self),0)
        }
        multi method append(strarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_s(self, $_) for flat @values;
            self
        }

        method pop(strarray:D: --> str) {
            nqp::elems(self)
              ?? nqp::pop_s(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift(strarray:D: --> str) {
            nqp::elems(self)
              ?? nqp::shift_s(self)
              !! die X::Cannot::Empty.new(:action<shift>, :what(self.^name));
        }

        multi method unshift(strarray:D: str $value) {
            nqp::unshift_s(self, $value);
            self
        }
        multi method unshift(strarray:D: Str:D $value) {
            nqp::unshift_s(self, $value);
            self
        }
        multi method unshift(strarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_s(self, @values.pop) while @values;
            self
        }
        multi method unshift(strarray:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'unshift to str array',
                got       => value,
                expected  => T,
            ).throw;
        }

        my $empty_s := nqp::list_s;

        multi method splice(strarray:D:) {
            my $splice := nqp::clone(self);
            nqp::setelems(self,0);
            $splice
        }
        multi method splice(strarray:D: Int:D \offset) {
            nqp::if(
              nqp::islt_i((my int $offset = offset),0)
                || nqp::isgt_i($offset,(my int $elems = nqp::elems(self))),
              Failure.new(X::OutOfRange.new(
                :what('Offset argument to splice'),
                :got($offset),
                :range("0..{nqp::elems(array)}")
              )),
              nqp::if(
                nqp::iseq_i($offset,nqp::elems(self)),
                nqp::create(self.WHAT),
                nqp::stmts(
                  (my $slice := nqp::slice(self,$offset,-1)),
                  nqp::splice(
                    self,
                    $empty_s,
                    $offset,
                    nqp::sub_i(nqp::elems(self),$offset)
                  ),
                  $slice
                )
              )
            )
        }
        multi method splice(strarray:D: Int:D $offset, Int:D $size) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(self,$empty_s,$offset,$size)
            );
            $slice
        }
        multi method splice(strarray:D: Int:D $offset, Int:D $size, strarray:D \values) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(
                self,
                nqp::if(nqp::eqaddr(self,values),nqp::clone(values),values),
                $offset,
                $size
              )
            );
            $slice
        }
        multi method splice(strarray:D: Int:D $offset, Int:D $size, Seq:D \seq) {
            nqp::if(
              seq.is-lazy,
              Failure.new(X::Cannot::Lazy.new(
                :action<splice>, :what(self.^name)
              )),
              nqp::stmts(
                nqp::unless(
                  nqp::istype(
                    (my $slice := CLONE_SLICE(self,$offset,$size)),
                    Failure
                  ),
                  nqp::splice(self,nqp::create(self).STORE(seq),$offset,$size)
                ),
                $slice
              )
            )
        }
        multi method splice(strarray:D: $offset=0, $size=Whatever, *@values) {
            fail X::Cannot::Lazy.new(:action('splice in'))
              if @values.is-lazy;

            my int $elems = nqp::elems(self);
            my int $o = nqp::istype($offset,Callable)
              ?? $offset($elems)
              !! nqp::istype($offset,Whatever)
                ?? $elems
                !! $offset.Int;
            my int $s = nqp::istype($size,Callable)
              ?? $size($elems - $o)
              !! !defined($size) || nqp::istype($size,Whatever)
                 ?? $elems - ($o min $elems)
                 !! $size.Int;

            unless nqp::istype(
              (my $splice := CLONE_SLICE(self,$o,$s)),
              Failure
            ) {
                my $splicees := nqp::create(self);
                nqp::push_s($splicees, @values.shift) while @values;
                nqp::splice(self,$splicees,$o,$s);
            }
            $splice
        }

        multi method min(strarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my str $min = nqp::atpos_s(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_s(nqp::atpos_s(self,$i),$min),
                    ($min = nqp::atpos_s(self,$i))
                  )
                ),
                $min
              ),
              Inf
            )
        }
        multi method max(strarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my str $max = nqp::atpos_s(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::isgt_s(nqp::atpos_s(self,$i),$max),
                    ($max = nqp::atpos_s(self,$i))
                  )
                ),
                $max
              ),
              -Inf
            )
        }
        multi method minmax(strarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my str $min =
                  my str $max = nqp::atpos_s(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_s(nqp::atpos_s(self,$i),$min),
                    ($min = nqp::atpos_s(self,$i)),
                    nqp::if(
                      nqp::isgt_s(nqp::atpos_s(self,$i),$max),
                      ($max = nqp::atpos_s(self,$i))
                    )
                  )
                ),
                Range.new($min,$max)
              ),
              Range.new(Inf,-Inf)
            )
        }

        method iterator(strarray:D:) {
            class :: does Iterator {
                has int $!i;
                has $!array;    # Native array we're iterating

                method SET-SELF(\array) {
                    $!array := nqp::decont(array);
                    $!i = -1;
                    self
                }
                method new(\array) { nqp::create(self).SET-SELF(array) }

                method pull-one() is raw {
                    ($!i = $!i + 1) < nqp::elems($!array)
                      ?? nqp::atposref_s($!array,$!i)
                      !! IterationEnd
                }
                method skip-one() {
                    ($!i = $!i + 1) < nqp::elems($!array)
                }
                method skip-at-least(int $toskip) {
                    nqp::unless(
                      ($!i = $!i + $toskip) < nqp::elems($!array),
                      nqp::stmts(
                        ($!i = nqp::elems($!array)),
                        0
                      )
                    )
                }
                method push-all($target --> IterationEnd) {
                    my int $i     = $!i;
                    my int $elems = nqp::elems($!array);
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      $target.push(nqp::atposref_s($!array,$i))
                    );
                    $!i = $i;
                }
            }.new(self)
        }
        method reverse(strarray:D:) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my int $last  = nqp::sub_i($elems,1)),
              (my int $i     = -1),
              (my $to := nqp::clone(self)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_s($to,nqp::sub_i($last,$i),
                  nqp::atpos_s(self,$i))
              ),
              $to
            )
        }
        method rotate(strarray:D: Int(Cool) $rotate = 1) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my $to := nqp::clone(self)),
              (my int $i = -1),
              (my int $j =
                nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),$rotate),$elems)),
              nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_s(
                  $to,
                  ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
                  nqp::atpos_s(self,$i)
                ),
              ),
              $to
            )
        }
        multi method sort(strarray:D:) {
            Rakudo::Sorting.MERGESORT-str(nqp::clone(self))
        }
        proto method grab(|) {*}
        multi method grab(strarray:D:) {
            nqp::if(nqp::elems(self),self.GRAB_ONE,Nil)
        }
        multi method grab(strarray:D: Callable:D $calculate) {
            self.grab($calculate(nqp::elems(self)))
        }
        multi method grab(strarray:D: Whatever) { self.grab(Inf) }
        multi method grab(strarray:D: $count) {
            Seq.new(nqp::if(
              nqp::elems(self),
              class :: does Iterator {
                  has $!array;
                  has int $!count;

                  method SET-SELF(\array,\count) {
                      nqp::stmts(
                        (my int $elems = nqp::elems(array)),
                        ($!array := array),
                        nqp::if(
                          count == Inf,
                          ($!count = $elems),
                          nqp::if(
                            nqp::isgt_i(($!count = count.Int),$elems),
                            ($!count = $elems)
                          )
                        ),
                        self
                      )

                  }
                  method new(\a,\c) { nqp::create(self).SET-SELF(a,c) }
                  method pull-one() {
                      nqp::if(
                        $!count && nqp::elems($!array),
                        nqp::stmts(
                          ($!count = nqp::sub_i($!count,1)),
                          $!array.GRAB_ONE
                        ),
                        IterationEnd
                      )
                  }
              }.new(self,$count),
              Rakudo::Iterator.Empty
            ))
        }

        method GRAB_ONE(strarray:D:) {
            nqp::stmts(
              (my $value := nqp::atpos_s(
                self,
                (my int $pos = nqp::floor_n(nqp::rand_n(nqp::elems(self))))
              )),
              nqp::splice(self,$empty_s,$pos,1),
              $value
            )
        }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of strarray role -------------------------------------

        method join(strarray:D: $delim = '') {

            my str $empty = "";
            my int $elems = nqp::elems(self);
            my int $i     = -1;
            nqp::bindpos_s(self,$i,$empty)
              if nqp::isnull_s(nqp::atposref_s(self,$i))
              while nqp::islt_i(++$i,$elems);

            nqp::join($delim.Str,self)
        }
    }

    my role intarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of intarray role -----------------------------------
#- Generated on 2018-05-09T14:49:37+02:00 by tools/build/makeNATIVE_ARRAY.pl6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method AT-POS(intarray:D: int $idx) is raw {
            nqp::atposref_i(self, $idx)
        }
        multi method AT-POS(intarray:D: Int:D $idx) is raw {
            nqp::atposref_i(self, $idx)
        }

        multi method ASSIGN-POS(intarray:D: int $idx, int $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: Int:D $idx, int $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: int $idx, Int:D $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: Int:D $idx, Int:D $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: Any $idx, Mu \value) {
            X::TypeCheck.new(
                operation => "assignment to int array element #$idx",
                got       => value,
                expected  => T,
            ).throw;
        }

        multi method STORE(intarray:D: $value) {
            nqp::setelems(self,1);
            nqp::bindpos_i(self, 0, nqp::unbox_i($value));
            self
        }
        multi method STORE(intarray:D: intarray:D \values) {
            nqp::setelems(self,nqp::elems(values));
            nqp::splice(self,values,0,nqp::elems(values))
        }
        multi method STORE(intarray:D: Seq:D \seq) {
            nqp::if(
              (my $iterator := seq.iterator).is-lazy,
              Failure.new(X::Cannot::Lazy.new(
                :action<store>, :what(self.^name)
              )),
              nqp::stmts(
                $iterator.push-all(self),
                self
              )
            )
        }
        multi method STORE(intarray:D: List:D \values) {
            my int $elems = values.elems;    # reifies
            my $reified := nqp::getattr(values,List,'$!reified');
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_i(self, $i,
                nqp::unbox_i(nqp::atpos($reified,$i)))
            );
            self
        }
        multi method STORE(intarray:D: @values) {
            my int $elems = @values.elems;   # reifies
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_i(self, $i,
                nqp::unbox_i(@values.AT-POS($i)))
            );
            self
        }

        multi method push(intarray:D: int $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method push(intarray:D: Int:D $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method push(intarray:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'push to int array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method append(intarray:D: int $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method append(intarray:D: Int:D $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method append(intarray:D: intarray:D $values) is default {
            nqp::splice(self,$values,nqp::elems(self),0)
        }
        multi method append(intarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_i(self, $_) for flat @values;
            self
        }

        method pop(intarray:D: --> int) {
            nqp::elems(self)
              ?? nqp::pop_i(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift(intarray:D: --> int) {
            nqp::elems(self)
              ?? nqp::shift_i(self)
              !! die X::Cannot::Empty.new(:action<shift>, :what(self.^name));
        }

        multi method unshift(intarray:D: int $value) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(intarray:D: Int:D $value) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(intarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_i(self, @values.pop) while @values;
            self
        }
        multi method unshift(intarray:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'unshift to int array',
                got       => value,
                expected  => T,
            ).throw;
        }

        my $empty_i := nqp::list_i;

        multi method splice(intarray:D:) {
            my $splice := nqp::clone(self);
            nqp::setelems(self,0);
            $splice
        }
        multi method splice(intarray:D: Int:D \offset) {
            nqp::if(
              nqp::islt_i((my int $offset = offset),0)
                || nqp::isgt_i($offset,(my int $elems = nqp::elems(self))),
              Failure.new(X::OutOfRange.new(
                :what('Offset argument to splice'),
                :got($offset),
                :range("0..{nqp::elems(array)}")
              )),
              nqp::if(
                nqp::iseq_i($offset,nqp::elems(self)),
                nqp::create(self.WHAT),
                nqp::stmts(
                  (my $slice := nqp::slice(self,$offset,-1)),
                  nqp::splice(
                    self,
                    $empty_i,
                    $offset,
                    nqp::sub_i(nqp::elems(self),$offset)
                  ),
                  $slice
                )
              )
            )
        }
        multi method splice(intarray:D: Int:D $offset, Int:D $size) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(self,$empty_i,$offset,$size)
            );
            $slice
        }
        multi method splice(intarray:D: Int:D $offset, Int:D $size, intarray:D \values) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(
                self,
                nqp::if(nqp::eqaddr(self,values),nqp::clone(values),values),
                $offset,
                $size
              )
            );
            $slice
        }
        multi method splice(intarray:D: Int:D $offset, Int:D $size, Seq:D \seq) {
            nqp::if(
              seq.is-lazy,
              Failure.new(X::Cannot::Lazy.new(
                :action<splice>, :what(self.^name)
              )),
              nqp::stmts(
                nqp::unless(
                  nqp::istype(
                    (my $slice := CLONE_SLICE(self,$offset,$size)),
                    Failure
                  ),
                  nqp::splice(self,nqp::create(self).STORE(seq),$offset,$size)
                ),
                $slice
              )
            )
        }
        multi method splice(intarray:D: $offset=0, $size=Whatever, *@values) {
            fail X::Cannot::Lazy.new(:action('splice in'))
              if @values.is-lazy;

            my int $elems = nqp::elems(self);
            my int $o = nqp::istype($offset,Callable)
              ?? $offset($elems)
              !! nqp::istype($offset,Whatever)
                ?? $elems
                !! $offset.Int;
            my int $s = nqp::istype($size,Callable)
              ?? $size($elems - $o)
              !! !defined($size) || nqp::istype($size,Whatever)
                 ?? $elems - ($o min $elems)
                 !! $size.Int;

            unless nqp::istype(
              (my $splice := CLONE_SLICE(self,$o,$s)),
              Failure
            ) {
                my $splicees := nqp::create(self);
                nqp::push_i($splicees, @values.shift) while @values;
                nqp::splice(self,$splicees,$o,$s);
            }
            $splice
        }

        multi method min(intarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my int $min = nqp::atpos_i(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::atpos_i(self,$i),$min),
                    ($min = nqp::atpos_i(self,$i))
                  )
                ),
                $min
              ),
              Inf
            )
        }
        multi method max(intarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my int $max = nqp::atpos_i(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::isgt_i(nqp::atpos_i(self,$i),$max),
                    ($max = nqp::atpos_i(self,$i))
                  )
                ),
                $max
              ),
              -Inf
            )
        }
        multi method minmax(intarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my int $min =
                  my int $max = nqp::atpos_i(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::atpos_i(self,$i),$min),
                    ($min = nqp::atpos_i(self,$i)),
                    nqp::if(
                      nqp::isgt_i(nqp::atpos_i(self,$i),$max),
                      ($max = nqp::atpos_i(self,$i))
                    )
                  )
                ),
                Range.new($min,$max)
              ),
              Range.new(Inf,-Inf)
            )
        }

        method iterator(intarray:D:) {
            class :: does Iterator {
                has int $!i;
                has $!array;    # Native array we're iterating

                method SET-SELF(\array) {
                    $!array := nqp::decont(array);
                    $!i = -1;
                    self
                }
                method new(\array) { nqp::create(self).SET-SELF(array) }

                method pull-one() is raw {
                    ($!i = $!i + 1) < nqp::elems($!array)
                      ?? nqp::atposref_i($!array,$!i)
                      !! IterationEnd
                }
                method skip-one() {
                    ($!i = $!i + 1) < nqp::elems($!array)
                }
                method skip-at-least(int $toskip) {
                    nqp::unless(
                      ($!i = $!i + $toskip) < nqp::elems($!array),
                      nqp::stmts(
                        ($!i = nqp::elems($!array)),
                        0
                      )
                    )
                }
                method push-all($target --> IterationEnd) {
                    my int $i     = $!i;
                    my int $elems = nqp::elems($!array);
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      $target.push(nqp::atposref_i($!array,$i))
                    );
                    $!i = $i;
                }
            }.new(self)
        }
        method reverse(intarray:D:) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my int $last  = nqp::sub_i($elems,1)),
              (my int $i     = -1),
              (my $to := nqp::clone(self)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_i($to,nqp::sub_i($last,$i),
                  nqp::atpos_i(self,$i))
              ),
              $to
            )
        }
        method rotate(intarray:D: Int(Cool) $rotate = 1) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my $to := nqp::clone(self)),
              (my int $i = -1),
              (my int $j =
                nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),$rotate),$elems)),
              nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_i(
                  $to,
                  ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
                  nqp::atpos_i(self,$i)
                ),
              ),
              $to
            )
        }
        multi method sort(intarray:D:) {
            Rakudo::Sorting.MERGESORT-int(nqp::clone(self))
        }
        proto method grab(|) {*}
        multi method grab(intarray:D:) {
            nqp::if(nqp::elems(self),self.GRAB_ONE,Nil)
        }
        multi method grab(intarray:D: Callable:D $calculate) {
            self.grab($calculate(nqp::elems(self)))
        }
        multi method grab(intarray:D: Whatever) { self.grab(Inf) }
        multi method grab(intarray:D: $count) {
            Seq.new(nqp::if(
              nqp::elems(self),
              class :: does Iterator {
                  has $!array;
                  has int $!count;

                  method SET-SELF(\array,\count) {
                      nqp::stmts(
                        (my int $elems = nqp::elems(array)),
                        ($!array := array),
                        nqp::if(
                          count == Inf,
                          ($!count = $elems),
                          nqp::if(
                            nqp::isgt_i(($!count = count.Int),$elems),
                            ($!count = $elems)
                          )
                        ),
                        self
                      )

                  }
                  method new(\a,\c) { nqp::create(self).SET-SELF(a,c) }
                  method pull-one() {
                      nqp::if(
                        $!count && nqp::elems($!array),
                        nqp::stmts(
                          ($!count = nqp::sub_i($!count,1)),
                          $!array.GRAB_ONE
                        ),
                        IterationEnd
                      )
                  }
              }.new(self,$count),
              Rakudo::Iterator.Empty
            ))
        }

        method GRAB_ONE(intarray:D:) {
            nqp::stmts(
              (my $value := nqp::atpos_i(
                self,
                (my int $pos = nqp::floor_n(nqp::rand_n(nqp::elems(self))))
              )),
              nqp::splice(self,$empty_i,$pos,1),
              $value
            )
        }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of intarray role -------------------------------------

        method join(intarray:D: $delim = '') {
            my int $elems = nqp::elems(self);
            my $list     := nqp::setelems(nqp::list_s,$elems);
            my int $i     = -1;

            nqp::bindpos_s($list,$i,
              nqp::tostr_I(nqp::p6box_i(nqp::atpos_i(self,$i))))
              while nqp::islt_i(++$i,$elems);

            nqp::join($delim.Str,$list)
        }

        multi method STORE(intarray:D: Range:D \range) {
            nqp::if(
              range.is-int,
              nqp::stmts(
                (my int $val = nqp::add_i(
                  nqp::getattr(range,Range,'$!min'),
                  nqp::getattr_i(range,Range,'$!excludes-min')
                )),
                (my int $max = nqp::sub_i(
                  nqp::getattr(range,Range,'$!max'),
                  nqp::getattr_i(range,Range,'$!excludes-max')
                )),
                nqp::setelems(self, nqp::add_i(nqp::sub_i($max,$val),1)),
                (my int $i = -1),
                ($val = nqp::sub_i($val,1)),
                nqp::while(
                  nqp::isle_i(($val = nqp::add_i($val,1)),$max),
                  nqp::bindpos_i(self,($i = nqp::add_i($i,1)),$val)
                ),
                self
              ),
              (die "Can only initialize an int array with an int Range")
            )
        }
    }

    my role numarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of numarray role -----------------------------------
#- Generated on 2018-05-09T14:49:37+02:00 by tools/build/makeNATIVE_ARRAY.pl6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method AT-POS(numarray:D: int $idx) is raw {
            nqp::atposref_n(self, $idx)
        }
        multi method AT-POS(numarray:D: Int:D $idx) is raw {
            nqp::atposref_n(self, $idx)
        }

        multi method ASSIGN-POS(numarray:D: int $idx, num $value) {
            nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: Int:D $idx, num $value) {
            nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: int $idx, Num:D $value) {
            nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: Int:D $idx, Num:D $value) {
            nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: Any $idx, Mu \value) {
            X::TypeCheck.new(
                operation => "assignment to num array element #$idx",
                got       => value,
                expected  => T,
            ).throw;
        }

        multi method STORE(numarray:D: $value) {
            nqp::setelems(self,1);
            nqp::bindpos_n(self, 0, nqp::unbox_n($value));
            self
        }
        multi method STORE(numarray:D: numarray:D \values) {
            nqp::setelems(self,nqp::elems(values));
            nqp::splice(self,values,0,nqp::elems(values))
        }
        multi method STORE(numarray:D: Seq:D \seq) {
            nqp::if(
              (my $iterator := seq.iterator).is-lazy,
              Failure.new(X::Cannot::Lazy.new(
                :action<store>, :what(self.^name)
              )),
              nqp::stmts(
                $iterator.push-all(self),
                self
              )
            )
        }
        multi method STORE(numarray:D: List:D \values) {
            my int $elems = values.elems;    # reifies
            my $reified := nqp::getattr(values,List,'$!reified');
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_n(self, $i,
                nqp::unbox_n(nqp::atpos($reified,$i)))
            );
            self
        }
        multi method STORE(numarray:D: @values) {
            my int $elems = @values.elems;   # reifies
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_n(self, $i,
                nqp::unbox_n(@values.AT-POS($i)))
            );
            self
        }

        multi method push(numarray:D: num $value) {
            nqp::push_n(self, $value);
            self
        }
        multi method push(numarray:D: Num:D $value) {
            nqp::push_n(self, $value);
            self
        }
        multi method push(numarray:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'push to num array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method append(numarray:D: num $value) {
            nqp::push_n(self, $value);
            self
        }
        multi method append(numarray:D: Num:D $value) {
            nqp::push_n(self, $value);
            self
        }
        multi method append(numarray:D: numarray:D $values) is default {
            nqp::splice(self,$values,nqp::elems(self),0)
        }
        multi method append(numarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_n(self, $_) for flat @values;
            self
        }

        method pop(numarray:D: --> num) {
            nqp::elems(self)
              ?? nqp::pop_n(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift(numarray:D: --> num) {
            nqp::elems(self)
              ?? nqp::shift_n(self)
              !! die X::Cannot::Empty.new(:action<shift>, :what(self.^name));
        }

        multi method unshift(numarray:D: num $value) {
            nqp::unshift_n(self, $value);
            self
        }
        multi method unshift(numarray:D: Num:D $value) {
            nqp::unshift_n(self, $value);
            self
        }
        multi method unshift(numarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_n(self, @values.pop) while @values;
            self
        }
        multi method unshift(numarray:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'unshift to num array',
                got       => value,
                expected  => T,
            ).throw;
        }

        my $empty_n := nqp::list_n;

        multi method splice(numarray:D:) {
            my $splice := nqp::clone(self);
            nqp::setelems(self,0);
            $splice
        }
        multi method splice(numarray:D: Int:D \offset) {
            nqp::if(
              nqp::islt_i((my int $offset = offset),0)
                || nqp::isgt_i($offset,(my int $elems = nqp::elems(self))),
              Failure.new(X::OutOfRange.new(
                :what('Offset argument to splice'),
                :got($offset),
                :range("0..{nqp::elems(array)}")
              )),
              nqp::if(
                nqp::iseq_i($offset,nqp::elems(self)),
                nqp::create(self.WHAT),
                nqp::stmts(
                  (my $slice := nqp::slice(self,$offset,-1)),
                  nqp::splice(
                    self,
                    $empty_n,
                    $offset,
                    nqp::sub_i(nqp::elems(self),$offset)
                  ),
                  $slice
                )
              )
            )
        }
        multi method splice(numarray:D: Int:D $offset, Int:D $size) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(self,$empty_n,$offset,$size)
            );
            $slice
        }
        multi method splice(numarray:D: Int:D $offset, Int:D $size, numarray:D \values) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(
                self,
                nqp::if(nqp::eqaddr(self,values),nqp::clone(values),values),
                $offset,
                $size
              )
            );
            $slice
        }
        multi method splice(numarray:D: Int:D $offset, Int:D $size, Seq:D \seq) {
            nqp::if(
              seq.is-lazy,
              Failure.new(X::Cannot::Lazy.new(
                :action<splice>, :what(self.^name)
              )),
              nqp::stmts(
                nqp::unless(
                  nqp::istype(
                    (my $slice := CLONE_SLICE(self,$offset,$size)),
                    Failure
                  ),
                  nqp::splice(self,nqp::create(self).STORE(seq),$offset,$size)
                ),
                $slice
              )
            )
        }
        multi method splice(numarray:D: $offset=0, $size=Whatever, *@values) {
            fail X::Cannot::Lazy.new(:action('splice in'))
              if @values.is-lazy;

            my int $elems = nqp::elems(self);
            my int $o = nqp::istype($offset,Callable)
              ?? $offset($elems)
              !! nqp::istype($offset,Whatever)
                ?? $elems
                !! $offset.Int;
            my int $s = nqp::istype($size,Callable)
              ?? $size($elems - $o)
              !! !defined($size) || nqp::istype($size,Whatever)
                 ?? $elems - ($o min $elems)
                 !! $size.Int;

            unless nqp::istype(
              (my $splice := CLONE_SLICE(self,$o,$s)),
              Failure
            ) {
                my $splicees := nqp::create(self);
                nqp::push_n($splicees, @values.shift) while @values;
                nqp::splice(self,$splicees,$o,$s);
            }
            $splice
        }

        multi method min(numarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my num $min = nqp::atpos_n(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_n(nqp::atpos_n(self,$i),$min),
                    ($min = nqp::atpos_n(self,$i))
                  )
                ),
                $min
              ),
              Inf
            )
        }
        multi method max(numarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my num $max = nqp::atpos_n(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::isgt_n(nqp::atpos_n(self,$i),$max),
                    ($max = nqp::atpos_n(self,$i))
                  )
                ),
                $max
              ),
              -Inf
            )
        }
        multi method minmax(numarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                (my num $min =
                  my num $max = nqp::atpos_n(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_n(nqp::atpos_n(self,$i),$min),
                    ($min = nqp::atpos_n(self,$i)),
                    nqp::if(
                      nqp::isgt_n(nqp::atpos_n(self,$i),$max),
                      ($max = nqp::atpos_n(self,$i))
                    )
                  )
                ),
                Range.new($min,$max)
              ),
              Range.new(Inf,-Inf)
            )
        }

        method iterator(numarray:D:) {
            class :: does Iterator {
                has int $!i;
                has $!array;    # Native array we're iterating

                method SET-SELF(\array) {
                    $!array := nqp::decont(array);
                    $!i = -1;
                    self
                }
                method new(\array) { nqp::create(self).SET-SELF(array) }

                method pull-one() is raw {
                    ($!i = $!i + 1) < nqp::elems($!array)
                      ?? nqp::atposref_n($!array,$!i)
                      !! IterationEnd
                }
                method skip-one() {
                    ($!i = $!i + 1) < nqp::elems($!array)
                }
                method skip-at-least(int $toskip) {
                    nqp::unless(
                      ($!i = $!i + $toskip) < nqp::elems($!array),
                      nqp::stmts(
                        ($!i = nqp::elems($!array)),
                        0
                      )
                    )
                }
                method push-all($target --> IterationEnd) {
                    my int $i     = $!i;
                    my int $elems = nqp::elems($!array);
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      $target.push(nqp::atposref_n($!array,$i))
                    );
                    $!i = $i;
                }
            }.new(self)
        }
        method reverse(numarray:D:) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my int $last  = nqp::sub_i($elems,1)),
              (my int $i     = -1),
              (my $to := nqp::clone(self)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_n($to,nqp::sub_i($last,$i),
                  nqp::atpos_n(self,$i))
              ),
              $to
            )
        }
        method rotate(numarray:D: Int(Cool) $rotate = 1) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my $to := nqp::clone(self)),
              (my int $i = -1),
              (my int $j =
                nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),$rotate),$elems)),
              nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_n(
                  $to,
                  ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
                  nqp::atpos_n(self,$i)
                ),
              ),
              $to
            )
        }
        multi method sort(numarray:D:) {
            Rakudo::Sorting.MERGESORT-num(nqp::clone(self))
        }
        proto method grab(|) {*}
        multi method grab(numarray:D:) {
            nqp::if(nqp::elems(self),self.GRAB_ONE,Nil)
        }
        multi method grab(numarray:D: Callable:D $calculate) {
            self.grab($calculate(nqp::elems(self)))
        }
        multi method grab(numarray:D: Whatever) { self.grab(Inf) }
        multi method grab(numarray:D: $count) {
            Seq.new(nqp::if(
              nqp::elems(self),
              class :: does Iterator {
                  has $!array;
                  has int $!count;

                  method SET-SELF(\array,\count) {
                      nqp::stmts(
                        (my int $elems = nqp::elems(array)),
                        ($!array := array),
                        nqp::if(
                          count == Inf,
                          ($!count = $elems),
                          nqp::if(
                            nqp::isgt_i(($!count = count.Int),$elems),
                            ($!count = $elems)
                          )
                        ),
                        self
                      )

                  }
                  method new(\a,\c) { nqp::create(self).SET-SELF(a,c) }
                  method pull-one() {
                      nqp::if(
                        $!count && nqp::elems($!array),
                        nqp::stmts(
                          ($!count = nqp::sub_i($!count,1)),
                          $!array.GRAB_ONE
                        ),
                        IterationEnd
                      )
                  }
              }.new(self,$count),
              Rakudo::Iterator.Empty
            ))
        }

        method GRAB_ONE(numarray:D:) {
            nqp::stmts(
              (my $value := nqp::atpos_n(
                self,
                (my int $pos = nqp::floor_n(nqp::rand_n(nqp::elems(self))))
              )),
              nqp::splice(self,$empty_n,$pos,1),
              $value
            )
        }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of numarray role -------------------------------------

        method sum(numarray:D:) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my num $sum = nqp::atpos_n(self,0)),
                (my int $i),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  $sum = nqp::add_n($sum,nqp::atpos_n(self,$i))
                ),
                $sum
              ),
              0e0
            )
        }
        multi method STORE(numarray:D: Range:D $range) {
            my num $val = $range.min;
            $val = $val + 1 if $range.excludes-min;
            my num $max = $range.max;
            $max = $max - 1 if $range.excludes-max;
            fail X::Cannot::Lazy.new(:action<initialize>,:what(self.^name))
              if $val == -Inf || $max == Inf;

            nqp::setelems(self, ($max - $val + 1).Int );
            my int $i;
            while $val <= $max {
                nqp::bindpos_n(self, $i, $val);
                $val = $val + 1;
                $i   = $i   + 1;
            }
            self
        }
    }

    role shapedarray does Rakudo::Internals::ShapedArrayCommon {
        method shape() {
            nqp::stmts(
              (my $idims := nqp::dimensions(self)),
              (my int $dims = nqp::elems($idims)),
              (my $odims  := nqp::setelems(nqp::create(IterationBuffer),$dims)),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$dims),
                nqp::bindpos($odims,$i,nqp::atpos_i($idims,$i))
              ),
              nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$odims)
            )
        }

        multi method EXISTS-POS(::?CLASS:D: **@indices) {
            nqp::p6bool(
              nqp::stmts(
                (my int $numdims = nqp::numdimensions(self)),
                (my int $numind  = @indices.elems),      # reifies
                (my $indices := nqp::getattr(@indices,List,'$!reified')),
                nqp::if(
                  nqp::isle_i($numind,$numdims),
                  nqp::stmts(
                    (my $dims := nqp::dimensions(self)),
                    (my int $i = -1),
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$numind)
                        && nqp::isge_i(nqp::atpos($indices,$i),0)
                        && nqp::islt_i(
                             nqp::atpos($indices,$i),
                             nqp::atpos_i($dims,$i)
                           ),
                      nqp::null
                    ),
                    nqp::iseq_i($i,$numind)
                  )
                )
              )
            )
        }

        proto method STORE(|) {*}
        multi method STORE(::?CLASS:D: Mu \item) {
            X::Assignment::ToShaped.new(shape => self.shape).throw
        }
    }

#- start of generated part of shapedintarray role -----------------------------
#- Generated on 2017-10-16T15:04:46+02:00 by tools/build/makeNATIVE_SHAPED_ARRAY.pl6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    role shapedintarray does shapedarray {
        multi method AT-POS(::?CLASS:D: **@indices) is raw {
            nqp::if(
              nqp::iseq_i(
                (my int $numdims = nqp::numdimensions(self)),
                (my int $numind  = @indices.elems),  # reifies
              ),
              nqp::stmts(
                (my $indices := nqp::getattr(@indices,List,'$!reified')),
                (my $idxs := nqp::list_i),
                nqp::while(                          # native index list
                  nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                  nqp::push_i($idxs,nqp::shift($indices))
                ),
#?if moar
                nqp::multidimref_i(self,$idxs)
#?endif
#?if !moar
                nqp::atposnd_i(self,$idxs)
#?endif
              ),
              nqp::if(
                nqp::isgt_i($numind,$numdims),
                X::TooManyDimensions.new(
                  operation => 'access',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw,
                X::NYI.new(
                  feature => "Partially dimensioned views of shaped arrays"
                ).throw
              )
            )
        }

        multi method ASSIGN-POS(::?CLASS:D: **@indices) {
            nqp::stmts(
              (my int $value = @indices.pop),
              nqp::if(
                nqp::iseq_i(
                  (my int $numdims = nqp::numdimensions(self)),
                  (my int $numind  = @indices.elems),  # reifies
                ),
                nqp::stmts(
                  (my $indices := nqp::getattr(@indices,List,'$!reified')),
                  (my $idxs := nqp::list_i),
                  nqp::while(                          # native index list
                    nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                    nqp::push_i($idxs,nqp::shift($indices))
                  ),
                  nqp::bindposnd_i(self, $idxs, $value)
                ),
                nqp::if(
                  nqp::isgt_i($numind,$numdims),
                  X::TooManyDimensions,
                  X::NotEnoughDimensions
                ).new(
                  operation => 'assign to',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw
              )
            )
        }

        sub NATCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := from),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) {
                    nqp::create(self).INIT(to,from)
                }
                method result(--> Nil) {
                    nqp::bindposnd_i($!list,$!indices,
#?if moar
                      nqp::multidimref_i($!from,$!indices))
#?endif
#?if !moar
                      nqp::atposnd_i($!from,$!indices))
#?endif
                }
            }.new(to,from).sink-all;
            to
        }
        sub OBJCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := nqp::getattr(from,List,'$!reified')),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) {
                    nqp::create(self).INIT(to,from)
                }
                method result(--> Nil) {
                    nqp::bindposnd_i($!list,$!indices,
                      nqp::atposnd($!from,$!indices))
                }
            }.new(to,from).sink-all;
            to
        }
        sub ITERCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeBranch {
                has $!iterators;
                method INIT(\to,\from) {
                    nqp::stmts(
                      self.SET-SELF(to),
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
                      nqp::atposnd_i($!list,$!indices)    # boom!
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
                            X::Assignment::ToShaped.new(shape => $!dims).throw
                          )
                        )
                      ),
                      (my $iter := nqp::atpos($!iterators,$!maxdim)),
                      nqp::until(                       # loop over highest dim
                        nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd)
                          || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
                        nqp::stmts(
                          nqp::bindposnd_i($!list,$!indices,$pulled),
                          nqp::bindpos_i($!indices,$!maxdim,  # increment index
                            nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
                        )
                      ),
                      nqp::unless(
                        nqp::eqaddr($pulled,IterationEnd) # if not exhausted
                          || nqp::isle_i(                 # and index too high
                               nqp::atpos_i($!indices,$!maxdim),$!maxind)
                          || $iter.is-lazy,               # and not lazy
                        nqp::atposnd_i($!list,$!indices)  # boom!
                      )
                    )
                }
            }.new(to,from).sink-all;
            to
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              EQV_DIMENSIONS(self,from),
              NATCPY(self,from),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: array:D \from) {
            nqp::if(
              nqp::istype(from.of,Int),
              nqp::if(
                EQV_DIMENSIONS(self,from),
                NATCPY(self,from),
                X::Assignment::ArrayShapeMismatch.new(
                  source-shape => from.shape,
                  target-shape => self.shape
                ).throw
              ),
              X::TypeCheck::Assignment.new(
                symbol   => self.^name ~ '[' ~ self.shape.join(';') ~ ']',
                expected => Int,
                got      => from.of
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \from) {
            nqp::if(
              nqp::can(from,'shape'),
              nqp::if(
                from.shape eqv self.shape,
                OBJCPY(self,from),
                X::Assignment::ArrayShapeMismatch.new(
                    source-shape => from.shape,
                    target-shape => self.shape
                ).throw
              ),
              ITERCPY(self,from)
            )
        }
        method iterator(::?CLASS:D:) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                method result() is raw {
#?if moar
                    nqp::multidimref_i($!list,nqp::clone($!indices))
#?endif
#?if !moar
                    nqp::atposnd_i($!list,nqp::clone($!indices))
#?endif
                }
            }.new(self)
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
#?if moar
                      nqp::multidimref_i($!list,nqp::clone($!indices))
#?endif
#?if !moar
                      nqp::atposnd_i($!list,nqp::clone($!indices))
#?endif
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
                method result() {
                    Pair.new(
                      self.indices,
#?if moar
                      nqp::multidimref_i($!list,nqp::clone($!indices))
#?endif
#?if !moar
                      nqp::atposnd_i($!list,nqp::clone($!indices))
#?endif
                    )
                }
            }.new(self))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(class :: does Rakudo::Iterator::ShapeLeaf {
                method result() {
                    Pair.new(nqp::atposnd_i($!list,$!indices),self.indices)
                }
            }.new(self))
        }
    }  # end of shapedintarray role

    role shaped1intarray does shapedintarray {
        multi method AT-POS(::?CLASS:D: int \one) is raw {
           nqp::atposref_i(self,one)
        }
        multi method AT-POS(::?CLASS:D: Int:D \one) is raw {
           nqp::atposref_i(self,one)
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \value) {
            nqp::bindpos_i(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, int \value) {
            nqp::bindpos_i(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int \one, Int:D \value) {
            nqp::bindpos_i(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \value) {
            nqp::bindpos_i(self,one,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one) {
            nqp::p6bool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one) {
            nqp::p6bool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              nqp::iseq_i((my int $elems = nqp::elems(self)),nqp::elems(from)),
              nqp::stmts(
                (my int $i = -1),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos_i(self,$i,nqp::atpos_i(from,$i))
                ),
                self
              ),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            nqp::stmts(
              (my \iter := in.iterator),
              (my int $elems = nqp::elems(self)),
              (my int $i = -1),
              nqp::until(
                nqp::eqaddr((my $pulled := iter.pull-one),IterationEnd)
                  || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_i(self,$i,$pulled)
              ),
              nqp::unless(
                nqp::islt_i($i,$elems) || iter.is-lazy,
                nqp::atpos_i(list,$i) # too many values on non-lazy it
              ),
              self
            )
        }
        multi method STORE(::?CLASS:D: Int:D \item) {
            nqp::stmts(
              nqp::bindpos_i(self,0,item),
              self
            )
        }
        method iterator(::?CLASS:D:) {
            class :: does Iterator {
                has Mu $!list;
                has int $!pos;
                method !SET-SELF(Mu \list) {
                    nqp::stmts(
                      ($!list := list),
                      ($!pos = -1),
                      self
                    )
                }
                method new(Mu \list) { nqp::create(self)!SET-SELF(list) }
                method pull-one() is raw {
                    nqp::if(
                      nqp::islt_i(
                        ($!pos = nqp::add_i($!pos,1)),
                        nqp::elems($!list)
                      ),
                      nqp::atposref_i($!list,$!pos),
                      IterationEnd
                    )
                }
                method push-all($target --> IterationEnd) {
                    nqp::stmts(
                      (my int $elems = nqp::elems($!list)),
                      (my int $pos = $!pos),
                      nqp::while(
                        nqp::islt_i(($pos = nqp::add_i($pos,1)),$elems),
                        $target.push(nqp::atpos_i($!list,$pos))
                      ),
                      ($!pos = $pos)
                    )
                }
                method count-only() { nqp::p6box_i(nqp::elems($!list)) }
                method bool-only()  { nqp::p6bool(nqp::elems($!list)) }
                method sink-all(--> IterationEnd) {
                    $!pos = nqp::elems($!list)
                }
            }.new(self)
        }
        multi method kv(::?CLASS:D:) {
            my int $i = -1;
            my int $elems = nqp::add_i(nqp::elems(self),nqp::elems(self));
            Seq.new(Rakudo::Iterator.Callable({
                nqp::if(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::bitand_i($i,1),
                    nqp::atposref_i(self,nqp::bitshiftr_i($i,1)),
                    nqp::bitshiftr_i($i,1)
                  ),
                  IterationEnd
                )
            }))
        }
        multi method pairs(::?CLASS:D:) {
            my int $i = -1;
            my int $elems = nqp::elems(self);
            Seq.new(Rakudo::Iterator.Callable({
                nqp::if(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  Pair.new($i,nqp::atposref_i(self,$i)),
                  IterationEnd
                )
            }))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }
        method reverse(::?CLASS:D:) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my int $last  = nqp::sub_i($elems,1)),
              (my int $i     = -1),
              (my $to := nqp::clone(self)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_i($to,nqp::sub_i($last,$i),
                  nqp::atpos_i(self,$i))
              ),
              $to
            )
        }
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my $to := nqp::clone(self)),
              (my int $i = -1),
              (my int $j =
                nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),$rotate),$elems)),
              nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_i(
                  $to,
                  ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
                  nqp::atpos_i(self,$i)
                ),
              ),
              $to
            )
        }
    } # end of shaped1intarray role

    role shaped2intarray does shapedintarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two) is raw {
#?if moar
            nqp::multidimref_i(self,nqp::list_i(one, two))
#?endif
#?if !moar
            nqp::atpos2d_i(self,one,two)
#?endif
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two) is raw {
#?if moar
            nqp::multidimref_i(self,nqp::list_i(one, two))
#?endif
#?if !moar
            nqp::atpos2d_i(self,one,two)
#?endif
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, Int:D \value) {
            nqp::bindpos2d_i(self,one,two,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \value) {
            nqp::bindpos2d_i(self,one,two,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
    } # end of shaped2intarray role

    role shaped3intarray does shapedintarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three) is raw {
#?if moar
            nqp::multidimref_i(self,nqp::list_i(one, two, three))
#?endif
#?if !moar
            nqp::atpos3d_i(self,one,two,three)
#?endif
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) is raw {
#?if moar
            nqp::multidimref_i(self,nqp::list_i(one, two, three))
#?endif
#?if !moar
            nqp::atpos3d_i(self,one,two,three)
#?endif
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, Int:D \value) {
            nqp::bindpos3d_i(self,one,two,three,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, Int:D \value) {
            nqp::bindpos3d_i(self,one,two,three,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
    } # end of shaped3intarray role
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of shapedintarray role -------------------------------

#- start of generated part of shapednumarray role -----------------------------
#- Generated on 2017-10-16T15:04:46+02:00 by tools/build/makeNATIVE_SHAPED_ARRAY.pl6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    role shapednumarray does shapedarray {
        multi method AT-POS(::?CLASS:D: **@indices) is raw {
            nqp::if(
              nqp::iseq_i(
                (my int $numdims = nqp::numdimensions(self)),
                (my int $numind  = @indices.elems),  # reifies
              ),
              nqp::stmts(
                (my $indices := nqp::getattr(@indices,List,'$!reified')),
                (my $idxs := nqp::list_i),
                nqp::while(                          # native index list
                  nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                  nqp::push_i($idxs,nqp::shift($indices))
                ),
#?if moar
                nqp::multidimref_n(self,$idxs)
#?endif
#?if !moar
                nqp::atposnd_n(self,$idxs)
#?endif
              ),
              nqp::if(
                nqp::isgt_i($numind,$numdims),
                X::TooManyDimensions.new(
                  operation => 'access',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw,
                X::NYI.new(
                  feature => "Partially dimensioned views of shaped arrays"
                ).throw
              )
            )
        }

        multi method ASSIGN-POS(::?CLASS:D: **@indices) {
            nqp::stmts(
              (my num $value = @indices.pop),
              nqp::if(
                nqp::iseq_i(
                  (my int $numdims = nqp::numdimensions(self)),
                  (my int $numind  = @indices.elems),  # reifies
                ),
                nqp::stmts(
                  (my $indices := nqp::getattr(@indices,List,'$!reified')),
                  (my $idxs := nqp::list_i),
                  nqp::while(                          # native index list
                    nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                    nqp::push_i($idxs,nqp::shift($indices))
                  ),
                  nqp::bindposnd_n(self, $idxs, $value)
                ),
                nqp::if(
                  nqp::isgt_i($numind,$numdims),
                  X::TooManyDimensions,
                  X::NotEnoughDimensions
                ).new(
                  operation => 'assign to',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw
              )
            )
        }

        sub NATCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := from),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) {
                    nqp::create(self).INIT(to,from)
                }
                method result(--> Nil) {
                    nqp::bindposnd_n($!list,$!indices,
#?if moar
                      nqp::multidimref_n($!from,$!indices))
#?endif
#?if !moar
                      nqp::atposnd_n($!from,$!indices))
#?endif
                }
            }.new(to,from).sink-all;
            to
        }
        sub OBJCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := nqp::getattr(from,List,'$!reified')),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) {
                    nqp::create(self).INIT(to,from)
                }
                method result(--> Nil) {
                    nqp::bindposnd_n($!list,$!indices,
                      nqp::atposnd($!from,$!indices))
                }
            }.new(to,from).sink-all;
            to
        }
        sub ITERCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeBranch {
                has $!iterators;
                method INIT(\to,\from) {
                    nqp::stmts(
                      self.SET-SELF(to),
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
                      nqp::atposnd_n($!list,$!indices)    # boom!
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
                            X::Assignment::ToShaped.new(shape => $!dims).throw
                          )
                        )
                      ),
                      (my $iter := nqp::atpos($!iterators,$!maxdim)),
                      nqp::until(                       # loop over highest dim
                        nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd)
                          || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
                        nqp::stmts(
                          nqp::bindposnd_n($!list,$!indices,$pulled),
                          nqp::bindpos_i($!indices,$!maxdim,  # increment index
                            nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
                        )
                      ),
                      nqp::unless(
                        nqp::eqaddr($pulled,IterationEnd) # if not exhausted
                          || nqp::isle_i(                 # and index too high
                               nqp::atpos_i($!indices,$!maxdim),$!maxind)
                          || $iter.is-lazy,               # and not lazy
                        nqp::atposnd_n($!list,$!indices)  # boom!
                      )
                    )
                }
            }.new(to,from).sink-all;
            to
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              EQV_DIMENSIONS(self,from),
              NATCPY(self,from),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: array:D \from) {
            nqp::if(
              nqp::istype(from.of,Num),
              nqp::if(
                EQV_DIMENSIONS(self,from),
                NATCPY(self,from),
                X::Assignment::ArrayShapeMismatch.new(
                  source-shape => from.shape,
                  target-shape => self.shape
                ).throw
              ),
              X::TypeCheck::Assignment.new(
                symbol   => self.^name ~ '[' ~ self.shape.join(';') ~ ']',
                expected => Num,
                got      => from.of
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \from) {
            nqp::if(
              nqp::can(from,'shape'),
              nqp::if(
                from.shape eqv self.shape,
                OBJCPY(self,from),
                X::Assignment::ArrayShapeMismatch.new(
                    source-shape => from.shape,
                    target-shape => self.shape
                ).throw
              ),
              ITERCPY(self,from)
            )
        }
        method iterator(::?CLASS:D:) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                method result() is raw {
#?if moar
                    nqp::multidimref_n($!list,nqp::clone($!indices))
#?endif
#?if !moar
                    nqp::atposnd_n($!list,nqp::clone($!indices))
#?endif
                }
            }.new(self)
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
#?if moar
                      nqp::multidimref_n($!list,nqp::clone($!indices))
#?endif
#?if !moar
                      nqp::atposnd_n($!list,nqp::clone($!indices))
#?endif
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
                method result() {
                    Pair.new(
                      self.indices,
#?if moar
                      nqp::multidimref_n($!list,nqp::clone($!indices))
#?endif
#?if !moar
                      nqp::atposnd_n($!list,nqp::clone($!indices))
#?endif
                    )
                }
            }.new(self))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(class :: does Rakudo::Iterator::ShapeLeaf {
                method result() {
                    Pair.new(nqp::atposnd_n($!list,$!indices),self.indices)
                }
            }.new(self))
        }
    }  # end of shapednumarray role

    role shaped1numarray does shapednumarray {
        multi method AT-POS(::?CLASS:D: int \one) is raw {
           nqp::atposref_n(self,one)
        }
        multi method AT-POS(::?CLASS:D: Int:D \one) is raw {
           nqp::atposref_n(self,one)
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, num \value) {
            nqp::bindpos_n(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, num \value) {
            nqp::bindpos_n(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int \one, Num:D \value) {
            nqp::bindpos_n(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Num:D \value) {
            nqp::bindpos_n(self,one,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one) {
            nqp::p6bool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one) {
            nqp::p6bool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              nqp::iseq_i((my int $elems = nqp::elems(self)),nqp::elems(from)),
              nqp::stmts(
                (my int $i = -1),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos_n(self,$i,nqp::atpos_n(from,$i))
                ),
                self
              ),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            nqp::stmts(
              (my \iter := in.iterator),
              (my int $elems = nqp::elems(self)),
              (my int $i = -1),
              nqp::until(
                nqp::eqaddr((my $pulled := iter.pull-one),IterationEnd)
                  || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_n(self,$i,$pulled)
              ),
              nqp::unless(
                nqp::islt_i($i,$elems) || iter.is-lazy,
                nqp::atpos_n(list,$i) # too many values on non-lazy it
              ),
              self
            )
        }
        multi method STORE(::?CLASS:D: Num:D \item) {
            nqp::stmts(
              nqp::bindpos_n(self,0,item),
              self
            )
        }
        method iterator(::?CLASS:D:) {
            class :: does Iterator {
                has Mu $!list;
                has int $!pos;
                method !SET-SELF(Mu \list) {
                    nqp::stmts(
                      ($!list := list),
                      ($!pos = -1),
                      self
                    )
                }
                method new(Mu \list) { nqp::create(self)!SET-SELF(list) }
                method pull-one() is raw {
                    nqp::if(
                      nqp::islt_i(
                        ($!pos = nqp::add_i($!pos,1)),
                        nqp::elems($!list)
                      ),
                      nqp::atposref_n($!list,$!pos),
                      IterationEnd
                    )
                }
                method push-all($target --> IterationEnd) {
                    nqp::stmts(
                      (my int $elems = nqp::elems($!list)),
                      (my int $pos = $!pos),
                      nqp::while(
                        nqp::islt_i(($pos = nqp::add_i($pos,1)),$elems),
                        $target.push(nqp::atpos_n($!list,$pos))
                      ),
                      ($!pos = $pos)
                    )
                }
                method count-only() { nqp::p6box_i(nqp::elems($!list)) }
                method bool-only()  { nqp::p6bool(nqp::elems($!list)) }
                method sink-all(--> IterationEnd) {
                    $!pos = nqp::elems($!list)
                }
            }.new(self)
        }
        multi method kv(::?CLASS:D:) {
            my int $i = -1;
            my int $elems = nqp::add_i(nqp::elems(self),nqp::elems(self));
            Seq.new(Rakudo::Iterator.Callable({
                nqp::if(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::bitand_i($i,1),
                    nqp::atposref_n(self,nqp::bitshiftr_i($i,1)),
                    nqp::bitshiftr_i($i,1)
                  ),
                  IterationEnd
                )
            }))
        }
        multi method pairs(::?CLASS:D:) {
            my int $i = -1;
            my int $elems = nqp::elems(self);
            Seq.new(Rakudo::Iterator.Callable({
                nqp::if(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  Pair.new($i,nqp::atposref_n(self,$i)),
                  IterationEnd
                )
            }))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }
        method reverse(::?CLASS:D:) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my int $last  = nqp::sub_i($elems,1)),
              (my int $i     = -1),
              (my $to := nqp::clone(self)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_n($to,nqp::sub_i($last,$i),
                  nqp::atpos_n(self,$i))
              ),
              $to
            )
        }
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my $to := nqp::clone(self)),
              (my int $i = -1),
              (my int $j =
                nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),$rotate),$elems)),
              nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_n(
                  $to,
                  ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
                  nqp::atpos_n(self,$i)
                ),
              ),
              $to
            )
        }
    } # end of shaped1numarray role

    role shaped2numarray does shapednumarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two) is raw {
#?if moar
            nqp::multidimref_n(self,nqp::list_i(one, two))
#?endif
#?if !moar
            nqp::atpos2d_n(self,one,two)
#?endif
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two) is raw {
#?if moar
            nqp::multidimref_n(self,nqp::list_i(one, two))
#?endif
#?if !moar
            nqp::atpos2d_n(self,one,two)
#?endif
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, Num:D \value) {
            nqp::bindpos2d_n(self,one,two,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Num:D \value) {
            nqp::bindpos2d_n(self,one,two,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
    } # end of shaped2numarray role

    role shaped3numarray does shapednumarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three) is raw {
#?if moar
            nqp::multidimref_n(self,nqp::list_i(one, two, three))
#?endif
#?if !moar
            nqp::atpos3d_n(self,one,two,three)
#?endif
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) is raw {
#?if moar
            nqp::multidimref_n(self,nqp::list_i(one, two, three))
#?endif
#?if !moar
            nqp::atpos3d_n(self,one,two,three)
#?endif
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, Num:D \value) {
            nqp::bindpos3d_n(self,one,two,three,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, Num:D \value) {
            nqp::bindpos3d_n(self,one,two,three,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
    } # end of shaped3numarray role
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of shapednumarray role -------------------------------

#- start of generated part of shapedstrarray role -----------------------------
#- Generated on 2017-10-16T15:04:46+02:00 by tools/build/makeNATIVE_SHAPED_ARRAY.pl6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    role shapedstrarray does shapedarray {
        multi method AT-POS(::?CLASS:D: **@indices) is raw {
            nqp::if(
              nqp::iseq_i(
                (my int $numdims = nqp::numdimensions(self)),
                (my int $numind  = @indices.elems),  # reifies
              ),
              nqp::stmts(
                (my $indices := nqp::getattr(@indices,List,'$!reified')),
                (my $idxs := nqp::list_i),
                nqp::while(                          # native index list
                  nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                  nqp::push_i($idxs,nqp::shift($indices))
                ),
#?if moar
                nqp::multidimref_s(self,$idxs)
#?endif
#?if !moar
                nqp::atposnd_s(self,$idxs)
#?endif
              ),
              nqp::if(
                nqp::isgt_i($numind,$numdims),
                X::TooManyDimensions.new(
                  operation => 'access',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw,
                X::NYI.new(
                  feature => "Partially dimensioned views of shaped arrays"
                ).throw
              )
            )
        }

        multi method ASSIGN-POS(::?CLASS:D: **@indices) {
            nqp::stmts(
              (my str $value = @indices.pop),
              nqp::if(
                nqp::iseq_i(
                  (my int $numdims = nqp::numdimensions(self)),
                  (my int $numind  = @indices.elems),  # reifies
                ),
                nqp::stmts(
                  (my $indices := nqp::getattr(@indices,List,'$!reified')),
                  (my $idxs := nqp::list_i),
                  nqp::while(                          # native index list
                    nqp::isge_i(($numdims = nqp::sub_i($numdims,1)),0),
                    nqp::push_i($idxs,nqp::shift($indices))
                  ),
                  nqp::bindposnd_s(self, $idxs, $value)
                ),
                nqp::if(
                  nqp::isgt_i($numind,$numdims),
                  X::TooManyDimensions,
                  X::NotEnoughDimensions
                ).new(
                  operation => 'assign to',
                  got-dimensions => $numind,
                  needed-dimensions => $numdims
                ).throw
              )
            )
        }

        sub NATCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := from),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) {
                    nqp::create(self).INIT(to,from)
                }
                method result(--> Nil) {
                    nqp::bindposnd_s($!list,$!indices,
#?if moar
                      nqp::multidimref_s($!from,$!indices))
#?endif
#?if !moar
                      nqp::atposnd_s($!from,$!indices))
#?endif
                }
            }.new(to,from).sink-all;
            to
        }
        sub OBJCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeLeaf {
                has Mu $!from;
                method INIT(Mu \to, Mu \from) {
                    nqp::stmts(
                      ($!from := nqp::getattr(from,List,'$!reified')),
                      self.SET-SELF(to)
                    )
                }
                method new(Mu \to, Mu \from) {
                    nqp::create(self).INIT(to,from)
                }
                method result(--> Nil) {
                    nqp::bindposnd_s($!list,$!indices,
                      nqp::atposnd($!from,$!indices))
                }
            }.new(to,from).sink-all;
            to
        }
        sub ITERCPY(Mu \to, Mu \from) is raw {
            class :: does Rakudo::Iterator::ShapeBranch {
                has $!iterators;
                method INIT(\to,\from) {
                    nqp::stmts(
                      self.SET-SELF(to),
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
                      nqp::atposnd_s($!list,$!indices)    # boom!
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
                            X::Assignment::ToShaped.new(shape => $!dims).throw
                          )
                        )
                      ),
                      (my $iter := nqp::atpos($!iterators,$!maxdim)),
                      nqp::until(                       # loop over highest dim
                        nqp::eqaddr((my $pulled := $iter.pull-one),IterationEnd)
                          || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
                        nqp::stmts(
                          nqp::bindposnd_s($!list,$!indices,$pulled),
                          nqp::bindpos_i($!indices,$!maxdim,  # increment index
                            nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
                        )
                      ),
                      nqp::unless(
                        nqp::eqaddr($pulled,IterationEnd) # if not exhausted
                          || nqp::isle_i(                 # and index too high
                               nqp::atpos_i($!indices,$!maxdim),$!maxind)
                          || $iter.is-lazy,               # and not lazy
                        nqp::atposnd_s($!list,$!indices)  # boom!
                      )
                    )
                }
            }.new(to,from).sink-all;
            to
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              EQV_DIMENSIONS(self,from),
              NATCPY(self,from),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: array:D \from) {
            nqp::if(
              nqp::istype(from.of,Str),
              nqp::if(
                EQV_DIMENSIONS(self,from),
                NATCPY(self,from),
                X::Assignment::ArrayShapeMismatch.new(
                  source-shape => from.shape,
                  target-shape => self.shape
                ).throw
              ),
              X::TypeCheck::Assignment.new(
                symbol   => self.^name ~ '[' ~ self.shape.join(';') ~ ']',
                expected => Str,
                got      => from.of
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \from) {
            nqp::if(
              nqp::can(from,'shape'),
              nqp::if(
                from.shape eqv self.shape,
                OBJCPY(self,from),
                X::Assignment::ArrayShapeMismatch.new(
                    source-shape => from.shape,
                    target-shape => self.shape
                ).throw
              ),
              ITERCPY(self,from)
            )
        }
        method iterator(::?CLASS:D:) {
            class :: does Rakudo::Iterator::ShapeLeaf {
                method result() is raw {
#?if moar
                    nqp::multidimref_s($!list,nqp::clone($!indices))
#?endif
#?if !moar
                    nqp::atposnd_s($!list,nqp::clone($!indices))
#?endif
                }
            }.new(self)
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
#?if moar
                      nqp::multidimref_s($!list,nqp::clone($!indices))
#?endif
#?if !moar
                      nqp::atposnd_s($!list,nqp::clone($!indices))
#?endif
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
                method result() {
                    Pair.new(
                      self.indices,
#?if moar
                      nqp::multidimref_s($!list,nqp::clone($!indices))
#?endif
#?if !moar
                      nqp::atposnd_s($!list,nqp::clone($!indices))
#?endif
                    )
                }
            }.new(self))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(class :: does Rakudo::Iterator::ShapeLeaf {
                method result() {
                    Pair.new(nqp::atposnd_s($!list,$!indices),self.indices)
                }
            }.new(self))
        }
    }  # end of shapedstrarray role

    role shaped1strarray does shapedstrarray {
        multi method AT-POS(::?CLASS:D: int \one) is raw {
           nqp::atposref_s(self,one)
        }
        multi method AT-POS(::?CLASS:D: Int:D \one) is raw {
           nqp::atposref_s(self,one)
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, str \value) {
            nqp::bindpos_s(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, str \value) {
            nqp::bindpos_s(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int \one, Str:D \value) {
            nqp::bindpos_s(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Str:D \value) {
            nqp::bindpos_s(self,one,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one) {
            nqp::p6bool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one) {
            nqp::p6bool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }

        multi method STORE(::?CLASS:D: ::?CLASS:D \from) {
            nqp::if(
              nqp::iseq_i((my int $elems = nqp::elems(self)),nqp::elems(from)),
              nqp::stmts(
                (my int $i = -1),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::bindpos_s(self,$i,nqp::atpos_s(from,$i))
                ),
                self
              ),
              X::Assignment::ArrayShapeMismatch.new(
                source-shape => from.shape,
                target-shape => self.shape
              ).throw
            )
        }
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            nqp::stmts(
              (my \iter := in.iterator),
              (my int $elems = nqp::elems(self)),
              (my int $i = -1),
              nqp::until(
                nqp::eqaddr((my $pulled := iter.pull-one),IterationEnd)
                  || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_s(self,$i,$pulled)
              ),
              nqp::unless(
                nqp::islt_i($i,$elems) || iter.is-lazy,
                nqp::atpos_s(list,$i) # too many values on non-lazy it
              ),
              self
            )
        }
        multi method STORE(::?CLASS:D: Str:D \item) {
            nqp::stmts(
              nqp::bindpos_s(self,0,item),
              self
            )
        }
        method iterator(::?CLASS:D:) {
            class :: does Iterator {
                has Mu $!list;
                has int $!pos;
                method !SET-SELF(Mu \list) {
                    nqp::stmts(
                      ($!list := list),
                      ($!pos = -1),
                      self
                    )
                }
                method new(Mu \list) { nqp::create(self)!SET-SELF(list) }
                method pull-one() is raw {
                    nqp::if(
                      nqp::islt_i(
                        ($!pos = nqp::add_i($!pos,1)),
                        nqp::elems($!list)
                      ),
                      nqp::atposref_s($!list,$!pos),
                      IterationEnd
                    )
                }
                method push-all($target --> IterationEnd) {
                    nqp::stmts(
                      (my int $elems = nqp::elems($!list)),
                      (my int $pos = $!pos),
                      nqp::while(
                        nqp::islt_i(($pos = nqp::add_i($pos,1)),$elems),
                        $target.push(nqp::atpos_s($!list,$pos))
                      ),
                      ($!pos = $pos)
                    )
                }
                method count-only() { nqp::p6box_i(nqp::elems($!list)) }
                method bool-only()  { nqp::p6bool(nqp::elems($!list)) }
                method sink-all(--> IterationEnd) {
                    $!pos = nqp::elems($!list)
                }
            }.new(self)
        }
        multi method kv(::?CLASS:D:) {
            my int $i = -1;
            my int $elems = nqp::add_i(nqp::elems(self),nqp::elems(self));
            Seq.new(Rakudo::Iterator.Callable({
                nqp::if(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::bitand_i($i,1),
                    nqp::atposref_s(self,nqp::bitshiftr_i($i,1)),
                    nqp::bitshiftr_i($i,1)
                  ),
                  IterationEnd
                )
            }))
        }
        multi method pairs(::?CLASS:D:) {
            my int $i = -1;
            my int $elems = nqp::elems(self);
            Seq.new(Rakudo::Iterator.Callable({
                nqp::if(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  Pair.new($i,nqp::atposref_s(self,$i)),
                  IterationEnd
                )
            }))
        }
        multi method antipairs(::?CLASS:D:) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }
        method reverse(::?CLASS:D:) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my int $last  = nqp::sub_i($elems,1)),
              (my int $i     = -1),
              (my $to := nqp::clone(self)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_s($to,nqp::sub_i($last,$i),
                  nqp::atpos_s(self,$i))
              ),
              $to
            )
        }
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1) is nodal {
            nqp::stmts(
              (my int $elems = nqp::elems(self)),
              (my $to := nqp::clone(self)),
              (my int $i = -1),
              (my int $j =
                nqp::mod_i(nqp::sub_i(nqp::sub_i($elems,1),$rotate),$elems)),
              nqp::if(nqp::islt_i($j,0),($j = nqp::add_i($j,$elems))),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_s(
                  $to,
                  ($j = nqp::mod_i(nqp::add_i($j,1),$elems)),
                  nqp::atpos_s(self,$i)
                ),
              ),
              $to
            )
        }
    } # end of shaped1strarray role

    role shaped2strarray does shapedstrarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two) is raw {
#?if moar
            nqp::multidimref_s(self,nqp::list_i(one, two))
#?endif
#?if !moar
            nqp::atpos2d_s(self,one,two)
#?endif
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two) is raw {
#?if moar
            nqp::multidimref_s(self,nqp::list_i(one, two))
#?endif
#?if !moar
            nqp::atpos2d_s(self,one,two)
#?endif
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, Str:D \value) {
            nqp::bindpos2d_s(self,one,two,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Str:D \value) {
            nqp::bindpos2d_s(self,one,two,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
    } # end of shaped2strarray role

    role shaped3strarray does shapedstrarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three) is raw {
#?if moar
            nqp::multidimref_s(self,nqp::list_i(one, two, three))
#?endif
#?if !moar
            nqp::atpos3d_s(self,one,two,three)
#?endif
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) is raw {
#?if moar
            nqp::multidimref_s(self,nqp::list_i(one, two, three))
#?endif
#?if !moar
            nqp::atpos3d_s(self,one,two,three)
#?endif
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, Str:D \value) {
            nqp::bindpos3d_s(self,one,two,three,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, Str:D \value) {
            nqp::bindpos3d_s(self,one,two,three,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three) {
            nqp::p6bool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
    } # end of shaped3strarray role
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of shapedstrarray role -------------------------------

    method ^parameterize(Mu:U \arr, Mu:U \t) {
        my $t := nqp::decont(t);
        my int $kind = nqp::objprimspec($t);
        my $what;

        if $kind == 1 {
            $what := arr.^mixin(intarray[$t]);
        }
        elsif $kind == 2 {
            $what := arr.^mixin(numarray[$t]);
        }
        elsif $kind == 3 {
            $what := arr.^mixin(strarray[$t]);
        }
        else {
            die "Can only parameterize array with a native type, not {t.^name}";
        }

        $what.^set_name("{arr.^name}[{t.^name}]");
        $what;
    }

    # poor man's 3x4 matrix
    constant typedim2role := nqp::list(nqp::null,
      nqp::list(shapedintarray,shaped1intarray,shaped2intarray,shaped3intarray),
      nqp::list(shapednumarray,shaped1numarray,shaped2numarray,shaped3numarray),
      nqp::list(shapedstrarray,shaped1strarray,shaped2strarray,shaped3strarray)
    );

    method !shaped(\shape) {
        nqp::if(
          (my int $dims = shape.elems),   # reifies
          nqp::stmts(
            # Calculate new meta-object (probably hitting caches in most cases).
            (my \shaped-type = self.WHAT.^mixin(
              nqp::atpos(
                nqp::atpos(typedim2role,nqp::objprimspec(my \T = self.of)),
                nqp::isle_i($dims,3) && $dims
              )
            )),
            nqp::if(   # set name if needed
              nqp::isne_s(shaped-type.^name,self.WHAT.^name),
              shaped-type.^set_name(self.WHAT.^name)
            ),
            # Allocate array storage for this shape, based on calculated type.
            Rakudo::Internals.SHAPED-ARRAY-STORAGE(shape,shaped-type.HOW,T)
          ),
          X::NotEnoughDimensions.new(
            operation         => 'create',
            got-dimensions    => $dims,
            needed-dimensions => '',
          ).throw
        )
    }

    method BIND-POS(|) {
        die "Cannot bind to a natively typed array";
    }
    method DELETE-POS(|) {
        die "Cannot delete from a natively typed array";
    }

    proto method ASSIGN-POS(|) {*} # Hide candidates from Any
    multi method ASSIGN-POS(Any:U \SELF: \pos, Mu \assignee) { # auto-viv
       SELF.AT-POS(pos) = assignee;
    }
    multi method ASSIGN-POS(Any:D: Any:U \pos, Mu \assignee) { # undefined idx
        die "Cannot use '{pos.^name}' as an index";
    }

    multi method EXISTS-POS(array:D: int $idx) {
        $idx >= 0 && $idx < nqp::elems(self)
    }
    multi method EXISTS-POS(array:D: Int:D $idx) {
        $idx >= 0 && $idx < nqp::elems(self)
    }

    multi method Bool(array:D:)    { nqp::p6bool(nqp::elems(self)) }
    multi method Numeric(array:D:) { nqp::elems(self) }
    multi method Str(array:D:)     { self.join(' ') }

    multi method elems(array:D:)    { nqp::elems(self) }
    method shape() { (*,) }
    proto method Int(|) {*}
    multi method Int(array:D:)      { nqp::elems(self) }
    multi method end(array:D:)      { nqp::elems(self) - 1 }

    method eager() { self }
    method flat()  { Seq.new(self.iterator) }
    method list()  { List.from-iterator(self.iterator) }
    method sink(--> Nil) { }

    multi method gist(array:D:) {
        '[' ~ self.map(-> $elem {
            given ++$ {
                when 101 { '...' }
                when 102 { last }
                default  { $elem.gist }
            }
        } ).join(' ') ~ ']';
    }

    multi method perl(array:D:) {
        'array[' ~ self.of.perl ~ '].new(' ~
            self.map(*.perl).join(', ') ~ ')'
    }

    method FLATTENABLE_LIST() { self }
    method FLATTENABLE_HASH() { nqp::hash() }

    method iterator() {
        nqp::die('iterator must be provided by native array parameterization role')
    }

    method out_of_range(array:D \SELF: int $index) {
        Failure.new(X::OutOfRange.new(
          :what('Index'),
          :got($index),
          :range("0..{nqp::elems(SELF)}")
        ))
    }
}

multi sub postcircumfix:<[ ]>(array:D \SELF, Range:D \range ) is raw {
    nqp::if(
      nqp::iscont(range),
      SELF.AT-POS(range.Int),                    # range in a container
      nqp::if(
        nqp::getattr_i(range,Range,'$!is-int'),
        nqp::if(                                 # we have an integer range
          nqp::islt_i(
            (my int $min = nqp::add_i(
              nqp::getattr(range,Range,'$!min'),
              nqp::getattr_i(range,Range,'$!excludes-min')
            )),
            0
          ),
          SELF.out_of_range($min),                 # starts too low
          nqp::if(                                   # start in range
            nqp::isgt_i(
              $min,
              (my int $max = nqp::sub_i(
                nqp::getattr(range,Range,'$!max'),
                nqp::getattr_i(range,Range,'$!excludes-max')
              ))
            ),
            nqp::create(SELF),                         # wrong order, empty!
            nqp::if(                                   # correct order
              nqp::islt_i($max,nqp::elems(SELF)),
              nqp::slice(SELF,$min,$max),              # end in range, slice!
              nqp::setelems(                             # end not in range
                nqp::if(
                  nqp::islt_i($min,nqp::elems(SELF)),
                  nqp::slice(SELF,$min,-1),              # start in range
                  nqp::create(SELF)                      # start not in range
                ),
                nqp::add_i(nqp::sub_i($max,$min),1)
              )
            )
          )
        ),
        POSITIONS(SELF, range).map({ SELF[$_] }).eager.list
      )
    )
}

# vim: ft=perl6 expandtab sw=4
