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

    proto method STORE(array:D: |) {*}
    multi method STORE(array:D: *@values) { self.STORE(@values) }

    multi method push(array:D:    **@values) { self.append(@values) }
    multi method append(array:D:   *@values) { self.append(@values) }
    multi method unshift(array:D: **@values) { self.unshift(@values) }
    multi method prepend(array:D:  *@values) { self.unshift(@values) }

    sub INDEX_OUT_OF_RANGE(Int:D $got --> Nil) {
        X::OutOfRange.new(what => "Index", :$got, range => "0..^Inf").throw
    }

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
#- Generated on 2019-08-12T21:36:23+02:00 by tools/build/makeNATIVE_ARRAY.p6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method AT-POS(strarray:D: int $idx --> str) is raw {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::atposref_s(self,$idx)
        }
        multi method AT-POS(strarray:D: Int:D $idx --> str) is raw {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::atposref_s(self,$idx)
        }

        multi method ASSIGN-POS(strarray:D: int $idx, str $value --> str) {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: Int:D $idx, str $value --> str) {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: int $idx, Str:D $value --> str) {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: Int:D $idx, Str:D $value --> str) {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_s(self, $idx, $value)
        }
        multi method ASSIGN-POS(strarray:D: Any $idx, Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => "assignment to str array element #$idx",
                got       => value,
                expected  => T,
            ).throw;
        }

        multi method STORE(strarray:D: $value --> strarray:D) {
            nqp::setelems(self,1);
            nqp::bindpos_s(self, 0, nqp::unbox_s($value));
            self
        }
        multi method STORE(strarray:D: strarray:D \values --> strarray:D) {
            nqp::setelems(self,nqp::elems(values));
            nqp::splice(self,values,0,nqp::elems(values))
        }
        multi method STORE(strarray:D: Seq:D \seq --> strarray:D) {
            nqp::if(
              (my $iterator := seq.iterator).is-lazy,
              X::Cannot::Lazy.new(
                :action<store>, :what(self.^name)
              ).throw,
              nqp::stmts(
                nqp::setelems(self,0),
                $iterator.push-all(self),
                self
              )
            )
        }
        multi method STORE(strarray:D: List:D \values --> strarray:D) {
            my int $elems = values.elems;    # reifies
            my \reified := nqp::getattr(values,List,'$!reified');
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_s(self,$i,
                nqp::if(
                  nqp::isnull(nqp::atpos(reified,$i)),
                  "",
                  nqp::unbox_s(nqp::atpos(reified,$i))
                )
              )
            );
            self
        }
        multi method STORE(strarray:D: @values --> strarray:D) {
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

        multi method push(strarray:D: str $value --> strarray:D) {
            nqp::push_s(self, $value);
            self
        }
        multi method push(strarray:D: Str:D $value --> strarray:D) {
            nqp::push_s(self, $value);
            self
        }
        multi method push(strarray:D: Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => 'push to str array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method append(strarray:D: str $value --> strarray:D) {
            nqp::push_s(self, $value);
            self
        }
        multi method append(strarray:D: Str:D $value --> strarray:D) {
            nqp::push_s(self, $value);
            self
        }
        multi method append(strarray:D: strarray:D $values --> strarray:D) is default {
            nqp::splice(self,$values,nqp::elems(self),0)
        }
        multi method append(strarray:D: @values --> strarray:D) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_s(self, $_) for flat @values;
            self
        }

        method pop(strarray:D: --> str) {
            nqp::elems(self)
              ?? nqp::pop_s(self)
              !! X::Cannot::Empty.new(:action<pop>, :what(self.^name)).throw;
        }

        method shift(strarray:D: --> str) {
            nqp::elems(self)
              ?? nqp::shift_s(self)
              !! X::Cannot::Empty.new(:action<shift>, :what(self.^name)).throw;
        }

        multi method unshift(strarray:D: str $value --> strarray:D) {
            nqp::unshift_s(self, $value);
            self
        }
        multi method unshift(strarray:D: Str:D $value --> strarray:D) {
            nqp::unshift_s(self, $value);
            self
        }
        multi method unshift(strarray:D: @values --> strarray:D) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_s(self, @values.pop) while @values;
            self
        }
        multi method unshift(strarray:D: Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => 'unshift to str array',
                got       => value,
                expected  => T,
            ).throw;
        }

        my $empty_s := nqp::list_s;

        multi method splice(strarray:D: --> strarray:D) {
            my $splice := nqp::clone(self);
            nqp::setelems(self,0);
            $splice
        }
        multi method splice(strarray:D: Int:D \offset --> strarray:D) {
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
        multi method splice(strarray:D: Int:D $offset, Int:D $size --> strarray:D) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(self,$empty_s,$offset,$size)
            );
            $slice
        }
        multi method splice(strarray:D: Int:D $offset, Int:D $size, strarray:D \values --> strarray:D) {
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
        multi method splice(strarray:D: Int:D $offset, Int:D $size, Seq:D \seq --> strarray:D) {
            nqp::if(
              seq.is-lazy,
              X::Cannot::Lazy.new(:action<splice>, :what(self.^name)).throw,
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
        multi method splice(strarray:D: $offset=0, $size=Whatever, *@values --> strarray:D) {
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
        multi method minmax(strarray:D: --> Range:D) {
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

        my class Iterate does Iterator {
            has int $!i;
            has $!array;    # Native array we're iterating

            method !SET-SELF(\array) {
                $!array := nqp::decont(array);
                $!i = -1;
                self
            }
            method new(\array) { nqp::create(self)!SET-SELF(array) }

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
            method push-all(\target --> IterationEnd) {
                my int $i     = $!i;
                my int $elems = nqp::elems($!array);
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  target.push(nqp::atposref_s($!array,$i))
                );
                $!i = $i;
            }
        }
        method iterator(strarray:D: --> Iterate:D) { Iterate.new(self) }

        method reverse(strarray:D: --> strarray:D) is nodal {
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
        method rotate(strarray:D: Int(Cool) $rotate = 1 --> strarray:D) is nodal {
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
        multi method sort(strarray:D: --> strarray:D) {
            Rakudo::Sorting.MERGESORT-str(nqp::clone(self))
        }

        multi method ACCEPTS(strarray:D: strarray:D \other --> Bool:D) {
            nqp::hllbool(
              nqp::unless(
                nqp::eqaddr(self,other),
                nqp::if(
                  nqp::iseq_i(
                    (my int $elems = nqp::elems(self)),
                    nqp::elems(other)
                  ),
                  nqp::stmts(
                    (my int $i = -1),
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                        && nqp::iseq_s(
                             nqp::atpos_s(self,$i),
                             nqp::atpos_s(other,$i)
                           ),
                      nqp::null
                    ),
                    nqp::iseq_i($i,$elems)
                  )
                )
              )
            )
        }
        proto method grab(|) {*}
        multi method grab(strarray:D: --> str) {
            nqp::if(nqp::elems(self),self.GRAB_ONE,Nil)
        }
        multi method grab(strarray:D: Callable:D $calculate --> str) {
            self.grab($calculate(nqp::elems(self)))
        }
        multi method grab(strarray:D: Whatever --> Seq:D) { self.grab(Inf) }

        my class GrabN does Iterator {
            has $!array;
            has int $!count;

            method !SET-SELF(\array,\count) {
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
            method new(\a,\c) { nqp::create(self)!SET-SELF(a,c) }
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
        }
        multi method grab(strarray:D: \count --> Seq:D) {
            Seq.new(nqp::if(
              nqp::elems(self),
              GrabN.new(self,count),
              Rakudo::Iterator.Empty
            ))
        }

        method GRAB_ONE(strarray:D: --> str) {
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
#- Generated on 2019-08-12T21:36:23+02:00 by tools/build/makeNATIVE_ARRAY.p6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method AT-POS(intarray:D: int $idx --> int) is raw {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::atposref_i(self,$idx)
        }
        multi method AT-POS(intarray:D: Int:D $idx --> int) is raw {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::atposref_i(self,$idx)
        }

        multi method ASSIGN-POS(intarray:D: int $idx, int $value --> int) {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: Int:D $idx, int $value --> int) {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: int $idx, Int:D $value --> int) {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: Int:D $idx, Int:D $value --> int) {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(intarray:D: Any $idx, Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => "assignment to int array element #$idx",
                got       => value,
                expected  => T,
            ).throw;
        }

        multi method STORE(intarray:D: $value --> intarray:D) {
            nqp::setelems(self,1);
            nqp::bindpos_i(self, 0, nqp::unbox_i($value));
            self
        }
        multi method STORE(intarray:D: intarray:D \values --> intarray:D) {
            nqp::setelems(self,nqp::elems(values));
            nqp::splice(self,values,0,nqp::elems(values))
        }
        multi method STORE(intarray:D: Seq:D \seq --> intarray:D) {
            nqp::if(
              (my $iterator := seq.iterator).is-lazy,
              X::Cannot::Lazy.new(
                :action<store>, :what(self.^name)
              ).throw,
              nqp::stmts(
                nqp::setelems(self,0),
                $iterator.push-all(self),
                self
              )
            )
        }
        multi method STORE(intarray:D: List:D \values --> intarray:D) {
            my int $elems = values.elems;    # reifies
            my \reified := nqp::getattr(values,List,'$!reified');
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_i(self,$i,
                nqp::if(
                  nqp::isnull(nqp::atpos(reified,$i)),
                  0,
                  nqp::unbox_i(nqp::atpos(reified,$i))
                )
              )
            );
            self
        }
        multi method STORE(intarray:D: @values --> intarray:D) {
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

        multi method push(intarray:D: int $value --> intarray:D) {
            nqp::push_i(self, $value);
            self
        }
        multi method push(intarray:D: Int:D $value --> intarray:D) {
            nqp::push_i(self, $value);
            self
        }
        multi method push(intarray:D: Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => 'push to int array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method append(intarray:D: int $value --> intarray:D) {
            nqp::push_i(self, $value);
            self
        }
        multi method append(intarray:D: Int:D $value --> intarray:D) {
            nqp::push_i(self, $value);
            self
        }
        multi method append(intarray:D: intarray:D $values --> intarray:D) is default {
            nqp::splice(self,$values,nqp::elems(self),0)
        }
        multi method append(intarray:D: @values --> intarray:D) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_i(self, $_) for flat @values;
            self
        }

        method pop(intarray:D: --> int) {
            nqp::elems(self)
              ?? nqp::pop_i(self)
              !! X::Cannot::Empty.new(:action<pop>, :what(self.^name)).throw;
        }

        method shift(intarray:D: --> int) {
            nqp::elems(self)
              ?? nqp::shift_i(self)
              !! X::Cannot::Empty.new(:action<shift>, :what(self.^name)).throw;
        }

        multi method unshift(intarray:D: int $value --> intarray:D) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(intarray:D: Int:D $value --> intarray:D) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(intarray:D: @values --> intarray:D) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_i(self, @values.pop) while @values;
            self
        }
        multi method unshift(intarray:D: Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => 'unshift to int array',
                got       => value,
                expected  => T,
            ).throw;
        }

        my $empty_i := nqp::list_i;

        multi method splice(intarray:D: --> intarray:D) {
            my $splice := nqp::clone(self);
            nqp::setelems(self,0);
            $splice
        }
        multi method splice(intarray:D: Int:D \offset --> intarray:D) {
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
        multi method splice(intarray:D: Int:D $offset, Int:D $size --> intarray:D) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(self,$empty_i,$offset,$size)
            );
            $slice
        }
        multi method splice(intarray:D: Int:D $offset, Int:D $size, intarray:D \values --> intarray:D) {
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
        multi method splice(intarray:D: Int:D $offset, Int:D $size, Seq:D \seq --> intarray:D) {
            nqp::if(
              seq.is-lazy,
              X::Cannot::Lazy.new(:action<splice>, :what(self.^name)).throw,
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
        multi method splice(intarray:D: $offset=0, $size=Whatever, *@values --> intarray:D) {
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
        multi method minmax(intarray:D: --> Range:D) {
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

        my class Iterate does Iterator {
            has int $!i;
            has $!array;    # Native array we're iterating

            method !SET-SELF(\array) {
                $!array := nqp::decont(array);
                $!i = -1;
                self
            }
            method new(\array) { nqp::create(self)!SET-SELF(array) }

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
            method push-all(\target --> IterationEnd) {
                my int $i     = $!i;
                my int $elems = nqp::elems($!array);
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  target.push(nqp::atposref_i($!array,$i))
                );
                $!i = $i;
            }
        }
        method iterator(intarray:D: --> Iterate:D) { Iterate.new(self) }

        method reverse(intarray:D: --> intarray:D) is nodal {
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
        method rotate(intarray:D: Int(Cool) $rotate = 1 --> intarray:D) is nodal {
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
        multi method sort(intarray:D: --> intarray:D) {
            Rakudo::Sorting.MERGESORT-int(nqp::clone(self))
        }

        multi method ACCEPTS(intarray:D: intarray:D \other --> Bool:D) {
            nqp::hllbool(
              nqp::unless(
                nqp::eqaddr(self,other),
                nqp::if(
                  nqp::iseq_i(
                    (my int $elems = nqp::elems(self)),
                    nqp::elems(other)
                  ),
                  nqp::stmts(
                    (my int $i = -1),
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                        && nqp::iseq_i(
                             nqp::atpos_i(self,$i),
                             nqp::atpos_i(other,$i)
                           ),
                      nqp::null
                    ),
                    nqp::iseq_i($i,$elems)
                  )
                )
              )
            )
        }
        proto method grab(|) {*}
        multi method grab(intarray:D: --> int) {
            nqp::if(nqp::elems(self),self.GRAB_ONE,Nil)
        }
        multi method grab(intarray:D: Callable:D $calculate --> int) {
            self.grab($calculate(nqp::elems(self)))
        }
        multi method grab(intarray:D: Whatever --> Seq:D) { self.grab(Inf) }

        my class GrabN does Iterator {
            has $!array;
            has int $!count;

            method !SET-SELF(\array,\count) {
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
            method new(\a,\c) { nqp::create(self)!SET-SELF(a,c) }
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
        }
        multi method grab(intarray:D: \count --> Seq:D) {
            Seq.new(nqp::if(
              nqp::elems(self),
              GrabN.new(self,count),
              Rakudo::Iterator.Empty
            ))
        }

        method GRAB_ONE(intarray:D: --> int) {
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

        method sum(intarray:D: :$wrap) {
            nqp::if(
              (my int $elems = nqp::elems(self)),
              nqp::stmts(
                (my int $i),
                nqp::if(
                  $wrap,
                  nqp::stmts(
                    (my int $sum = nqp::atpos_i(self,0)),
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      $sum = nqp::add_i($sum,nqp::atpos_i(self,$i))
                    ),
                    $sum
                  ),
                  nqp::stmts(
                    (my Int $Sum = nqp::atpos_i(self,0)),
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                      $Sum = $Sum + nqp::atpos_i(self,$i)
                    ),
                    $Sum
                  )
                )
              ),
              0
            )
        }
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
                nqp::setelems(self,0),  # make sure we start from scratch
                ($val = nqp::sub_i($val,1)),
                nqp::while(
                  nqp::isle_i(($val = nqp::add_i($val,1)),$max),
                  nqp::push_i(self,$val)
                ),
                self
              ),
              X::AdHoc.new( payload => "Can only initialize an int array with an int Range" ).throw
            )
        }
    }

    my role numarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of numarray role -----------------------------------
#- Generated on 2019-08-12T21:36:23+02:00 by tools/build/makeNATIVE_ARRAY.p6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method AT-POS(numarray:D: int $idx --> num) is raw {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::atposref_n(self,$idx)
        }
        multi method AT-POS(numarray:D: Int:D $idx --> num) is raw {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::atposref_n(self,$idx)
        }

        multi method ASSIGN-POS(numarray:D: int $idx, num $value --> num) {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: Int:D $idx, num $value --> num) {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: int $idx, Num:D $value --> num) {
            nqp::islt_i($idx,0)
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: Int:D $idx, Num:D $value --> num) {
            $idx < 0
              ?? INDEX_OUT_OF_RANGE($idx)
              !! nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(numarray:D: Any $idx, Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => "assignment to num array element #$idx",
                got       => value,
                expected  => T,
            ).throw;
        }

        multi method STORE(numarray:D: $value --> numarray:D) {
            nqp::setelems(self,1);
            nqp::bindpos_n(self, 0, nqp::unbox_n($value));
            self
        }
        multi method STORE(numarray:D: numarray:D \values --> numarray:D) {
            nqp::setelems(self,nqp::elems(values));
            nqp::splice(self,values,0,nqp::elems(values))
        }
        multi method STORE(numarray:D: Seq:D \seq --> numarray:D) {
            nqp::if(
              (my $iterator := seq.iterator).is-lazy,
              X::Cannot::Lazy.new(
                :action<store>, :what(self.^name)
              ).throw,
              nqp::stmts(
                nqp::setelems(self,0),
                $iterator.push-all(self),
                self
              )
            )
        }
        multi method STORE(numarray:D: List:D \values --> numarray:D) {
            my int $elems = values.elems;    # reifies
            my \reified := nqp::getattr(values,List,'$!reified');
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_n(self,$i,
                nqp::if(
                  nqp::isnull(nqp::atpos(reified,$i)),
                  0e0,
                  nqp::unbox_n(nqp::atpos(reified,$i))
                )
              )
            );
            self
        }
        multi method STORE(numarray:D: @values --> numarray:D) {
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

        multi method push(numarray:D: num $value --> numarray:D) {
            nqp::push_n(self, $value);
            self
        }
        multi method push(numarray:D: Num:D $value --> numarray:D) {
            nqp::push_n(self, $value);
            self
        }
        multi method push(numarray:D: Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => 'push to num array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method append(numarray:D: num $value --> numarray:D) {
            nqp::push_n(self, $value);
            self
        }
        multi method append(numarray:D: Num:D $value --> numarray:D) {
            nqp::push_n(self, $value);
            self
        }
        multi method append(numarray:D: numarray:D $values --> numarray:D) is default {
            nqp::splice(self,$values,nqp::elems(self),0)
        }
        multi method append(numarray:D: @values --> numarray:D) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_n(self, $_) for flat @values;
            self
        }

        method pop(numarray:D: --> num) {
            nqp::elems(self)
              ?? nqp::pop_n(self)
              !! X::Cannot::Empty.new(:action<pop>, :what(self.^name)).throw;
        }

        method shift(numarray:D: --> num) {
            nqp::elems(self)
              ?? nqp::shift_n(self)
              !! X::Cannot::Empty.new(:action<shift>, :what(self.^name)).throw;
        }

        multi method unshift(numarray:D: num $value --> numarray:D) {
            nqp::unshift_n(self, $value);
            self
        }
        multi method unshift(numarray:D: Num:D $value --> numarray:D) {
            nqp::unshift_n(self, $value);
            self
        }
        multi method unshift(numarray:D: @values --> numarray:D) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_n(self, @values.pop) while @values;
            self
        }
        multi method unshift(numarray:D: Mu \value --> Nil) {
            X::TypeCheck.new(
                operation => 'unshift to num array',
                got       => value,
                expected  => T,
            ).throw;
        }

        my $empty_n := nqp::list_n;

        multi method splice(numarray:D: --> numarray:D) {
            my $splice := nqp::clone(self);
            nqp::setelems(self,0);
            $splice
        }
        multi method splice(numarray:D: Int:D \offset --> numarray:D) {
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
        multi method splice(numarray:D: Int:D $offset, Int:D $size --> numarray:D) {
            nqp::unless(
              nqp::istype(
                (my $slice := CLONE_SLICE(self,$offset,$size)),
                Failure
              ),
              nqp::splice(self,$empty_n,$offset,$size)
            );
            $slice
        }
        multi method splice(numarray:D: Int:D $offset, Int:D $size, numarray:D \values --> numarray:D) {
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
        multi method splice(numarray:D: Int:D $offset, Int:D $size, Seq:D \seq --> numarray:D) {
            nqp::if(
              seq.is-lazy,
              X::Cannot::Lazy.new(:action<splice>, :what(self.^name)).throw,
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
        multi method splice(numarray:D: $offset=0, $size=Whatever, *@values --> numarray:D) {
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
        multi method minmax(numarray:D: --> Range:D) {
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

        my class Iterate does Iterator {
            has int $!i;
            has $!array;    # Native array we're iterating

            method !SET-SELF(\array) {
                $!array := nqp::decont(array);
                $!i = -1;
                self
            }
            method new(\array) { nqp::create(self)!SET-SELF(array) }

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
            method push-all(\target --> IterationEnd) {
                my int $i     = $!i;
                my int $elems = nqp::elems($!array);
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  target.push(nqp::atposref_n($!array,$i))
                );
                $!i = $i;
            }
        }
        method iterator(numarray:D: --> Iterate:D) { Iterate.new(self) }

        method reverse(numarray:D: --> numarray:D) is nodal {
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
        method rotate(numarray:D: Int(Cool) $rotate = 1 --> numarray:D) is nodal {
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
        multi method sort(numarray:D: --> numarray:D) {
            Rakudo::Sorting.MERGESORT-num(nqp::clone(self))
        }

        multi method ACCEPTS(numarray:D: numarray:D \other --> Bool:D) {
            nqp::hllbool(
              nqp::unless(
                nqp::eqaddr(self,other),
                nqp::if(
                  nqp::iseq_i(
                    (my int $elems = nqp::elems(self)),
                    nqp::elems(other)
                  ),
                  nqp::stmts(
                    (my int $i = -1),
                    nqp::while(
                      nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                        && nqp::iseq_n(
                             nqp::atpos_n(self,$i),
                             nqp::atpos_n(other,$i)
                           ),
                      nqp::null
                    ),
                    nqp::iseq_i($i,$elems)
                  )
                )
              )
            )
        }
        proto method grab(|) {*}
        multi method grab(numarray:D: --> num) {
            nqp::if(nqp::elems(self),self.GRAB_ONE,Nil)
        }
        multi method grab(numarray:D: Callable:D $calculate --> num) {
            self.grab($calculate(nqp::elems(self)))
        }
        multi method grab(numarray:D: Whatever --> Seq:D) { self.grab(Inf) }

        my class GrabN does Iterator {
            has $!array;
            has int $!count;

            method !SET-SELF(\array,\count) {
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
            method new(\a,\c) { nqp::create(self)!SET-SELF(a,c) }
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
        }
        multi method grab(numarray:D: \count --> Seq:D) {
            Seq.new(nqp::if(
              nqp::elems(self),
              GrabN.new(self,count),
              Rakudo::Iterator.Empty
            ))
        }

        method GRAB_ONE(numarray:D: --> num) {
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
              $odims.List
            )
        }

        multi method EXISTS-POS(::?CLASS:D: **@indices) {
            nqp::hllbool(
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
#- Generated on 2018-12-29T21:01:14+01:00 by tools/build/makeNATIVE_SHAPED_ARRAY.p6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    role shapedintarray does shapedarray {
        multi method AT-POS(::?CLASS:D: **@indices --> int) is raw {
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
                nqp::multidimref_i(self,$idxs)
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

        multi method ASSIGN-POS(::?CLASS:D: **@indices --> int) {
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

        my class NATCPY-int does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := from),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_i($!list,$!indices,
                  nqp::multidimref_i($!from,$!indices))
            }
        }
        sub NATCPY(Mu \to, Mu \from) is raw {
            NATCPY-int.new(to,from).sink-all;
            to
        }

        my class OBJCPY-int does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := nqp::getattr(from,List,'$!reified')),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_i($!list,$!indices,
                  nqp::atposnd($!from,$!indices))
            }
        }
        sub OBJCPY(Mu \to, Mu \from) is raw {
            OBJCPY-int.new(to,from).sink-all;
            to
        }

        my class ITERCPY-int does Rakudo::Iterator::ShapeBranch {
            has $!iterators;
            method !INIT(\to,\from) {
                nqp::stmts(
                  self!SET-SELF(to),
                  ($!iterators := nqp::setelems(
                    nqp::list(from.iterator),
                    nqp::add_i($!maxdim,1)
                  )),
                  self
                )
            }
            method new(\to,\from) { nqp::create(self)!INIT(to,from) }
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
                      nqp::eqaddr((my \item :=      # exhausted ?
                        nqp::atpos($!iterators,nqp::sub_i($i,1)).pull-one),
                        IterationEnd
                      ),
                      nqp::bindpos($!iterators,$i,  # add an empty one
                        Rakudo::Iterator.Empty),
                      nqp::if(                      # is it an iterator?
                        nqp::istype(item,Iterable) && nqp::isconcrete(item),
                        nqp::bindpos($!iterators,$i,item.iterator),
                        X::Assignment::ToShaped.new(shape => $!dims).throw
                      )
                    )
                  ),
                  (my \iter := nqp::atpos($!iterators,$!maxdim)),
                  nqp::until(                       # loop over highest dim
                    nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
                      || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
                    nqp::stmts(
                      nqp::bindposnd_i($!list,$!indices,pulled),
                      nqp::bindpos_i($!indices,$!maxdim,  # increment index
                        nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
                    )
                  ),
                  nqp::unless(
                    nqp::eqaddr(pulled,IterationEnd) # if not exhausted
                      || nqp::isle_i(                 # and index too high
                           nqp::atpos_i($!indices,$!maxdim),$!maxind)
                      || iter.is-lazy,                # and not lazy
                    nqp::atposnd_i($!list,$!indices)  # boom!
                  )
                )
            }
        }
        sub ITERCPY(Mu \to, Mu \from) is raw {
            ITERCPY-int.new(to,from).sink-all;
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

        my class Iterate-int does Rakudo::Iterator::ShapeLeaf {
            method result() is raw {
                nqp::multidimref_i($!list,nqp::clone($!indices))
            }
        }
        method iterator(::?CLASS:D: --> Iterate-int:D) {
            Iterate-int.new(self)
        }

        my class KV-int does Rakudo::Iterator::ShapeLeaf {
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
                  nqp::multidimref_i($!list,nqp::clone($!indices))
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
        multi method kv(::?CLASS:D: --> Seq:D) { Seq.new(KV-int.new(self)) }

        my class Pairs-int does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(
                  self.indices,
                  nqp::multidimref_i($!list,nqp::clone($!indices))
                )
            }
        }
        multi method pairs(::?CLASS:D: --> Seq:D) { Seq.new(Pairs-int.new(self)) }

        my class Antipairs-int does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(nqp::atposnd_i($!list,$!indices),self.indices)
            }
        }
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Antipairs-int.new(self))
        }
    }  # end of shapedintarray role

    role shaped1intarray does shapedintarray {
        multi method AT-POS(::?CLASS:D: int \one --> int) is raw {
           nqp::atposref_i(self,one)
        }
        multi method AT-POS(::?CLASS:D: Int:D \one --> int) is raw {
           nqp::atposref_i(self,one)
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \value --> int) {
            nqp::bindpos_i(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, int \value --> int) {
            nqp::bindpos_i(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int \one, Int:D \value --> int) {
            nqp::bindpos_i(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \value --> int) {
            nqp::bindpos_i(self,one,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one --> Bool:D) {
            nqp::hllbool(
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
                nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
                  || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_i(self,$i,pulled)
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

        my class Iterate-int does PredictiveIterator {
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
            method skip-one() {
                nqp::islt_i(($!pos = nqp::add_i($!pos,1)),nqp::elems($!list))
            }
            method push-all(\target --> IterationEnd) {
                nqp::stmts(
                  (my int $elems = nqp::elems($!list)),
                  (my int $pos = $!pos),
                  nqp::while(
                    nqp::islt_i(($pos = nqp::add_i($pos,1)),$elems),
                    target.push(nqp::atpos_i($!list,$pos))
                  ),
                  ($!pos = $pos)
                )
            }
            method count-only(--> Int:D) {
                nqp::p6box_i(
                  nqp::elems($!list)
                    - $!pos
                    - nqp::islt_i($!pos,nqp::elems($!list))
                )
            }
            method sink-all(--> IterationEnd) {
                $!pos = nqp::elems($!list)
            }
        }
        method iterator(::?CLASS:D: --> Iterate-int:D) {
            Iterate-int.new(self)
        }

        multi method kv(::?CLASS:D: --> Seq:D) {
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
        multi method pairs(::?CLASS:D: --> Seq:D) {
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
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }
        method reverse(::?CLASS:D: --> ::?CLASS:D) is nodal {
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
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1 --> ::?CLASS:D) is nodal {
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
        multi method AT-POS(::?CLASS:D: int \one, int \two --> int) is raw {
            nqp::multidimref_i(self,nqp::list_i(one, two))
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two --> int) is raw {
            nqp::multidimref_i(self,nqp::list_i(one, two))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, Int:D \value --> int) {
            nqp::bindpos2d_i(self,one,two,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \value --> int) {
            nqp::bindpos2d_i(self,one,two,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
    } # end of shaped2intarray role

    role shaped3intarray does shapedintarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three --> int) is raw {
            nqp::multidimref_i(self,nqp::list_i(one, two, three))
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three --> int) is raw {
            nqp::multidimref_i(self,nqp::list_i(one, two, three))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, Int:D \value --> int) {
            nqp::bindpos3d_i(self,one,two,three,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, Int:D \value --> int) {
            nqp::bindpos3d_i(self,one,two,three,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three --> Bool:D) {
            nqp::hllbool(
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
#- Generated on 2018-12-29T21:01:14+01:00 by tools/build/makeNATIVE_SHAPED_ARRAY.p6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    role shapednumarray does shapedarray {
        multi method AT-POS(::?CLASS:D: **@indices --> num) is raw {
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
                nqp::multidimref_n(self,$idxs)
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

        multi method ASSIGN-POS(::?CLASS:D: **@indices --> num) {
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

        my class NATCPY-num does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := from),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_n($!list,$!indices,
                  nqp::multidimref_n($!from,$!indices))
            }
        }
        sub NATCPY(Mu \to, Mu \from) is raw {
            NATCPY-num.new(to,from).sink-all;
            to
        }

        my class OBJCPY-num does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := nqp::getattr(from,List,'$!reified')),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_n($!list,$!indices,
                  nqp::atposnd($!from,$!indices))
            }
        }
        sub OBJCPY(Mu \to, Mu \from) is raw {
            OBJCPY-num.new(to,from).sink-all;
            to
        }

        my class ITERCPY-num does Rakudo::Iterator::ShapeBranch {
            has $!iterators;
            method !INIT(\to,\from) {
                nqp::stmts(
                  self!SET-SELF(to),
                  ($!iterators := nqp::setelems(
                    nqp::list(from.iterator),
                    nqp::add_i($!maxdim,1)
                  )),
                  self
                )
            }
            method new(\to,\from) { nqp::create(self)!INIT(to,from) }
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
                      nqp::eqaddr((my \item :=      # exhausted ?
                        nqp::atpos($!iterators,nqp::sub_i($i,1)).pull-one),
                        IterationEnd
                      ),
                      nqp::bindpos($!iterators,$i,  # add an empty one
                        Rakudo::Iterator.Empty),
                      nqp::if(                      # is it an iterator?
                        nqp::istype(item,Iterable) && nqp::isconcrete(item),
                        nqp::bindpos($!iterators,$i,item.iterator),
                        X::Assignment::ToShaped.new(shape => $!dims).throw
                      )
                    )
                  ),
                  (my \iter := nqp::atpos($!iterators,$!maxdim)),
                  nqp::until(                       # loop over highest dim
                    nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
                      || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
                    nqp::stmts(
                      nqp::bindposnd_n($!list,$!indices,pulled),
                      nqp::bindpos_i($!indices,$!maxdim,  # increment index
                        nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
                    )
                  ),
                  nqp::unless(
                    nqp::eqaddr(pulled,IterationEnd) # if not exhausted
                      || nqp::isle_i(                 # and index too high
                           nqp::atpos_i($!indices,$!maxdim),$!maxind)
                      || iter.is-lazy,                # and not lazy
                    nqp::atposnd_n($!list,$!indices)  # boom!
                  )
                )
            }
        }
        sub ITERCPY(Mu \to, Mu \from) is raw {
            ITERCPY-num.new(to,from).sink-all;
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

        my class Iterate-num does Rakudo::Iterator::ShapeLeaf {
            method result() is raw {
                nqp::multidimref_n($!list,nqp::clone($!indices))
            }
        }
        method iterator(::?CLASS:D: --> Iterate-num:D) {
            Iterate-num.new(self)
        }

        my class KV-num does Rakudo::Iterator::ShapeLeaf {
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
                  nqp::multidimref_n($!list,nqp::clone($!indices))
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
        multi method kv(::?CLASS:D: --> Seq:D) { Seq.new(KV-num.new(self)) }

        my class Pairs-num does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(
                  self.indices,
                  nqp::multidimref_n($!list,nqp::clone($!indices))
                )
            }
        }
        multi method pairs(::?CLASS:D: --> Seq:D) { Seq.new(Pairs-num.new(self)) }

        my class Antipairs-num does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(nqp::atposnd_n($!list,$!indices),self.indices)
            }
        }
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Antipairs-num.new(self))
        }
    }  # end of shapednumarray role

    role shaped1numarray does shapednumarray {
        multi method AT-POS(::?CLASS:D: int \one --> num) is raw {
           nqp::atposref_n(self,one)
        }
        multi method AT-POS(::?CLASS:D: Int:D \one --> num) is raw {
           nqp::atposref_n(self,one)
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, num \value --> num) {
            nqp::bindpos_n(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, num \value --> num) {
            nqp::bindpos_n(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int \one, Num:D \value --> num) {
            nqp::bindpos_n(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Num:D \value --> num) {
            nqp::bindpos_n(self,one,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one --> Bool:D) {
            nqp::hllbool(
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
                  nqp::bindpos_n(self,$i,nqp::atpos_i(from,$i))
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
                nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
                  || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_n(self,$i,pulled)
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

        my class Iterate-num does PredictiveIterator {
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
            method skip-one() {
                nqp::islt_i(($!pos = nqp::add_i($!pos,1)),nqp::elems($!list))
            }
            method push-all(\target --> IterationEnd) {
                nqp::stmts(
                  (my int $elems = nqp::elems($!list)),
                  (my int $pos = $!pos),
                  nqp::while(
                    nqp::islt_i(($pos = nqp::add_i($pos,1)),$elems),
                    target.push(nqp::atpos_n($!list,$pos))
                  ),
                  ($!pos = $pos)
                )
            }
            method count-only(--> Int:D) {
                nqp::p6box_i(
                  nqp::elems($!list)
                    - $!pos
                    - nqp::islt_i($!pos,nqp::elems($!list))
                )
            }
            method sink-all(--> IterationEnd) {
                $!pos = nqp::elems($!list)
            }
        }
        method iterator(::?CLASS:D: --> Iterate-num:D) {
            Iterate-num.new(self)
        }

        multi method kv(::?CLASS:D: --> Seq:D) {
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
        multi method pairs(::?CLASS:D: --> Seq:D) {
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
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }
        method reverse(::?CLASS:D: --> ::?CLASS:D) is nodal {
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
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1 --> ::?CLASS:D) is nodal {
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
        multi method AT-POS(::?CLASS:D: int \one, int \two --> num) is raw {
            nqp::multidimref_n(self,nqp::list_i(one, two))
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two --> num) is raw {
            nqp::multidimref_n(self,nqp::list_i(one, two))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, Num:D \value --> num) {
            nqp::bindpos2d_n(self,one,two,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Num:D \value --> num) {
            nqp::bindpos2d_n(self,one,two,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
    } # end of shaped2numarray role

    role shaped3numarray does shapednumarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three --> num) is raw {
            nqp::multidimref_n(self,nqp::list_i(one, two, three))
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three --> num) is raw {
            nqp::multidimref_n(self,nqp::list_i(one, two, three))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, Num:D \value --> num) {
            nqp::bindpos3d_n(self,one,two,three,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, Num:D \value --> num) {
            nqp::bindpos3d_n(self,one,two,three,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three --> Bool:D) {
            nqp::hllbool(
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
#- Generated on 2018-12-29T21:01:14+01:00 by tools/build/makeNATIVE_SHAPED_ARRAY.p6
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    role shapedstrarray does shapedarray {
        multi method AT-POS(::?CLASS:D: **@indices --> str) is raw {
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
                nqp::multidimref_s(self,$idxs)
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

        multi method ASSIGN-POS(::?CLASS:D: **@indices --> str) {
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

        my class NATCPY-str does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := from),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_s($!list,$!indices,
                  nqp::multidimref_s($!from,$!indices))
            }
        }
        sub NATCPY(Mu \to, Mu \from) is raw {
            NATCPY-str.new(to,from).sink-all;
            to
        }

        my class OBJCPY-str does Rakudo::Iterator::ShapeLeaf {
            has Mu $!from;
            method !INIT(Mu \to, Mu \from) {
                nqp::stmts(
                  ($!from := nqp::getattr(from,List,'$!reified')),
                  self!SET-SELF(to)
                )
            }
            method new(Mu \to, Mu \from) { nqp::create(self)!INIT(to,from) }
            method result(--> Nil) {
                nqp::bindposnd_s($!list,$!indices,
                  nqp::atposnd($!from,$!indices))
            }
        }
        sub OBJCPY(Mu \to, Mu \from) is raw {
            OBJCPY-str.new(to,from).sink-all;
            to
        }

        my class ITERCPY-str does Rakudo::Iterator::ShapeBranch {
            has $!iterators;
            method !INIT(\to,\from) {
                nqp::stmts(
                  self!SET-SELF(to),
                  ($!iterators := nqp::setelems(
                    nqp::list(from.iterator),
                    nqp::add_i($!maxdim,1)
                  )),
                  self
                )
            }
            method new(\to,\from) { nqp::create(self)!INIT(to,from) }
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
                      nqp::eqaddr((my \item :=      # exhausted ?
                        nqp::atpos($!iterators,nqp::sub_i($i,1)).pull-one),
                        IterationEnd
                      ),
                      nqp::bindpos($!iterators,$i,  # add an empty one
                        Rakudo::Iterator.Empty),
                      nqp::if(                      # is it an iterator?
                        nqp::istype(item,Iterable) && nqp::isconcrete(item),
                        nqp::bindpos($!iterators,$i,item.iterator),
                        X::Assignment::ToShaped.new(shape => $!dims).throw
                      )
                    )
                  ),
                  (my \iter := nqp::atpos($!iterators,$!maxdim)),
                  nqp::until(                       # loop over highest dim
                    nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
                      || nqp::isgt_i(nqp::atpos_i($!indices,$!maxdim),$!maxind),
                    nqp::stmts(
                      nqp::bindposnd_s($!list,$!indices,pulled),
                      nqp::bindpos_i($!indices,$!maxdim,  # increment index
                        nqp::add_i(nqp::atpos_i($!indices,$!maxdim),1))
                    )
                  ),
                  nqp::unless(
                    nqp::eqaddr(pulled,IterationEnd) # if not exhausted
                      || nqp::isle_i(                 # and index too high
                           nqp::atpos_i($!indices,$!maxdim),$!maxind)
                      || iter.is-lazy,                # and not lazy
                    nqp::atposnd_s($!list,$!indices)  # boom!
                  )
                )
            }
        }
        sub ITERCPY(Mu \to, Mu \from) is raw {
            ITERCPY-str.new(to,from).sink-all;
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

        my class Iterate-str does Rakudo::Iterator::ShapeLeaf {
            method result() is raw {
                nqp::multidimref_s($!list,nqp::clone($!indices))
            }
        }
        method iterator(::?CLASS:D: --> Iterate-str:D) {
            Iterate-str.new(self)
        }

        my class KV-str does Rakudo::Iterator::ShapeLeaf {
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
                  nqp::multidimref_s($!list,nqp::clone($!indices))
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
        multi method kv(::?CLASS:D: --> Seq:D) { Seq.new(KV-str.new(self)) }

        my class Pairs-str does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(
                  self.indices,
                  nqp::multidimref_s($!list,nqp::clone($!indices))
                )
            }
        }
        multi method pairs(::?CLASS:D: --> Seq:D) { Seq.new(Pairs-str.new(self)) }

        my class Antipairs-str does Rakudo::Iterator::ShapeLeaf {
            method result() {
                Pair.new(nqp::atposnd_s($!list,$!indices),self.indices)
            }
        }
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Antipairs-str.new(self))
        }
    }  # end of shapedstrarray role

    role shaped1strarray does shapedstrarray {
        multi method AT-POS(::?CLASS:D: int \one --> str) is raw {
           nqp::atposref_s(self,one)
        }
        multi method AT-POS(::?CLASS:D: Int:D \one --> str) is raw {
           nqp::atposref_s(self,one)
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, str \value --> str) {
            nqp::bindpos_s(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, str \value --> str) {
            nqp::bindpos_s(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: int \one, Str:D \value --> str) {
            nqp::bindpos_s(self,one,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Str:D \value --> str) {
            nqp::bindpos_s(self,one,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0) && nqp::islt_i(one,nqp::elems(self))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one --> Bool:D) {
            nqp::hllbool(
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
                  nqp::bindpos_s(self,$i,nqp::atpos_i(from,$i))
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
                nqp::eqaddr((my \pulled := iter.pull-one),IterationEnd)
                  || nqp::iseq_i(($i = nqp::add_i($i,1)),$elems),
                nqp::bindpos_s(self,$i,pulled)
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

        my class Iterate-str does PredictiveIterator {
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
            method skip-one() {
                nqp::islt_i(($!pos = nqp::add_i($!pos,1)),nqp::elems($!list))
            }
            method push-all(\target --> IterationEnd) {
                nqp::stmts(
                  (my int $elems = nqp::elems($!list)),
                  (my int $pos = $!pos),
                  nqp::while(
                    nqp::islt_i(($pos = nqp::add_i($pos,1)),$elems),
                    target.push(nqp::atpos_s($!list,$pos))
                  ),
                  ($!pos = $pos)
                )
            }
            method count-only(--> Int:D) {
                nqp::p6box_i(
                  nqp::elems($!list)
                    - $!pos
                    - nqp::islt_i($!pos,nqp::elems($!list))
                )
            }
            method sink-all(--> IterationEnd) {
                $!pos = nqp::elems($!list)
            }
        }
        method iterator(::?CLASS:D: --> Iterate-str:D) {
            Iterate-str.new(self)
        }

        multi method kv(::?CLASS:D: --> Seq:D) {
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
        multi method pairs(::?CLASS:D: --> Seq:D) {
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
        multi method antipairs(::?CLASS:D: --> Seq:D) {
            Seq.new(Rakudo::Iterator.AntiPair(self.iterator))
        }
        method reverse(::?CLASS:D: --> ::?CLASS:D) is nodal {
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
        method rotate(::?CLASS:D: Int(Cool) $rotate = 1 --> ::?CLASS:D) is nodal {
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
        multi method AT-POS(::?CLASS:D: int \one, int \two --> str) is raw {
            nqp::multidimref_s(self,nqp::list_i(one, two))
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two --> str) is raw {
            nqp::multidimref_s(self,nqp::list_i(one, two))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, Str:D \value --> str) {
            nqp::bindpos2d_s(self,one,two,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Str:D \value --> str) {
            nqp::bindpos2d_s(self,one,two,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
            )
        }
    } # end of shaped2strarray role

    role shaped3strarray does shapedstrarray {
        multi method AT-POS(::?CLASS:D: int \one, int \two, int \three --> str) is raw {
            nqp::multidimref_s(self,nqp::list_i(one, two, three))
        }
        multi method AT-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three --> str) is raw {
            nqp::multidimref_s(self,nqp::list_i(one, two, three))
        }

        multi method ASSIGN-POS(::?CLASS:D: int \one, int \two, int \three, Str:D \value --> str) {
            nqp::bindpos3d_s(self,one,two,three,value)
        }
        multi method ASSIGN-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three, Str:D \value --> str) {
            nqp::bindpos3d_s(self,one,two,three,value)
        }

        multi method EXISTS-POS(::?CLASS:D: int \one, int \two, int \three --> Bool:D) {
            nqp::hllbool(
              nqp::isge_i(one,0)
                && nqp::isge_i(two,0)
                && nqp::isge_i(three,0)
                && nqp::islt_i(one,nqp::atpos_i(nqp::dimensions(self),0))
                && nqp::islt_i(two,nqp::atpos_i(nqp::dimensions(self),1))
                && nqp::islt_i(three,nqp::atpos_i(nqp::dimensions(self),2))
            )
        }
        multi method EXISTS-POS(::?CLASS:D: Int:D \one, Int:D \two, Int:D \three --> Bool:D) {
            nqp::hllbool(
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

    method ^parameterize(Mu:U \arr, Mu \t) {
        if nqp::isconcrete(t) {
            return "Can not parameterize {arr.^name} with {t.perl}";
        }
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
#?if js
        elsif $kind == 4 || $kind == 5 {
            $what := arr.^mixin(intarray[$t]);
        }
#?endif
        else {
            return "Can only parameterize array with a native type, not {t.^name}";
        }

        $what.^set_name("{arr.^name}[{t.^name}]");
        $what
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

    multi method Bool(array:D:)    { nqp::hllbool(nqp::elems(self)) }
    multi method Numeric(array:D:) { nqp::elems(self) }
    multi method Str(array:D:)     { self.join(' ') }

    multi method elems(array:D:)    { nqp::elems(self) }
    method shape() { (*,) }
    proto method Real(|) {*}
    multi method Real(array:D:)     { nqp::elems(self) }
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
