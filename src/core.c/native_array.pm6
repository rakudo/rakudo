my class X::Delete { ... }
my class X::MustBeParametric { ... }
my class X::TooManyDimensions { ... }
my class X::TypeCheck::Assignment { ... }

my class array does Iterable does Positional {

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
            ?? self.set-shape($shape)
            !! Metamodel::EnumHOW.ACCEPTS($shape.HOW)
              ?? self.set-shape($shape.^elems)
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

    role strarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of strarray role -----------------------------------
#- Generated on 2021-04-01T19:23:33+02:00 by ./tools/build/makeNATIVE_ARRAY.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method grep(strarray:D: Str:D $needle, :$k, :$kv, :$p, :$v --> Seq:D) {
            my int $i     = -1;
            my int $elems = nqp::elems(self);
            my $result   := nqp::create(IterationBuffer);

            if $k {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_s(nqp::atpos_s(self,$i),$needle),
                    nqp::push($result,nqp::clone($i))
                  )
                );
            }
            elsif $kv {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_s(nqp::atpos_s(self,$i),$needle),
                    nqp::stmts(
                      nqp::push($result,nqp::clone($i)),
                      nqp::push($result,$needle)
                    )
                  )
                );
            }
            elsif $p {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_s(nqp::atpos_s(self,$i),$needle),
                    nqp::push($result,Pair.new($i,$needle))
                  )
                );
            }
            else {
                my int $found;
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_s(nqp::atpos_s(self,$i),$needle),
                    nqp::push($result,$needle)
                  )
                );
            }
            $result.Seq
        }

        multi method first(strarray:D: Str:D $needle, :$k, :$kv, :$p, :$v) {
            my int $i     = -1;
            my int $elems = nqp::elems(self);

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::isne_s(nqp::atpos_s(self,$i),$needle),
              nqp::null()
            );

            nqp::iseq_i($i,nqp::elems(self))
              ?? Nil
              !! $k
                ?? $i
                !! $kv
                  ?? ($i,$needle)
                  !! $p
                    ?? Pair.new($i,$needle)
                    !! $needle
        }

#        multi method unique(strarray:D: --> Seq:D) {
#            my int $i     = -1;
#            my int $elems = nqp::elems(self);
#            my $result := nqp::create(self);
#            my $seen   := nqp::hash;
#
#            nqp::while(
#              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#              nqp::unless(
#                nqp::existskey($seen,nqp::atpos_s(self,$i)),
#                nqp::stmts(
#                  nqp::bindkey($seen,nqp::atpos_s(self,$i),1),
#                  nqp::push_s($result,nqp::atpos_s(self,$i))
#                )
#              )
#            );
#
#            $result.Seq
#        }
#
#        multi method repeated(strarray:D: --> Seq:D) {
#            my int $i     = -1;
#            my int $elems = nqp::elems(self);
#            my $result := nqp::create(self);
#            my $seen   := nqp::hash;
#
#            nqp::while(
#              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#              nqp::if(
#                nqp::existskey($seen,nqp::atpos_s(self,$i)),
#                nqp::push_s($result,nqp::atpos_s(self,$i)),
#                nqp::bindkey($seen,nqp::atpos_s(self,$i),1)
#              )
#            );
#
#            $result.Seq
#        }
#
#        multi method squish(strarray:D: --> Seq:D) {
#            if nqp::elems(self) -> int $elems {
#                my $result  := nqp::create(self);
#                my str $last = nqp::push_s($result,nqp::atpos_s(self,0));
#                my int $i;
#
#                nqp::while(
#                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#                  nqp::if(
#                    nqp::isne_s(nqp::atpos_s(self,$i),$last),
#                    nqp::push_s($result,$last = nqp::atpos_s(self,$i))
#                  )
#                );
#                $result.Seq
#            }
#            else {
#                self.Seq
#            }
#        }

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
              self.throw-iterator-cannot-be-lazy('store'),
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
            return self.fail-iterator-cannot-be-lazy('.append')
              if @values.is-lazy;
            nqp::push_s(self, $_) for flat @values;
            self
        }

        method pop(strarray:D: --> str) {
            nqp::elems(self)
              ?? nqp::pop_s(self)
              !! self.throw-cannot-be-empty('pop')
        }

        method shift(strarray:D: --> str) {
            nqp::elems(self)
              ?? nqp::shift_s(self)
              !! self.throw-cannot-be-empty('shift')
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
            return self.fail-iterator-cannot-be-lazy('.unshift')
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
              self.throw-iterator-cannot-be-lazy('.splice'),
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
            return self.fail-iterator-cannot-be-lazy('splice in')
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
        method iterator(strarray:D: --> PredictiveIterator:D) {
            Rakudo::Iterator.native_s(self)
        }
        method Seq(strarray:D: --> Seq:D) {
            Seq.new(Rakudo::Iterator.native_s(self))
        }

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

        multi method ACCEPTS(strarray:D: strarray:D \o --> Bool:D) {
            nqp::hllbool(
              nqp::unless(
                nqp::eqaddr(self,my \other := nqp::decont(o)),
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
            nqp::elems(self) ?? self.GRAB_ONE !! Nil
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
            method is-deterministic(--> False) { }
        }
        multi method grab(strarray:D: \count --> Seq:D) {
            Seq.new(
              nqp::elems(self)
                ?? GrabN.new(self,count)
                !! Rakudo::Iterator.Empty
            )
        }

        method GRAB_ONE(strarray:D: --> str) is implementation-detail {
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
        method raku(strarray:D: --> Str:D) {
            my $parts := nqp::list_s;
            my int $i  = -1;

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems(self)),
              nqp::push_s($parts,nqp::if(
                nqp::isnull_s(my $str := nqp::atpos_s(self,$i)),
                '""',
                $str.raku
              ))
            );

            nqp::concat('array[',
              nqp::concat(T.^name,
                nqp::concat('].new(',
                  nqp::concat(nqp::join(', ',$parts),')')
                )
              )
            )
        }
    }

    role intarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of intarray role -----------------------------------
#- Generated on 2021-04-01T19:23:33+02:00 by ./tools/build/makeNATIVE_ARRAY.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method grep(intarray:D: Int:D $needle, :$k, :$kv, :$p, :$v --> Seq:D) {
            my int $i     = -1;
            my int $elems = nqp::elems(self);
            my $result   := nqp::create(IterationBuffer);

            if $k {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_i(nqp::atpos_i(self,$i),$needle),
                    nqp::push($result,nqp::clone($i))
                  )
                );
            }
            elsif $kv {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_i(nqp::atpos_i(self,$i),$needle),
                    nqp::stmts(
                      nqp::push($result,nqp::clone($i)),
                      nqp::push($result,$needle)
                    )
                  )
                );
            }
            elsif $p {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_i(nqp::atpos_i(self,$i),$needle),
                    nqp::push($result,Pair.new($i,$needle))
                  )
                );
            }
            else {
                my int $found;
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_i(nqp::atpos_i(self,$i),$needle),
                    nqp::push($result,$needle)
                  )
                );
            }
            $result.Seq
        }

        multi method first(intarray:D: Int:D $needle, :$k, :$kv, :$p, :$v) {
            my int $i     = -1;
            my int $elems = nqp::elems(self);

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::isne_i(nqp::atpos_i(self,$i),$needle),
              nqp::null()
            );

            nqp::iseq_i($i,nqp::elems(self))
              ?? Nil
              !! $k
                ?? $i
                !! $kv
                  ?? ($i,$needle)
                  !! $p
                    ?? Pair.new($i,$needle)
                    !! $needle
        }

#        multi method unique(intarray:D: --> Seq:D) {
#            my int $i     = -1;
#            my int $elems = nqp::elems(self);
#            my $result := nqp::create(self);
#            my $seen   := nqp::hash;
#
#            nqp::while(
#              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#              nqp::unless(
#                nqp::existskey($seen,nqp::atpos_i(self,$i)),
#                nqp::stmts(
#                  nqp::bindkey($seen,nqp::atpos_i(self,$i),1),
#                  nqp::push_i($result,nqp::atpos_i(self,$i))
#                )
#              )
#            );
#
#            $result.Seq
#        }
#
#        multi method repeated(intarray:D: --> Seq:D) {
#            my int $i     = -1;
#            my int $elems = nqp::elems(self);
#            my $result := nqp::create(self);
#            my $seen   := nqp::hash;
#
#            nqp::while(
#              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#              nqp::if(
#                nqp::existskey($seen,nqp::atpos_i(self,$i)),
#                nqp::push_i($result,nqp::atpos_i(self,$i)),
#                nqp::bindkey($seen,nqp::atpos_i(self,$i),1)
#              )
#            );
#
#            $result.Seq
#        }
#
#        multi method squish(intarray:D: --> Seq:D) {
#            if nqp::elems(self) -> int $elems {
#                my $result  := nqp::create(self);
#                my int $last = nqp::push_i($result,nqp::atpos_i(self,0));
#                my int $i;
#
#                nqp::while(
#                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#                  nqp::if(
#                    nqp::isne_i(nqp::atpos_i(self,$i),$last),
#                    nqp::push_i($result,$last = nqp::atpos_i(self,$i))
#                  )
#                );
#                $result.Seq
#            }
#            else {
#                self.Seq
#            }
#        }

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
              self.throw-iterator-cannot-be-lazy('store'),
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
            return self.fail-iterator-cannot-be-lazy('.append')
              if @values.is-lazy;
            nqp::push_i(self, $_) for flat @values;
            self
        }

        method pop(intarray:D: --> int) {
            nqp::elems(self)
              ?? nqp::pop_i(self)
              !! self.throw-cannot-be-empty('pop')
        }

        method shift(intarray:D: --> int) {
            nqp::elems(self)
              ?? nqp::shift_i(self)
              !! self.throw-cannot-be-empty('shift')
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
            return self.fail-iterator-cannot-be-lazy('.unshift')
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
              self.throw-iterator-cannot-be-lazy('.splice'),
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
            return self.fail-iterator-cannot-be-lazy('splice in')
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
        method iterator(intarray:D: --> PredictiveIterator:D) {
            Rakudo::Iterator.native_i(self)
        }
        method Seq(intarray:D: --> Seq:D) {
            Seq.new(Rakudo::Iterator.native_i(self))
        }

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

        multi method ACCEPTS(intarray:D: intarray:D \o --> Bool:D) {
            nqp::hllbool(
              nqp::unless(
                nqp::eqaddr(self,my \other := nqp::decont(o)),
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
            nqp::elems(self) ?? self.GRAB_ONE !! Nil
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
            method is-deterministic(--> False) { }
        }
        multi method grab(intarray:D: \count --> Seq:D) {
            Seq.new(
              nqp::elems(self)
                ?? GrabN.new(self,count)
                !! Rakudo::Iterator.Empty
            )
        }

        method GRAB_ONE(intarray:D: --> int) is implementation-detail {
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

        multi method chrs(intarray:D: --> Str:D) {
            my int $i = -1;
            my int $elems = nqp::elems(self);
            my $result   := nqp::setelems(nqp::list_s,$elems);
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos_s($result,$i,nqp::chr(nqp::atpos_i(self,$i)))
            );
            nqp::join("",$result)
        }

        multi method sum(intarray:D: :$wrap) {
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

    role numarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of numarray role -----------------------------------
#- Generated on 2021-04-01T19:23:33+02:00 by ./tools/build/makeNATIVE_ARRAY.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

        multi method grep(numarray:D: Num:D $needle, :$k, :$kv, :$p, :$v --> Seq:D) {
            my int $i     = -1;
            my int $elems = nqp::elems(self);
            my $result   := nqp::create(IterationBuffer);

            if $k {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_n(nqp::atpos_n(self,$i),$needle),
                    nqp::push($result,nqp::clone($i))
                  )
                );
            }
            elsif $kv {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_n(nqp::atpos_n(self,$i),$needle),
                    nqp::stmts(
                      nqp::push($result,nqp::clone($i)),
                      nqp::push($result,$needle)
                    )
                  )
                );
            }
            elsif $p {
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_n(nqp::atpos_n(self,$i),$needle),
                    nqp::push($result,Pair.new($i,$needle))
                  )
                );
            }
            else {
                my int $found;
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::iseq_n(nqp::atpos_n(self,$i),$needle),
                    nqp::push($result,$needle)
                  )
                );
            }
            $result.Seq
        }

        multi method first(numarray:D: Num:D $needle, :$k, :$kv, :$p, :$v) {
            my int $i     = -1;
            my int $elems = nqp::elems(self);

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                && nqp::isne_n(nqp::atpos_n(self,$i),$needle),
              nqp::null()
            );

            nqp::iseq_i($i,nqp::elems(self))
              ?? Nil
              !! $k
                ?? $i
                !! $kv
                  ?? ($i,$needle)
                  !! $p
                    ?? Pair.new($i,$needle)
                    !! $needle
        }

#        multi method unique(numarray:D: --> Seq:D) {
#            my int $i     = -1;
#            my int $elems = nqp::elems(self);
#            my $result := nqp::create(self);
#            my $seen   := nqp::hash;
#
#            nqp::while(
#              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#              nqp::unless(
#                nqp::existskey($seen,nqp::atpos_n(self,$i)),
#                nqp::stmts(
#                  nqp::bindkey($seen,nqp::atpos_n(self,$i),1),
#                  nqp::push_n($result,nqp::atpos_n(self,$i))
#                )
#              )
#            );
#
#            $result.Seq
#        }
#
#        multi method repeated(numarray:D: --> Seq:D) {
#            my int $i     = -1;
#            my int $elems = nqp::elems(self);
#            my $result := nqp::create(self);
#            my $seen   := nqp::hash;
#
#            nqp::while(
#              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#              nqp::if(
#                nqp::existskey($seen,nqp::atpos_n(self,$i)),
#                nqp::push_n($result,nqp::atpos_n(self,$i)),
#                nqp::bindkey($seen,nqp::atpos_n(self,$i),1)
#              )
#            );
#
#            $result.Seq
#        }
#
#        multi method squish(numarray:D: --> Seq:D) {
#            if nqp::elems(self) -> int $elems {
#                my $result  := nqp::create(self);
#                my num $last = nqp::push_n($result,nqp::atpos_n(self,0));
#                my int $i;
#
#                nqp::while(
#                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
#                  nqp::if(
#                    nqp::isne_n(nqp::atpos_n(self,$i),$last),
#                    nqp::push_n($result,$last = nqp::atpos_n(self,$i))
#                  )
#                );
#                $result.Seq
#            }
#            else {
#                self.Seq
#            }
#        }

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
              self.throw-iterator-cannot-be-lazy('store'),
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
            return self.fail-iterator-cannot-be-lazy('.append')
              if @values.is-lazy;
            nqp::push_n(self, $_) for flat @values;
            self
        }

        method pop(numarray:D: --> num) {
            nqp::elems(self)
              ?? nqp::pop_n(self)
              !! self.throw-cannot-be-empty('pop')
        }

        method shift(numarray:D: --> num) {
            nqp::elems(self)
              ?? nqp::shift_n(self)
              !! self.throw-cannot-be-empty('shift')
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
            return self.fail-iterator-cannot-be-lazy('.unshift')
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
              self.throw-iterator-cannot-be-lazy('.splice'),
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
            return self.fail-iterator-cannot-be-lazy('splice in')
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
        method iterator(numarray:D: --> PredictiveIterator:D) {
            Rakudo::Iterator.native_n(self)
        }
        method Seq(numarray:D: --> Seq:D) {
            Seq.new(Rakudo::Iterator.native_n(self))
        }

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

        multi method ACCEPTS(numarray:D: numarray:D \o --> Bool:D) {
            nqp::hllbool(
              nqp::unless(
                nqp::eqaddr(self,my \other := nqp::decont(o)),
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
            nqp::elems(self) ?? self.GRAB_ONE !! Nil
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
            method is-deterministic(--> False) { }
        }
        multi method grab(numarray:D: \count --> Seq:D) {
            Seq.new(
              nqp::elems(self)
                ?? GrabN.new(self,count)
                !! Rakudo::Iterator.Empty
            )
        }

        method GRAB_ONE(numarray:D: --> num) is implementation-detail {
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

        multi method sum(numarray:D:) {
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
        method BIND-POS(|) {
            X::Bind.new(target => 'a natively typed shaped array').throw
        }
        method DELETE-POS(|) {
            X::Delete.new(target => 'a natively typed shaped array').throw
        }

        method shape() {
            my $idims := nqp::dimensions(self);
            my $odims := nqp::create(IterationBuffer);
            nqp::while(
              nqp::elems($idims),
              nqp::push($odims,nqp::shift_i($idims))
            );
            $odims.List
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
#- Generated on 2020-12-08T18:47:05+01:00 by tools/build/makeNATIVE_SHAPED_ARRAY.raku
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
            EQV_DIMENSIONS(self,from)
              ?? NATCPY(self,from)
              !! X::Assignment::ArrayShapeMismatch.new(
                   source-shape => from.shape,
                   target-shape => self.shape
                 ).throw
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
            my \iter := Rakudo::Iterator.TailWith(in.iterator,0);
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems(self)),
              nqp::bindpos_i(self,$i,iter.pull-one)
            );
            # too many values? then throw by just accessing out of range
            nqp::atpos_i(list,$i) unless iter.exhausted;
            self
        }
        multi method STORE(::?CLASS:D: Int:D \item) {
            nqp::bindpos_i(self,0,item);
            self
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
                nqp::islt_i(($!pos = nqp::add_i($!pos,1)),nqp::elems($!list))
                  ?? nqp::atposref_i($!list,$!pos)
                  !! IterationEnd
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
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                  ?? Pair.new($i,nqp::atposref_i(self,$i))
                  !! IterationEnd
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
#- Generated on 2020-12-08T18:47:05+01:00 by tools/build/makeNATIVE_SHAPED_ARRAY.raku
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
            EQV_DIMENSIONS(self,from)
              ?? NATCPY(self,from)
              !! X::Assignment::ArrayShapeMismatch.new(
                   source-shape => from.shape,
                   target-shape => self.shape
                 ).throw
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
            my \iter := Rakudo::Iterator.TailWith(in.iterator,0e0);
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems(self)),
              nqp::bindpos_n(self,$i,iter.pull-one)
            );
            # too many values? then throw by just accessing out of range
            nqp::atpos_i(list,$i) unless iter.exhausted;
            self
        }
        multi method STORE(::?CLASS:D: Num:D \item) {
            nqp::bindpos_n(self,0,item);
            self
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
                nqp::islt_i(($!pos = nqp::add_i($!pos,1)),nqp::elems($!list))
                  ?? nqp::atposref_n($!list,$!pos)
                  !! IterationEnd
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
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                  ?? Pair.new($i,nqp::atposref_n(self,$i))
                  !! IterationEnd
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
#- Generated on 2020-12-08T18:47:05+01:00 by tools/build/makeNATIVE_SHAPED_ARRAY.raku
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
            EQV_DIMENSIONS(self,from)
              ?? NATCPY(self,from)
              !! X::Assignment::ArrayShapeMismatch.new(
                   source-shape => from.shape,
                   target-shape => self.shape
                 ).throw
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
            my \iter := Rakudo::Iterator.TailWith(in.iterator,"");
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems(self)),
              nqp::bindpos_s(self,$i,iter.pull-one)
            );
            # too many values? then throw by just accessing out of range
            nqp::atpos_i(list,$i) unless iter.exhausted;
            self
        }
        multi method STORE(::?CLASS:D: Str:D \item) {
            nqp::bindpos_s(self,0,item);
            self
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
                nqp::islt_i(($!pos = nqp::add_i($!pos,1)),nqp::elems($!list))
                  ?? nqp::atposref_s($!list,$!pos)
                  !! IterationEnd
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
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                  ?? Pair.new($i,nqp::atposref_s(self,$i))
                  !! IterationEnd
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
            return "Can not parameterize {arr.^name} with {t.raku}";
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
        elsif $dims {
            # Calculate new meta-object (probably hitting caches in most cases).
            my \shaped-type = self.WHAT.^mixin(
              nqp::atpos(
                nqp::atpos(typedim2role,nqp::objprimspec(my \T = self.of)),
                nqp::isle_i($dims,3) && $dims
              )
            );
            shaped-type.^set_name(self.WHAT.^name)        # set name if needed
              if nqp::isne_s(shaped-type.^name,self.WHAT.^name);

            # Allocate array storage for this shape, based on calculated type.
            Rakudo::Internals.SHAPED-ARRAY-STORAGE(shape,shaped-type.HOW,T)
        }
        else {
            X::NotEnoughDimensions.new(
              operation         => 'create',
              got-dimensions    => $dims,
              needed-dimensions => '',
            ).throw
        }
    }

    method BIND-POS(|) {
        X::Bind.new(target => 'a natively typed array').throw
    }
    method DELETE-POS(|) {
        X::Delete.new(target => 'a natively typed array').throw
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

    multi method raku(array:D:) {
        'array[' ~ self.of.raku ~ '].new(' ~
            self.map(*.raku).join(', ') ~ ')'
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
        postcircumfix:<[ ]>(SELF, range.list)
      )
    )
}

#- start of postcircumfix candidates of strarray -------------------------------
#- Generated on 2021-04-03T16:18:57+02:00 by tools/build/makeNATIVE_CANDIDATES.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos, Str:D \assignee
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! nqp::bindpos_s(nqp::decont(SELF),$pos,assignee)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D, Int:D, :$BIND!
) {
    X::Bind.new(target => 'a native str array').throw
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos, :$exists!, *%_
) {
    my int $state =
      nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)));
    my $value := nqp::hllbool($exists ?? $state !! nqp::not_i($state));

    $state
      ?? nqp::elems(my $adverbs := nqp::getattr(%_,Map,'$!storage'))
        ?? nqp::atkey($adverbs,'kv')
          ?? ($pos,$value)
          !! nqp::atkey($adverbs,'p')
            ?? Pair.new($pos,$value)
            !! Failure.new(
                 X::Adverb.new(
                   what   => "slice",
                   source => "a native str array",
                   nogo   => ('exists', |%_.keys).sort
                 )
               )
        !! $value
      !! $value
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos, :$delete!, *%_
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $delete
        ?? X::Delete.new(target => 'a native str array').throw
        !! nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? postcircumfix:<[ ]>(SELF, $pos, |%_)
          !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos, :$kv!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $kv
        ?? nqp::list($pos,nqp::atpos_s(nqp::decont(SELF),$pos))
        !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos, :$p!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $p
        ?? Pair.new($pos,nqp::atpos_s(nqp::decont(SELF),$pos))
        !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos, :$k!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $k
        ?? $pos
        !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Int:D $pos, :$v!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $v
        ?? nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)))
          ?? nqp::list(nqp::atpos_s(nqp::decont(SELF),$pos))
          !! ()
        !! nqp::atpos_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Callable:D $pos
) is raw {
    nqp::istype((my $got := $pos.POSITIONS(SELF)),Int)
      ?? nqp::islt_i($got,0)
        ?? X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw
        !! nqp::atposref_s(nqp::decont(SELF),$got)
      !! postcircumfix:<[ ]>(SELF, $got)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Iterable:D $pos is rw
) is raw {
    nqp::islt_i((my int $got = $pos.Int),0)
      ?? X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw
      !! nqp::atposref_s(nqp::decont(SELF),$got)
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Iterable:D $pos
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
#?if jvm
    my @result := array[str].new;
#?endif
#?if !jvm
    my str @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_s(@result,nqp::atpos_s($self,$got)),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when slicing a native str array".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Iterable:D $pos, array::strarray:D $values
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my int $i    = -1;
#?if jvm
    my @result := array[str].new;
#?endif
#?if !jvm
    my str @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_s(
          @result,
          nqp::bindpos_s(
            $self,
            $got,
            nqp::atpos_s($values,$i = nqp::add_i($i,1))
          )
        ),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when assigning to a native str array slice".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Iterable:D $pos, \values
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my $values  := Rakudo::Iterator.TailWith(values.iterator,'');
#?if jvm
    my @result := array[str].new;
#?endif
#?if !jvm
    my str @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_s(
          @result,
          nqp::bindpos_s(
            $self,
            $got,
            $values.pull-one.Str
          )
        ),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when assigning to a native str array slice".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::strarray:D \SELF, Whatever
) {
    nqp::decont(SELF)
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of postcircumfix candidates of strarray ---------------------------------

#- start of postcircumfix candidates of numarray -------------------------------
#- Generated on 2021-04-03T16:18:57+02:00 by tools/build/makeNATIVE_CANDIDATES.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos, Num:D \assignee
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! nqp::bindpos_n(nqp::decont(SELF),$pos,assignee)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D, Int:D, :$BIND!
) {
    X::Bind.new(target => 'a native num array').throw
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos, :$exists!, *%_
) {
    my int $state =
      nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)));
    my $value := nqp::hllbool($exists ?? $state !! nqp::not_i($state));

    $state
      ?? nqp::elems(my $adverbs := nqp::getattr(%_,Map,'$!storage'))
        ?? nqp::atkey($adverbs,'kv')
          ?? ($pos,$value)
          !! nqp::atkey($adverbs,'p')
            ?? Pair.new($pos,$value)
            !! Failure.new(
                 X::Adverb.new(
                   what   => "slice",
                   source => "a native num array",
                   nogo   => ('exists', |%_.keys).sort
                 )
               )
        !! $value
      !! $value
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos, :$delete!, *%_
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $delete
        ?? X::Delete.new(target => 'a native num array').throw
        !! nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? postcircumfix:<[ ]>(SELF, $pos, |%_)
          !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos, :$kv!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $kv
        ?? nqp::list($pos,nqp::atpos_n(nqp::decont(SELF),$pos))
        !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos, :$p!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $p
        ?? Pair.new($pos,nqp::atpos_n(nqp::decont(SELF),$pos))
        !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos, :$k!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $k
        ?? $pos
        !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Int:D $pos, :$v!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $v
        ?? nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)))
          ?? nqp::list(nqp::atpos_n(nqp::decont(SELF),$pos))
          !! ()
        !! nqp::atpos_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Callable:D $pos
) is raw {
    nqp::istype((my $got := $pos.POSITIONS(SELF)),Int)
      ?? nqp::islt_i($got,0)
        ?? X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw
        !! nqp::atposref_n(nqp::decont(SELF),$got)
      !! postcircumfix:<[ ]>(SELF, $got)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Iterable:D $pos is rw
) is raw {
    nqp::islt_i((my int $got = $pos.Int),0)
      ?? X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw
      !! nqp::atposref_n(nqp::decont(SELF),$got)
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Iterable:D $pos
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
#?if jvm
    my @result := array[num].new;
#?endif
#?if !jvm
    my num @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_n(@result,nqp::atpos_n($self,$got)),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when slicing a native num array".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Iterable:D $pos, array::numarray:D $values
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my int $i    = -1;
#?if jvm
    my @result := array[num].new;
#?endif
#?if !jvm
    my num @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_n(
          @result,
          nqp::bindpos_n(
            $self,
            $got,
            nqp::atpos_n($values,$i = nqp::add_i($i,1))
          )
        ),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when assigning to a native num array slice".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Iterable:D $pos, \values
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my $values  := Rakudo::Iterator.TailWith(values.iterator,0e0);
#?if jvm
    my @result := array[num].new;
#?endif
#?if !jvm
    my num @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_n(
          @result,
          nqp::bindpos_n(
            $self,
            $got,
            $values.pull-one.Num
          )
        ),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when assigning to a native num array slice".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::numarray:D \SELF, Whatever
) {
    nqp::decont(SELF)
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of postcircumfix candidates of numarray ---------------------------------

#- start of postcircumfix candidates of intarray -------------------------------
#- Generated on 2021-04-03T16:18:57+02:00 by tools/build/makeNATIVE_CANDIDATES.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos, Int:D \assignee
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! nqp::bindpos_i(nqp::decont(SELF),$pos,assignee)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D, Int:D, :$BIND!
) {
    X::Bind.new(target => 'a native int array').throw
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos, :$exists!, *%_
) {
    my int $state =
      nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)));
    my $value := nqp::hllbool($exists ?? $state !! nqp::not_i($state));

    $state
      ?? nqp::elems(my $adverbs := nqp::getattr(%_,Map,'$!storage'))
        ?? nqp::atkey($adverbs,'kv')
          ?? ($pos,$value)
          !! nqp::atkey($adverbs,'p')
            ?? Pair.new($pos,$value)
            !! Failure.new(
                 X::Adverb.new(
                   what   => "slice",
                   source => "a native int array",
                   nogo   => ('exists', |%_.keys).sort
                 )
               )
        !! $value
      !! $value
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos, :$delete!, *%_
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $delete
        ?? X::Delete.new(target => 'a native int array').throw
        !! nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? postcircumfix:<[ ]>(SELF, $pos, |%_)
          !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos, :$kv!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $kv
        ?? nqp::list($pos,nqp::atpos_i(nqp::decont(SELF),$pos))
        !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos, :$p!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $p
        ?? Pair.new($pos,nqp::atpos_i(nqp::decont(SELF),$pos))
        !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos, :$k!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $k
        ?? $pos
        !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Int:D $pos, :$v!
) is raw {
    nqp::islt_i($pos,0)
      ?? X::OutOfRange.new(:what<Index>, :got($pos), :range<0..^Inf>).throw
      !! $v
        ?? nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)))
          ?? nqp::list(nqp::atpos_i(nqp::decont(SELF),$pos))
          !! ()
        !! nqp::atpos_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Callable:D $pos
) is raw {
    nqp::istype((my $got := $pos.POSITIONS(SELF)),Int)
      ?? nqp::islt_i($got,0)
        ?? X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw
        !! nqp::atposref_i(nqp::decont(SELF),$got)
      !! postcircumfix:<[ ]>(SELF, $got)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Iterable:D $pos is rw
) is raw {
    nqp::islt_i((my int $got = $pos.Int),0)
      ?? X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw
      !! nqp::atposref_i(nqp::decont(SELF),$got)
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Iterable:D $pos
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
#?if jvm
    my @result := array[int].new;
#?endif
#?if !jvm
    my int @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_i(@result,nqp::atpos_i($self,$got)),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when slicing a native int array".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Iterable:D $pos, array::intarray:D $values
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my int $i    = -1;
#?if jvm
    my @result := array[int].new;
#?endif
#?if !jvm
    my int @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_i(
          @result,
          nqp::bindpos_i(
            $self,
            $got,
            nqp::atpos_i($values,$i = nqp::add_i($i,1))
          )
        ),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when assigning to a native int array slice".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Iterable:D $pos, \values
) is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my $values  := Rakudo::Iterator.TailWith(values.iterator,0);
#?if jvm
    my @result := array[int].new;
#?endif
#?if !jvm
    my int @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::if(
        nqp::istype(
          (my $got := nqp::if(
            nqp::istype($pulled,Callable),
            $pulled.POSITIONS($self),
            $pulled
          )),
          Int
        ) && nqp::isge_i($got,0),
        nqp::push_i(
          @result,
          nqp::bindpos_i(
            $self,
            $got,
            $values.pull-one.Int
          )
        ),
        nqp::if(
          nqp::istype($got,Int),
          X::OutOfRange.new(:what<Index>, :$got, :range<0..^Inf>).throw,
          (die "Cannot handle {$got.raku} as an index in an Iterable when assigning to a native int array slice".naive-word-wrapper)
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::intarray:D \SELF, Whatever
) {
    nqp::decont(SELF)
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of postcircumfix candidates of intarray ---------------------------------

#- start of shaped1 postcircumfix candidates of strarray -----------------------
#- Generated on 2021-04-03T16:18:54+02:00 by tools/build/makeNATIVE_SHAPED1_CANDIDATES.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Int:D $pos
) is default is raw {
    nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Int:D $pos, Str:D \assignee
) is default is raw {
    nqp::bindpos_s(nqp::decont(SELF),$pos,assignee)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Int:D $pos, :$exists!, *%_
) is default {
    my int $state =
      nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)));
    my $value := nqp::hllbool($exists ?? $state !! nqp::not_i($state));

    $state
      ?? nqp::elems(my $adverbs := nqp::getattr(%_,Map,'$!storage'))
        ?? nqp::atkey($adverbs,'kv')
          ?? ($pos,$value)
          !! nqp::atkey($adverbs,'p')
            ?? Pair.new($pos,$value)
            !! Failure.new(
                 X::Adverb.new(
                   what   => "slice",
                   source => "native shaped1 str array",
                   nogo   => ('exists', |%_.keys).sort
                 )
               )
        !! $value
      !! $value
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Int:D $pos, :$kv!
) is default is raw {
    $kv
      ?? nqp::list($pos,nqp::atpos_s(nqp::decont(SELF),$pos))
      !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Int:D $pos, :$p!
) is default is raw {
    $p
      ?? Pair.new($pos,nqp::atpos_s(nqp::decont(SELF),$pos))
      !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Int:D $pos, :$k!
) is default is raw {
    $k ?? $pos !! nqp::atposref_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Int:D $pos, :$v!
) is default is raw {
    $v
      ?? nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)))
        ?? nqp::list(nqp::atpos_s(nqp::decont(SELF),$pos))
        !! ()
      !! nqp::atpos_s(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Iterable:D $pos is rw
) is default is raw {
    nqp::atposref_s(nqp::decont(SELF),$pos.Int)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Callable:D $pos
) is default is raw {
    nqp::atposref_s(
      nqp::decont(SELF),
      $pos(nqp::elems(nqp::decont(SELF)))
    )
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Iterable:D $pos
) is default is raw {
    my $self     := nqp::decont(SELF);
    my $iterator := $pos.iterator;
#?if jvm
    my @result := array[str].new;
#?endif
#?if !jvm
    my str @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
      nqp::push_s(
        @result,
        nqp::atpos_s(
          $self,
          nqp::if(
            nqp::istype($pulled,Callable),
            $pulled(nqp::elems($self)),
            $pulled.Int
          )
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::shaped1strarray:D \SELF, Iterable:D $pos, array::strarray:D $values
) is default is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my int $i    = -1;
#?if jvm
    my @result := array[str].new;
#?endif
#?if !jvm
    my str @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::push_s(
        @result,
        nqp::bindpos_s(
          $self,
          nqp::if(
            nqp::istype($pulled,Callable),
            $pulled(nqp::elems($self)),
            $pulled.Int
          ),
          nqp::atpos_s($values,$i = nqp::add_i($i,1))
        )
      )
    );

    @result
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of shaped1 postcircumfix candidates of strarray -------------------------

#- start of shaped1 postcircumfix candidates of intarray -----------------------
#- Generated on 2021-04-03T16:18:54+02:00 by tools/build/makeNATIVE_SHAPED1_CANDIDATES.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Int:D $pos
) is default is raw {
    nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Int:D $pos, Int:D \assignee
) is default is raw {
    nqp::bindpos_i(nqp::decont(SELF),$pos,assignee)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Int:D $pos, :$exists!, *%_
) is default {
    my int $state =
      nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)));
    my $value := nqp::hllbool($exists ?? $state !! nqp::not_i($state));

    $state
      ?? nqp::elems(my $adverbs := nqp::getattr(%_,Map,'$!storage'))
        ?? nqp::atkey($adverbs,'kv')
          ?? ($pos,$value)
          !! nqp::atkey($adverbs,'p')
            ?? Pair.new($pos,$value)
            !! Failure.new(
                 X::Adverb.new(
                   what   => "slice",
                   source => "native shaped1 int array",
                   nogo   => ('exists', |%_.keys).sort
                 )
               )
        !! $value
      !! $value
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Int:D $pos, :$kv!
) is default is raw {
    $kv
      ?? nqp::list($pos,nqp::atpos_i(nqp::decont(SELF),$pos))
      !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Int:D $pos, :$p!
) is default is raw {
    $p
      ?? Pair.new($pos,nqp::atpos_i(nqp::decont(SELF),$pos))
      !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Int:D $pos, :$k!
) is default is raw {
    $k ?? $pos !! nqp::atposref_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Int:D $pos, :$v!
) is default is raw {
    $v
      ?? nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)))
        ?? nqp::list(nqp::atpos_i(nqp::decont(SELF),$pos))
        !! ()
      !! nqp::atpos_i(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Iterable:D $pos is rw
) is default is raw {
    nqp::atposref_i(nqp::decont(SELF),$pos.Int)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Callable:D $pos
) is default is raw {
    nqp::atposref_i(
      nqp::decont(SELF),
      $pos(nqp::elems(nqp::decont(SELF)))
    )
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Iterable:D $pos
) is default is raw {
    my $self     := nqp::decont(SELF);
    my $iterator := $pos.iterator;
#?if jvm
    my @result := array[int].new;
#?endif
#?if !jvm
    my int @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
      nqp::push_i(
        @result,
        nqp::atpos_i(
          $self,
          nqp::if(
            nqp::istype($pulled,Callable),
            $pulled(nqp::elems($self)),
            $pulled.Int
          )
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::shaped1intarray:D \SELF, Iterable:D $pos, array::intarray:D $values
) is default is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my int $i    = -1;
#?if jvm
    my @result := array[int].new;
#?endif
#?if !jvm
    my int @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::push_i(
        @result,
        nqp::bindpos_i(
          $self,
          nqp::if(
            nqp::istype($pulled,Callable),
            $pulled(nqp::elems($self)),
            $pulled.Int
          ),
          nqp::atpos_i($values,$i = nqp::add_i($i,1))
        )
      )
    );

    @result
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of shaped1 postcircumfix candidates of intarray -------------------------

#- start of shaped1 postcircumfix candidates of numarray -----------------------
#- Generated on 2021-04-03T16:18:54+02:00 by tools/build/makeNATIVE_SHAPED1_CANDIDATES.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Int:D $pos
) is default is raw {
    nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Int:D $pos, Num:D \assignee
) is default is raw {
    nqp::bindpos_n(nqp::decont(SELF),$pos,assignee)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Int:D $pos, :$exists!, *%_
) is default {
    my int $state =
      nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)));
    my $value := nqp::hllbool($exists ?? $state !! nqp::not_i($state));

    $state
      ?? nqp::elems(my $adverbs := nqp::getattr(%_,Map,'$!storage'))
        ?? nqp::atkey($adverbs,'kv')
          ?? ($pos,$value)
          !! nqp::atkey($adverbs,'p')
            ?? Pair.new($pos,$value)
            !! Failure.new(
                 X::Adverb.new(
                   what   => "slice",
                   source => "native shaped1 num array",
                   nogo   => ('exists', |%_.keys).sort
                 )
               )
        !! $value
      !! $value
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Int:D $pos, :$kv!
) is default is raw {
    $kv
      ?? nqp::list($pos,nqp::atpos_n(nqp::decont(SELF),$pos))
      !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Int:D $pos, :$p!
) is default is raw {
    $p
      ?? Pair.new($pos,nqp::atpos_n(nqp::decont(SELF),$pos))
      !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Int:D $pos, :$k!
) is default is raw {
    $k ?? $pos !! nqp::atposref_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Int:D $pos, :$v!
) is default is raw {
    $v
      ?? nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::elems(nqp::decont(SELF)))
        ?? nqp::list(nqp::atpos_n(nqp::decont(SELF),$pos))
        !! ()
      !! nqp::atpos_n(nqp::decont(SELF),$pos)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Iterable:D $pos is rw
) is default is raw {
    nqp::atposref_n(nqp::decont(SELF),$pos.Int)
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Callable:D $pos
) is default is raw {
    nqp::atposref_n(
      nqp::decont(SELF),
      $pos(nqp::elems(nqp::decont(SELF)))
    )
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Iterable:D $pos
) is default is raw {
    my $self     := nqp::decont(SELF);
    my $iterator := $pos.iterator;
#?if jvm
    my @result := array[num].new;
#?endif
#?if !jvm
    my num @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
      nqp::push_n(
        @result,
        nqp::atpos_n(
          $self,
          nqp::if(
            nqp::istype($pulled,Callable),
            $pulled(nqp::elems($self)),
            $pulled.Int
          )
        )
      )
    );

    @result
}

multi sub postcircumfix:<[ ]>(
  array::shaped1numarray:D \SELF, Iterable:D $pos, array::numarray:D $values
) is default is raw {
    my $self    := nqp::decont(SELF);
    my $indices := $pos.iterator;
    my int $i    = -1;
#?if jvm
    my @result := array[num].new;
#?endif
#?if !jvm
    my num @result;
#?endif

    nqp::until(
      nqp::eqaddr((my $pulled := $indices.pull-one),IterationEnd),
      nqp::push_n(
        @result,
        nqp::bindpos_n(
          $self,
          nqp::if(
            nqp::istype($pulled,Callable),
            $pulled(nqp::elems($self)),
            $pulled.Int
          ),
          nqp::atpos_n($values,$i = nqp::add_i($i,1))
        )
      )
    );

    @result
}

#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of shaped1 postcircumfix candidates of numarray -------------------------

# vim: expandtab shiftwidth=4
