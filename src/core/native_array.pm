my class X::MustBeParametric  { ... }
my class X::TooManyDimensions { ... }

my class array does Iterable is repr('VMArray') {

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
            !! nqp::create(self)
    }

    proto method STORE(|) { * }
    multi method STORE(array:D: *@values) { self.STORE(@values) }

    multi method push(array:D:    **@values) { self.append(@values) }
    multi method append(array:D:   *@values) { self.append(@values) }
    multi method unshift(array:D: **@values) { self.unshift(@values) }
    multi method prepend(array:D:  *@values) { self.unshift(@values) }

    my role strarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of strarray role -----------------------------------
#- Generated on 2016-07-30T23:24:42+02:00 by tools/build/makeNATIVE_ARRAY.pl6
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
            nqp::bindpos_s(self, 0, nqp::unbox_s($value));
            self
        }
        multi method STORE(strarray:D: str @values) {
            nqp::splice(self,@values,0,0)
        }
        multi method STORE(strarray:D: @values) {
            my int $elems = @values.elems;
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::bindpos_s(self, $i,
              nqp::unbox_s(@values.AT-POS($i)))
              while nqp::islt_i($i = nqp::add_i($i,1),$elems);
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
        multi method append(strarray:D: str @values) {
            nqp::splice(self,@values,nqp::elems(self),0)
        }
        multi method append(strarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_s(self, $_) for flat @values;
            self
        }

        method pop(strarray:D:) returns str {
            nqp::elems(self) > 0
              ?? nqp::pop_s(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift(strarray:D:) returns str {
            nqp::elems(self) > 0
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

        multi method splice(strarray:D: $offset=0, $size=Whatever, *@values, :$SINK) {
            fail X::Cannot::Lazy.new(:action('splice in'))
              if @values.is-lazy;

            my $elems = self.elems;
            my int $o = nqp::istype($offset,Callable)
              ?? $offset($elems)
              !! nqp::istype($offset,Whatever)
                ?? $elems
                !! $offset.Int;
            X::OutOfRange.new(
              :what('Offset argument to splice'),
              :got($o),
              :range("0..$elems"),
            ).fail if $o < 0 || $o > $elems; # one after list allowed for "push"

            my int $s = nqp::istype($size,Callable)
              ?? $size($elems - $o)
              !! !defined($size) || nqp::istype($size,Whatever)
                 ?? $elems - ($o min $elems)
                 !! $size.Int;
            X::OutOfRange.new(
              :what('Size argument to splice'),
              :got($s),
              :range("0..^{$elems - $o}"),
            ).fail if $s < 0;

            if $SINK {
                my @splicees := nqp::create(self);
                nqp::push_s(@splicees, @values.shift) while @values;
                nqp::splice(self, @splicees, $o, $s);
                Nil;
            }

            else {
                my @ret := nqp::create(self);
                my int $i = $o;
                my int $n = ($elems min $o + $s) - 1;
                while $i <= $n {
                    nqp::push_s(@ret, nqp::atpos_s(self, $i));
                    $i = $i + 1;
                }

                my @splicees := nqp::create(self);
                nqp::push_s(@splicees, @values.shift) while @values;
                nqp::splice(self, @splicees, $o, $s);
                @ret;
            }
        }

        method min(strarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my str $min = nqp::atpos_s(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::cmp_s(
                      nqp::atpos_s(self,$i),$min),0),
                    ($min = nqp::atpos_s(self,$i))
                  )
                ),
                $min
              ),
              Inf
            )
        }
        method max(strarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my str $max = nqp::atpos_s(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::isgt_i(nqp::cmp_s(
                      nqp::atpos_s(self,$i),$max),0),
                    ($max = nqp::atpos_s(self,$i))
                  )
                ),
                $max
              ),
              -Inf
            )
        }
        method minmax(strarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my str $min =
                  my str $max = nqp::atpos_s(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::cmp_s(
                      nqp::atpos_s(self,$i),$min),0),
                    ($min = nqp::atpos_s(self,$i)),
                    nqp::if(
                      nqp::isgt_i(nqp::cmp_s(
                        nqp::atpos_s(self,$i),$max),0),
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
                method push-all($target) {
                    my int $i     = $!i;
                    my int $elems = nqp::elems($!array);
                    $target.push(nqp::atposref_s($!array,$i))
                      while ($i = $i + 1) < $elems;
                    $!i = $i;
                    IterationEnd
                }
            }.new(self)
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
#- Generated on 2016-07-30T23:24:42+02:00 by tools/build/makeNATIVE_ARRAY.pl6
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
            nqp::bindpos_i(self, 0, nqp::unbox_i($value));
            self
        }
        multi method STORE(intarray:D: int @values) {
            nqp::splice(self,@values,0,0)
        }
        multi method STORE(intarray:D: @values) {
            my int $elems = @values.elems;
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::bindpos_i(self, $i,
              nqp::unbox_i(@values.AT-POS($i)))
              while nqp::islt_i($i = nqp::add_i($i,1),$elems);
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
        multi method append(intarray:D: int @values) {
            nqp::splice(self,@values,nqp::elems(self),0)
        }
        multi method append(intarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_i(self, $_) for flat @values;
            self
        }

        method pop(intarray:D:) returns int {
            nqp::elems(self) > 0
              ?? nqp::pop_i(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift(intarray:D:) returns int {
            nqp::elems(self) > 0
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

        multi method splice(intarray:D: $offset=0, $size=Whatever, *@values, :$SINK) {
            fail X::Cannot::Lazy.new(:action('splice in'))
              if @values.is-lazy;

            my $elems = self.elems;
            my int $o = nqp::istype($offset,Callable)
              ?? $offset($elems)
              !! nqp::istype($offset,Whatever)
                ?? $elems
                !! $offset.Int;
            X::OutOfRange.new(
              :what('Offset argument to splice'),
              :got($o),
              :range("0..$elems"),
            ).fail if $o < 0 || $o > $elems; # one after list allowed for "push"

            my int $s = nqp::istype($size,Callable)
              ?? $size($elems - $o)
              !! !defined($size) || nqp::istype($size,Whatever)
                 ?? $elems - ($o min $elems)
                 !! $size.Int;
            X::OutOfRange.new(
              :what('Size argument to splice'),
              :got($s),
              :range("0..^{$elems - $o}"),
            ).fail if $s < 0;

            if $SINK {
                my @splicees := nqp::create(self);
                nqp::push_i(@splicees, @values.shift) while @values;
                nqp::splice(self, @splicees, $o, $s);
                Nil;
            }

            else {
                my @ret := nqp::create(self);
                my int $i = $o;
                my int $n = ($elems min $o + $s) - 1;
                while $i <= $n {
                    nqp::push_i(@ret, nqp::atpos_i(self, $i));
                    $i = $i + 1;
                }

                my @splicees := nqp::create(self);
                nqp::push_i(@splicees, @values.shift) while @values;
                nqp::splice(self, @splicees, $o, $s);
                @ret;
            }
        }

        method min(intarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my int $min = nqp::atpos_i(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::cmp_i(
                      nqp::atpos_i(self,$i),$min),0),
                    ($min = nqp::atpos_i(self,$i))
                  )
                ),
                $min
              ),
              Inf
            )
        }
        method max(intarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my int $max = nqp::atpos_i(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::isgt_i(nqp::cmp_i(
                      nqp::atpos_i(self,$i),$max),0),
                    ($max = nqp::atpos_i(self,$i))
                  )
                ),
                $max
              ),
              -Inf
            )
        }
        method minmax(intarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my int $min =
                  my int $max = nqp::atpos_i(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::cmp_i(
                      nqp::atpos_i(self,$i),$min),0),
                    ($min = nqp::atpos_i(self,$i)),
                    nqp::if(
                      nqp::isgt_i(nqp::cmp_i(
                        nqp::atpos_i(self,$i),$max),0),
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
                method push-all($target) {
                    my int $i     = $!i;
                    my int $elems = nqp::elems($!array);
                    $target.push(nqp::atposref_i($!array,$i))
                      while ($i = $i + 1) < $elems;
                    $!i = $i;
                    IterationEnd
                }
            }.new(self)
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

        multi method STORE(intarray:D: Range:D $range) {
            fail "Can only initialize an int array with an int Range"
              unless $range.is-int;

            my int $val = $range.min;
            $val = $val + 1 if $range.excludes-min;
            my int $max = $range.max;
            $max = $max - 1 if $range.excludes-max;
            nqp::setelems(self, $max - $val + 1);

            my int $i = -1;
            --$val;
            nqp::bindpos_i(self,++$i,$val) while nqp::isle_i(++$val,$max);

            self
        }
    }

    my role numarray[::T] does Positional[T] is array_type(T) {
#- start of generated part of numarray role -----------------------------------
#- Generated on 2016-07-30T23:24:42+02:00 by tools/build/makeNATIVE_ARRAY.pl6
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
            nqp::bindpos_n(self, 0, nqp::unbox_n($value));
            self
        }
        multi method STORE(numarray:D: num @values) {
            nqp::splice(self,@values,0,0)
        }
        multi method STORE(numarray:D: @values) {
            my int $elems = @values.elems;
            nqp::setelems(self, $elems);

            my int $i = -1;
            nqp::bindpos_n(self, $i,
              nqp::unbox_n(@values.AT-POS($i)))
              while nqp::islt_i($i = nqp::add_i($i,1),$elems);
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
        multi method append(numarray:D: num @values) {
            nqp::splice(self,@values,nqp::elems(self),0)
        }
        multi method append(numarray:D: @values) {
            fail X::Cannot::Lazy.new(:action<append>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_n(self, $_) for flat @values;
            self
        }

        method pop(numarray:D:) returns num {
            nqp::elems(self) > 0
              ?? nqp::pop_n(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift(numarray:D:) returns num {
            nqp::elems(self) > 0
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

        multi method splice(numarray:D: $offset=0, $size=Whatever, *@values, :$SINK) {
            fail X::Cannot::Lazy.new(:action('splice in'))
              if @values.is-lazy;

            my $elems = self.elems;
            my int $o = nqp::istype($offset,Callable)
              ?? $offset($elems)
              !! nqp::istype($offset,Whatever)
                ?? $elems
                !! $offset.Int;
            X::OutOfRange.new(
              :what('Offset argument to splice'),
              :got($o),
              :range("0..$elems"),
            ).fail if $o < 0 || $o > $elems; # one after list allowed for "push"

            my int $s = nqp::istype($size,Callable)
              ?? $size($elems - $o)
              !! !defined($size) || nqp::istype($size,Whatever)
                 ?? $elems - ($o min $elems)
                 !! $size.Int;
            X::OutOfRange.new(
              :what('Size argument to splice'),
              :got($s),
              :range("0..^{$elems - $o}"),
            ).fail if $s < 0;

            if $SINK {
                my @splicees := nqp::create(self);
                nqp::push_n(@splicees, @values.shift) while @values;
                nqp::splice(self, @splicees, $o, $s);
                Nil;
            }

            else {
                my @ret := nqp::create(self);
                my int $i = $o;
                my int $n = ($elems min $o + $s) - 1;
                while $i <= $n {
                    nqp::push_n(@ret, nqp::atpos_n(self, $i));
                    $i = $i + 1;
                }

                my @splicees := nqp::create(self);
                nqp::push_n(@splicees, @values.shift) while @values;
                nqp::splice(self, @splicees, $o, $s);
                @ret;
            }
        }

        method min(numarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my num $min = nqp::atpos_n(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::cmp_n(
                      nqp::atpos_n(self,$i),$min),0),
                    ($min = nqp::atpos_n(self,$i))
                  )
                ),
                $min
              ),
              Inf
            )
        }
        method max(numarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my num $max = nqp::atpos_n(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::isgt_i(nqp::cmp_n(
                      nqp::atpos_n(self,$i),$max),0),
                    ($max = nqp::atpos_n(self,$i))
                  )
                ),
                $max
              ),
              -Inf
            )
        }
        method minmax(numarray:D:) {
            nqp::if(
              (my int $elems = self.elems),
              nqp::stmts(
                (my int $i),
                (my num $min =
                  my num $max = nqp::atpos_n(self,0)),
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                  nqp::if(
                    nqp::islt_i(nqp::cmp_n(
                      nqp::atpos_n(self,$i),$min),0),
                    ($min = nqp::atpos_n(self,$i)),
                    nqp::if(
                      nqp::isgt_i(nqp::cmp_n(
                        nqp::atpos_n(self,$i),$max),0),
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
                method push-all($target) {
                    my int $i     = $!i;
                    my int $elems = nqp::elems($!array);
                    $target.push(nqp::atposref_n($!array,$i))
                      while ($i = $i + 1) < $elems;
                    $!i = $i;
                    IterationEnd
                }
            }.new(self)
        }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of numarray role -------------------------------------

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
            my Mu \idims = nqp::dimensions(self);
            my Mu \dims = nqp::list();
            loop (my int $i = 0; $i < nqp::elems(idims); $i = $i + 1) {
                nqp::bindpos(dims, $i, nqp::atpos_i(idims, $i))
            }
            nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', dims)
        }

        proto method EXISTS-POS(|) {*}
        multi method EXISTS-POS(array:U: |c) {
            self.Any::EXISTS-POS(|c)
        }
        multi method EXISTS-POS(array:D: **@indices) {
            my int $numdims = nqp::numdimensions(self);
            my int $numind  = @indices.elems;
            if $numind <= $numdims {
                my $dims := nqp::dimensions(self);
                loop (my int $i = 0; $i < $numind; $i = $i + 1) {
                    return False if @indices[$i] >= nqp::atpos_i($dims, $i);
                }
                True
            }
            else {
                False
            }
        }

        proto method STORE(|) { * }
        multi method STORE(::?CLASS:D: Iterable:D \in) {
            my \in-shape = nqp::can(in, 'shape') ?? in.shape !! Nil;
            if in-shape && !nqp::istype(in-shape.AT-POS(0), Whatever) {
                if self.shape eqv in-shape {
                    # Can do a VM-supported memcpy-like thing in the future
                    for self.keys {
                        self.ASSIGN-POS(|$_, in.AT-POS(|$_))
                    }
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

        method rotate(::?CLASS:D: Cool) {
            X::IllegalOnFixedDimensionArray.new(operation => 'rotate').throw
        }
    }

    role shapedintarray[::T] does shapedarray {
        proto method AT-POS(|) is raw {*}
        multi method AT-POS(array:U: |c) is raw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(array:D: **@indices) is raw {
            my int $numdims = nqp::numdimensions(self);
            my int $numind  = @indices.elems;
            if $numind == $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift);
                    $numdims = $numdims - 1;
                }
#?if moar
                nqp::multidimref_i(self, $idxs)
#?endif
#?if !moar
                nqp::atposnd_i(self, $idxs)
#?endif
            }
            elsif $numind > $numdims {
                X::TooManyDimensions.new(
                    operation => 'access',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
            else {
                X::NYI.new(feature => "Partially dimensioned views of arrays").throw
            }
        }

        proto method ASSIGN-POS(|) {*}
        multi method ASSIGN-POS(array:U: |c) {
            self.Any::ASSIGN-POS(|c)
        }
        multi method ASSIGN-POS(array:D: **@indices) {
            my int $value   = @indices.pop;
            my int $numdims = nqp::numdimensions(self);
            my int $numind  = @indices.elems;
            if $numind == $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift);
                    $numdims = $numdims - 1;
                }
                nqp::bindposnd_i(self, $idxs, $value)
            }
            elsif $numind > $numdims {
                X::TooManyDimensions.new(
                    operation => 'assign to',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
            else {
                X::NotEnoughDimensions.new(
                    operation => 'assign to',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
        }
    }

    role shapednumarray[::T] does shapedarray {
        proto method AT-POS(|) is raw {*}
        multi method AT-POS(array:U: |c) is raw {
            self.Any::AT-POS(|c)
        }
        multi method AT-POS(array:D: **@indices) is raw {
            my int $numdims = nqp::numdimensions(self);
            my int $numind  = @indices.elems;
            if $numind == $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift);
                    $numdims = $numdims - 1;
                }
#?if moar
                nqp::multidimref_n(self, $idxs)
#?endif
#?if !moar
                nqp::atposnd_n(self, $idxs)
#?endif
            }
            elsif $numind > $numdims {
                X::TooManyDimensions.new(
                    operation => 'access',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
            else {
                X::NYI.new(feature => "Partially dimensioned views of arrays").throw
            }
        }

        proto method ASSIGN-POS(|) {*}
        multi method ASSIGN-POS(array:U: |c) {
            self.Any::ASSIGN-POS(|c)
        }
        multi method ASSIGN-POS(array:D: **@indices) {
            my num $value   = @indices.pop;
            my int $numdims = nqp::numdimensions(self);
            my int $numind  = @indices.elems;
            if $numind == $numdims {
                my $idxs := nqp::list_i();
                while $numdims > 0 {
                    nqp::push_i($idxs, @indices.shift);
                    $numdims = $numdims - 1;
                }
                nqp::bindposnd_n(self, $idxs, $value)
            }
            elsif $numind > $numdims {
                X::TooManyDimensions.new(
                    operation => 'assign to',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
            else {
                X::NotEnoughDimensions.new(
                    operation => 'assign to',
                    got-dimensions => $numind,
                    needed-dimensions => $numdims
                ).throw
            }
        }
    }

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

    method !shaped($shape) {
        # Calculate new meta-object (probably hitting caches in most cases).
        my \T = self.of;
        my int $kind = nqp::objprimspec(T);
        my \shaped-type = self.WHAT.^mixin($kind == 1
            ?? shapedintarray[T]
            !! shapednumarray[T]);
        shaped-type.^set_name(self.^name());

        # Allocate array storage for this shape, based on the calculated type.
        Rakudo::Internals.SHAPED-ARRAY-STORAGE($shape.list, shaped-type.HOW, T)
    }

    method BIND-POS(|) {
        die "Cannot bind to a natively typed array";
    }
    method DELETE-POS(|) {
        die "Cannot delete from a natively typed array";
    }

    proto method ASSIGN-POS(|) { * } # Hide candidates from Any
    multi method ASSIGN-POS(Any:U \SELF: \pos, Mu \assignee) { # auto-viv
       SELF.AT-POS(pos) = assignee;
    }
    multi method ASSIGN-POS(Any:D: Any:U \pos, Mu \assignee) { # undefined idx
        die "Cannot use '{pos.^name}' as an index";
    }

    multi method EXISTS-POS(array:D: int $idx) {
        $idx >= 0 && $idx < nqp::elems(self)
    }
    multi method EXISTS-POS(array:D: Int $idx) {
        $idx >= 0 && $idx < nqp::elems(self)
    }

    multi method Bool(array:D:)    { nqp::p6bool(nqp::elems(self)) }
    multi method Numeric(array:D:) { nqp::elems(self) }
    multi method Str(array:D:)     { self.join(' ') }

    multi method elems(array:D:)    { nqp::elems(self) }
    method shape() { (*,) }
    proto method Int(|) { * }
    multi method Int(array:D:)      { nqp::elems(self) }
    multi method end(array:D:)      { nqp::elems(self) - 1 }
    method is-lazy(array:D:) { False }

    method eager() { self }
    method flat()  { Seq.new(self.iterator) }
    method list()  { List.from-iterator(self.iterator) }
    method sink(--> Nil) { }

    multi method gist(array:D:) {
        self.map(-> $elem {
            given ++$ {
                when 101 { '...' }
                when 102 { last }
                default  { $elem.gist }
            }
        } ).join: ' ';
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
}

# vim: ft=perl6 expandtab sw=4
