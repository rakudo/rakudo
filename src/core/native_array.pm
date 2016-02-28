my class X::TooManyDimensions { ... }

my class array does Iterable is repr('VMArray') {

    proto method STORE(|) { * }
    multi method STORE(array:D: *@values) { self.STORE(@values) }

    multi method push(array:D: *@values)  { self.append(@values) }
    multi method append(array:D: *@values)  { self.append(@values) }
    multi method unshift(array:D: *@values) { self.unshift(@values) }
    multi method prepend(array:D: *@values) { self.prepend(@values) }

    my role intarray[::T] does Positional[T] is array_type(T) {

        multi method AT-POS(array:D: int $idx) is raw {
            nqp::atposref_i(self, $idx)
        }
        multi method AT-POS(array:D: Int:D $idx) is raw {
            nqp::atposref_i(self, $idx)
        }

        multi method ASSIGN-POS(array:D: int $idx, int $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Int:D $idx, int $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Any $idx, int $value) {
            nqp::bindpos_i(self, $idx.Int, $value)
        }
        multi method ASSIGN-POS(array:D: int $idx, Int:D $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Int $idx, Int:D $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Any $idx, Mu \value) {
            X::TypeCheck.new(
                operation => 'assignment to array',
                got       => value,
                expected  => T,
            ).throw;
        }

        multi method STORE(array:D: $value) {
            nqp::setelems(self, 1);
            nqp::bindpos_i(self, 0, nqp::unbox_i($value));
            self
        }
        multi method STORE(array:D: @values) {
            my int $i = 0;
            my int $n = @values.elems;
            nqp::setelems(self, $n);
            while $i < $n {
                nqp::bindpos_i(self, $i, nqp::unbox_i(@values.AT-POS($i)));
                $i = $i + 1;
            }
            self
        }
        multi method STORE(array:D: Range:D $range) {
            my int $val = $range.min;
            $val = $val + 1 if $range.excludes-min;
            my int $max = $range.max;
            $max = $max - 1 if $range.excludes-max;
            nqp::setelems(self, $max - $val + 1);

            my int $i;
            while $val <= $max {
                nqp::bindpos_i(self, $i, $val);
                $val = $val + 1;
                $i   = $i   + 1;
            }
            self
        }

        multi method push(array:D: int $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method push(array:D: Int $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method push(array:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'push to array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method append(array:D: int $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method append(array:D: Int $value) {
            nqp::push_i(self, $value);
            self
        }
        multi method append(array:D: int @values) {
            nqp::splice(self,@values,nqp::elems(self),0)
        }
        multi method append(array:D: @values) {
            fail X::Cannot::Lazy.new(:action<push>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_i(self, $_) for flat @values;
            self
        }

        method pop(array:D:) returns int {
            nqp::elems(self) > 0
              ?? nqp::pop_i(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift() returns int {
            nqp::elems(self) > 0
              ?? nqp::shift_i(self)
              !! die X::Cannot::Empty.new(:action<shift>, :what(self.^name));
        }

        multi method unshift(array:D: int $value) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(array:D: Int $value) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(array:D: Mu \value) {
            X::TypeCheck.new(
                operation => 'push to array',
                got       => value,
                expected  => T,
            ).throw;
        }
        multi method unshift(array:D: @values) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_i(self, @values.pop) while @values;
            self
        }

        multi method splice(array:D: $offset=0, $size=Whatever, *@values, :$SINK) {
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
                method push-exactly($target, int $n) {
                    my int $elems = nqp::elems($!array);
                    my int $left  = $elems - $!i - 1;
                    if $n >= $left {
                        $target.push(nqp::atposref_i($!array,$!i))
                          while ($!i = $!i + 1) < $elems;
                        IterationEnd
                    }
                    else {
                        my int $end = $!i + 1 + $n;
                        $target.push(nqp::atposref_i($!array,$!i))
                          while ($!i = $!i + 1) < $end;
                        $!i = $!i - 1; # did one too many
                        $n
                    }
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
    }

    # please note that this role is mostly same as intarray but s/_i$/_n/
    my role numarray[::T] does Positional[T] is array_type(T) {
        multi method AT-POS(array:D: int $idx) is raw {
            nqp::atposref_n(self, $idx)
        }
        multi method AT-POS(array:D: Int $idx) is raw {
            nqp::atposref_n(self, $idx)
        }

        multi method ASSIGN-POS(array:D: int $idx, num $value) {
            nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Int:D $idx, num $value) {
            nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: int $idx, Num:D $value) {
            nqp::bindpos_n(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Int:D $idx, Mu \value) {
            nqp::bindpos_n(self, $idx, value)
        }
        multi method ASSIGN-POS(array:D: Any:D $idx, Mu \value) {
            nqp::bindpos_n(self, $idx.Int, value)
        }

        multi method STORE(array:D: $value) {
            nqp::setelems(self, 1);
            nqp::bindpos_n(self, 0, nqp::unbox_n($value));
            self
        }
        multi method STORE(array:D: @values) {
            my int $i = 0;
            my int $n = @values.elems;
            nqp::setelems(self, $n);
            while $i < $n {
                nqp::bindpos_n(self, $i, nqp::unbox_n(@values.AT-POS($i)));
                $i = $i + 1;
            }
            self
        }
        multi method STORE(array:D: Range:D $range) {
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

        multi method push(array:D: num $value) {
            nqp::push_n(self, $value);
            self
        }
        multi method push(array:D: Num $value) {
            nqp::push_n(self, $value);
            self
        }
        multi method append(array:D: @values) {
            fail X::Cannot::Lazy.new(:action<push>, :what(self.^name))
              if @values.is-lazy;
            nqp::push_n(self, $_) for flat @values;
            self
        }

        method pop(array:D:) returns num {
            nqp::elems(self) > 0
              ?? nqp::pop_n(self)
              !! die X::Cannot::Empty.new(:action<pop>, :what(self.^name));
        }

        method shift() returns num {
            nqp::elems(self) > 0
              ?? nqp::shift_n(self)
              !! die X::Cannot::Empty.new(:action<shift>, :what(self.^name));
        }

        multi method unshift(array:D: num $value) {
            nqp::unshift_n(self, $value);
            self
        }
        multi method unshift(array:D: Num $value) {
            nqp::unshift_n(self, $value);
            self
        }
        multi method unshift(array:D: @values) {
            fail X::Cannot::Lazy.new(:action<unshift>, :what(self.^name))
              if @values.is-lazy;
            nqp::unshift_n(self, @values.pop) while @values;
            self
        }

        multi method splice(array:D: $offset=0, $size=Whatever, *@values, :$SINK) {
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
                method push-exactly($target, int $n) {
                    my int $elems = nqp::elems($!array);
                    my int $left  = $elems - $!i - 1;
                    if $n >= $left {
                        $target.push(nqp::atposref_n($!array,$!i))
                          while ($!i = $!i + 1) < $elems;
                        IterationEnd
                    }
                    else {
                        my int $end = $!i + 1 + $n;
                        $target.push(nqp::atposref_n($!array,$!i))
                          while ($!i = $!i + 1) < $end;
                        $!i = $!i - 1; # did one too many
                        $n
                    }
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
        if $kind == 1 {
            my $what := arr.^mixin(intarray[$t]);
            $what.^set_name("{arr.^name}[{t.^name}]");
            $what;
        }
        elsif $kind == 2 {
            my $what := arr.^mixin(numarray[$t]);
            $what.^set_name("{arr.^name}[{t.^name}]");
            $what;
        }
        elsif $kind == 3 {
            X::NYI.new(feature => 'native string arrays').throw;
        }
        else {
            die "Can only parameterize array with a native type, not {t.^name}";
        }
    }

    multi method new(:$shape) {
        self!create($shape)
    }
    multi method new(@values, :$shape) {
        self!create($shape).STORE(@values)
    }
    multi method new(**@values, :$shape) {
        self!create($shape).STORE(@values)
    }

    method !create($shape) {
        nqp::isnull(nqp::typeparameterized(self)) &&
            die "Must parameterize array[T] with a type before creating it";
        nqp::isconcrete($shape) ?? self!shaped($shape) !! nqp::create(self)
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

# needs native arrays, so we can only define it here
sub permutations(int $n where $n > 0) {
    Seq.new(
        class :: does Iterator {
            # See:  L<https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order>
            has int $!n;
            has     @!a;
            submethod BUILD(:$n --> Nil) { $!n = $n } # cannot set native in sig
            #method is-lazy { True }
            method pull-one {
                return (@!a = ^$!n).List unless @!a;
                # Find the largest index k such that a[k] < a[k + 1].
                # If no such index exists, the permutation is the last permutation.
                my int $k = @!a.end - 1;
                $k-- or return IterationEnd until @!a[$k] < @!a[$k + 1];
                
                # Find the largest index l greater than k such that a[k] < a[l].
                my int $l = @!a.end;
                $l-- until @!a[$k] < @!a[$l];
                # use L<https://en.wikipedia.org/wiki/XOR_swap_algorithm>
                # @!a[$k, $l].=reverse
                (@!a[$k] +^= @!a[$l]) +^= @!a[$l] +^= @!a[$k];

                # @!a[$k+1 .. @!a.end].=reverse;
                $l = $!n;
                (@!a[$k] +^= @!a[$l]) +^= @!a[$l] +^= @!a[$k] until ++$k >= --$l;
                @!a.List;
            }
            method count-only { [*] 1 .. $!n }
        }.new(:$n)
    );
}
