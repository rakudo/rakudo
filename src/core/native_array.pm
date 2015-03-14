class array is Iterable is repr('VMArray') {

    proto method STORE(|) { * }
    multi method STORE(array:D: *@values) { self.STORE(@values) }

    multi method push(array:D: *@values)    { self.push(@values) }
    multi method unshift(array:D: *@values) { self.unshift(@values) }

    my role intarray[::T] does Positional[T] is array_type(T) {
        multi method AT-POS(array:D: int $idx) is rw {
            nqp::atposref_i(self, $idx)
        }
        multi method AT-POS(array:D: Int:D $idx) is rw {
            nqp::atposref_i(self, $idx)
        }

        multi method ASSIGN-POS(array:D: int $idx, int $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Int:D $idx, int $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: int $idx, Int:D $value) {
            nqp::bindpos_i(self, $idx, $value)
        }
        multi method ASSIGN-POS(array:D: Int:D $idx, Mu \value) {
            nqp::bindpos_i(self, $idx, value)
        }
        multi method ASSIGN-POS(array:D: Any:D $idx, Mu \value) {
            nqp::bindpos_i(self, $idx.Int, value)
        }

        multi method STORE(array:D: $value) {
            nqp::setelems(self, 0);
            nqp::bindpos_i(self, 0, nqp::unbox_i($value));
            self
        }
        multi method STORE(array:D: @values) {
            nqp::setelems(self, 0);
            my int $i = 0;
            my int $n = @values.elems;
            while $i < $n {
                nqp::bindpos_i(self, $i, nqp::unbox_i(@values.AT-POS($i)));
                $i = $i + 1;
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
        multi method push(array:D: @values) {
            fail 'Cannot .push an infinite list' if @values.infinite;
            nqp::push_i(self, $_) for @values;
            self
        }

        method pop(array:D:) returns int {
            nqp::elems(self) > 0
              ?? nqp::pop_i(self)
              !! die 'Element popped from empty array';
        }

        method shift() returns int {
            nqp::elems(self) > 0
              ?? nqp::shift_i(self)
              !! die 'Element shifted from empty array';
        }

        multi method unshift(array:D: int $value) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(array:D: Int $value) {
            nqp::unshift_i(self, $value);
            self
        }
        multi method unshift(array:D: @values) {
            fail 'Cannot .unshift an infinite list' if @values.infinite;
            nqp::unshift_i(self, @values.pop) while @values;
            self
        }

        method splice($offset = 0, $size?, *@values) {
            my $o = $offset;
            my $s = $size;
            my $elems = self.elems;
            $o = $o($elems) if nqp::istype($o, Callable);
            X::OutOfRange.new(
                what => 'offset argument to array.splice',
                got  => $offset,
                range => (0..^$elems),
            ).fail if $o < 0;
            $s //= $elems - ($o min $elems);
            $s = $s($elems - $o) if nqp::istype($s, Callable);
            X::OutOfRange.new(
                what => 'size argument to array.splice',
                got  => $size,
                range => (0..^($elems - $o)),
            ).fail if $s < 0;

            my @ret := nqp::create(self);
            my int $i = $o;
            my int $n = $o + $s - 1;
            while $i <= $n {
                nqp::push_i(@ret, nqp::atpos_i(self, $i));
                $i = $i + 1;
            }

            my @splicees := nqp::create(self);
            nqp::push_i(@splicees, @values.shift) while @values;
            nqp::splice(self, @splicees, $o.Int, $s.Int);
            @ret;
        }

        my class NativeIntArrayIter is Iterator {
            has $!array;    # Native array we're iterating
            has $!reified;  # Parcel of native array refs we return after reifying
            has int $!idx;  # Starting index of this iterator

            method new($array) {
                my \iter = nqp::create(self);
                iter.BUILD(:$array);
                iter
            }

            submethod BUILD(:$array) {
                $!array := nqp::decont($array);
            }

            method reify($n, :$sink) {
                unless nqp::isconcrete($!reified) {
                    my $rpa := nqp::list();
                    my int $i = $!idx;
                    my int $stop = $i + nqp::unbox_i(
                        nqp::istype($n, Whatever) ?? 1000 !! $n);
                    $stop = nqp::elems($!array)
                        if $stop > nqp::elems($!array);
                    while $i < $stop {
                        nqp::push($rpa, nqp::atposref_i($!array, $i));
                        $i = $i + 1;
                    }
                    if $stop != nqp::elems($!array) {
                        my $next := nqp::create(self);
                        nqp::bindattr($next, NativeIntArrayIter, '$!array', $!array);
                        nqp::bindattr_i($next, NativeIntArrayIter, '$!idx', $i);
                        nqp::push($rpa, $next);
                    }
                    $!reified := nqp::p6parcel($rpa, nqp::null());
                    $!array := Any;
                }
                $!reified;
            }

            multi method infinite(NativeIntArrayIter:D:) { False }

            multi method DUMP(NativeIntArrayIter:D: :$indent-step = 4, :%ctx?) {
                return DUMP(self, :$indent-step) unless %ctx;
                my Mu $attrs := nqp::list();
                nqp::push($attrs, '$!array');
                nqp::push($attrs,  $!array);
                nqp::push($attrs, '$!reified');
                nqp::push($attrs,  $!reified);
                self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
            }
        }
        method iterator() {
            NativeIntArrayIter.new(self)
        }
    }

    my role numarray[::T] does Positional[T] is array_type(T) {
        multi method AT-POS(array:D: int $idx) is rw {
            nqp::atposref_n(self, $idx)
        }
        multi method AT-POS(array:D: Int $idx) is rw {
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
            nqp::setelems(self, 0);
            nqp::bindpos_n(self, 0, nqp::unbox_n($value));
            self
        }
        multi method STORE(array:D: @values) {
            nqp::setelems(self, 0);
            my int $i = 0;
            my int $n = @values.elems;
            while $i < $n {
                nqp::bindpos_n(self, $i, nqp::unbox_n(@values.AT-POS($i)));
                $i = $i + 1;
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
        multi method push(array:D: @values) {
            fail 'Cannot .push an infinite list' if @values.infinite;
            nqp::push_n(self, $_) for @values;
            self
        }

        method pop(array:D:) returns num {
            nqp::elems(self) > 0
              ?? nqp::pop_n(self)
              !! die 'Element popped from empty array';
        }

        method shift() returns num {
            nqp::elems(self) > 0
              ?? nqp::shift_n(self)
              !! die 'Element shifted from empty array';
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
            fail 'Cannot .unshift an infinite list' if @values.infinite;
            nqp::unshift_n(self, @values.pop) while @values;
            self
        }

        method splice($offset = 0, $size?, *@values) {
            my $o = $offset;
            my $s = $size;
            my $elems = self.elems;
            $o = $o($elems) if nqp::istype($o, Callable);
            X::OutOfRange.new(
                what => 'offset argument to array.splice',
                got  => $offset,
                range => (0..^$elems),
            ).fail if $o < 0;
            $s //= $elems - ($o min $elems);
            $s = $s($elems - $o) if nqp::istype($s, Callable);
            X::OutOfRange.new(
                what => 'size argument to array.splice',
                got  => $size,
                range => (0..^($elems - $o)),
            ).fail if $s < 0;

            my @ret := nqp::create(self);
            my int $i = $o;
            my int $n = $o + $s - 1;
            while $i <= $n {
                nqp::push_n(@ret, nqp::atpos_n(self, $i));
                $i = $i + 1;
            }

            my @splicees := nqp::create(self);
            nqp::push_n(@splicees, @values.shift) while @values;
            nqp::splice(self, @splicees, $o.Int, $s.Int);
            @ret;
        }

        my class NativeNumArrayIter is Iterator {
            has $!array;    # Native array we're iterating
            has $!reified;  # Parcel of native array refs we return after reifying
            has int $!idx;  # Starting index of this iterator

            method new($array) {
                my \iter = nqp::create(self);
                iter.BUILD(:$array);
                iter
            }

            submethod BUILD(:$array) {
                $!array := nqp::decont($array);
            }

            method reify($n, :$sink) {
                unless nqp::isconcrete($!reified) {
                    my $rpa := nqp::list();
                    my int $i = $!idx;
                    my int $stop = $i + nqp::unbox_i(
                        nqp::istype($n, Whatever) ?? 1000 !! $n);
                    $stop = nqp::elems($!array)
                        if $stop > nqp::elems($!array);
                    while $i < $stop {
                        nqp::push($rpa, nqp::atposref_n($!array, $i));
                        $i = $i + 1;
                    }
                    if $stop != nqp::elems($!array) {
                        my $next := nqp::create(self);
                        nqp::bindattr($next, NativeNumArrayIter, '$!array', $!array);
                        nqp::bindattr_i($next, NativeNumArrayIter, '$!idx', $i);
                        nqp::push($rpa, $next);
                    }
                    $!reified := nqp::p6parcel($rpa, nqp::null());
                    $!array := Any;
                }
                $!reified;
            }

            multi method infinite(NativeNumArrayIter:D:) { False }

            multi method DUMP(NativeNumArrayIter:D: :$indent-step = 4, :%ctx?) {
                return DUMP(self, :$indent-step) unless %ctx;
                my Mu $attrs := nqp::list();
                nqp::push($attrs, '$!array');
                nqp::push($attrs,  $!array);
                nqp::push($attrs, '$!reified');
                nqp::push($attrs,  $!reified);
                self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
            }
        }
        method iterator() {
            NativeNumArrayIter.new(self)
        }
    }

    method ^parameterize(Mu:U \arr, Mu:U \t) {
        my int $kind = nqp::objprimspec(t);
        if $kind == 1 {
            my $what := arr.^mixin(intarray[t]);
            $what.^set_name("{arr.^name}[{t.^name}]");
            $what;
        }
        elsif $kind == 2 {
            my $what := arr.^mixin(numarray[t]);
            $what.^set_name("{arr.^name}[{t.^name}]");
            $what;
        }
        elsif $kind == 3 {
            nqp::die('NYI');
        }
        else {
            die "Can only parameterize array with a native type, not {t.^name}";
        }
    }

    proto method new(|) {*}
    multi method new() {
        self!validate-parameterized();
        nqp::create(self)
    }
    multi method new(@values) {
        self!validate-parameterized();
        nqp::create(self).STORE(@values)
    }
    multi method new(*@values) {
        self!validate-parameterized();
        nqp::create(self).STORE(@values)
    }

    method !validate-parameterized() {
        nqp::isnull(nqp::typeparameterized(self)) &&
            die "Must parameterize array[T] with a type before creating it";
    }

    method BIND-POS(|) is hidden-from-backtrace {
        die "Cannot bind to a natively typed array";
    }
    method DELETE-POS(|) is hidden-from-backtrace {
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
    multi method end(array:D:)      { nqp::elems(self) - 1 }
    multi method infinite(array:D:) { False }

    method eager() { self }
    method flat()  { self }
    method list()  { self }

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

    method gimme($) {
        # Native arrays aren't lazy, so nothing to do.
        nqp::elems(self)
    }

    method FLATTENABLE_LIST() { self }
    method FLATTENABLE_HASH() { nqp::hash() }
}
