# for our tantrums
my class X::TypeCheck { ... }
my class X::Cannot::Infinite { ... }
my class X::Cannot::Empty { ... }
my role Supply { ... }

my sub combinations($n, $k) {
    my @result;
    my @stack;

    return ((),) unless $k;

    @stack.push(0);
    gather while @stack {
        my $index = @stack - 1;
        my $value = @stack.pop;

        while $value < $n {
            @result[$index++] = $value++;
            @stack.push($value);
            if $index == $k {
                take [@result].Parcel;
                $value = $n;  # fake a last
            }
        }
    }
}

my sub permutations(Int $n) {
    $n == 1 ?? ( (0,) ) !!
    gather for ^$n -> $i {
        my @i = 0 ..^ $i, $i ^..^ $n;
        sink permutations($n - 1).map: { take [$i, @i[@$_]].Parcel }
    }
}

my class List does Positional { # declared in BOOTSTRAP
    # class List is Iterable is Cool
    #   has Mu $!items;        # VM's array of our reified elements
    #   has Mu $!flattens;     # true if this list flattens its parcels
    #   has Mu $!nextiter;     # iterator for generating remaining elements
    #   has Any $!infinite;    # is this list infinite or not

    method new(|) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);

        nqp::p6list($args, self.WHAT, Mu);
    }

    multi method Bool(List:D:)    { self.gimme(1).Bool }
    multi method Int(List:D:)     { self.elems }
    multi method end(List:D:)     { self.elems - 1 }
    multi method Numeric(List:D:) { self.elems }
    multi method Str(List:D:)     { self.join(' ') }

    # Pretend we're a Match assuming we're a list of Matches
    method to()         { self.elems ?? self[self.end].to !! Nil }
    method from()       { self.elems ?? self[0].from !! Nil }

    method fmt($format = '%s', $separator = ' ') {
        self.map({ .fmt($format) }).join($separator);
    }

    method flat() { self.flattens
                    ?? self
                    !! nqp::p6list(nqp::list(self), List, Bool::True)
    }
    method list() { self }
    method lol() {
        self.gimme(0);
        my Mu $rpa := nqp::clone($!items);
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6list($rpa, LoL, Mu);
    }

    method flattens() { $!flattens }

    method Capture() {
        self.gimme(*);
        my $cap := nqp::create(Capture);
        nqp::bindattr($cap, Capture, '$!list', $!items);
        $cap
    }

    method Parcel() {
        my Mu $rpa := nqp::clone(nqp::p6listitems(self));
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6parcel($rpa, Any);
    }

    method Supply(List:D:) { Supply.from-list(self) }

    multi method AT-POS(List:D: int \pos) is rw {
        fail X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>)
          if nqp::islt_i(pos,0);
        self.EXISTS-POS(pos) ?? nqp::atpos($!items,pos) !! Nil;
    }
    multi method AT-POS(List:D: Int:D \pos) is rw {
        my int $pos = nqp::unbox_i(pos);
        fail X::OutOfRange.new(:what<Index>,:got(pos),:range<0..Inf>)
          if nqp::islt_i($pos,0);
        self.EXISTS-POS($pos) ?? nqp::atpos($!items,$pos) !! Nil;
    }

    method eager() { self.gimme(*); self }

    method elems() is nodal {
        return 0 unless self.DEFINITE;
        return nqp::elems(nqp::p6listitems(self)) unless nqp::defined($!nextiter);
        # Get as many elements as we can.  If gimme stops before
        # reaching the end of the list, assume the list is infinite.
        my $n := self.gimme(*);
        nqp::defined($!nextiter) ?? Inf !! $n
    }

    multi method EXISTS-POS(List:D: int $pos) {
        return False if nqp::islt_i($pos,0);
        self.gimme($pos + 1);
        nqp::p6bool(
          nqp::not_i(nqp::isnull(nqp::atpos($!items,$pos)))
        );
    }
    multi method EXISTS-POS(List:D: Int:D $pos) {
        return False if $pos < 0;
        self.gimme($pos + 1);
        nqp::p6bool(
          nqp::not_i(nqp::isnull(nqp::atpos($!items,nqp::unbox_i($pos))))
        );
    }

    method gimme($n, :$sink) {
        return unless self.DEFINITE;
        # loop through iterators until we have at least $n elements
        my int $count = nqp::elems(nqp::p6listitems(self));
        if nqp::istype($n, Whatever) || nqp::istype($n, Num) && nqp::istrue($n == Inf) {
            while $!nextiter.DEFINITE && !$!nextiter.infinite {
                $!nextiter.reify(*, :$sink);
                $count = nqp::elems($!items);
            }
        }
        else {
            my int $target = $n.Int;
            while nqp::isconcrete($!nextiter) && $count < $target {
                $!nextiter.reify($target - $count, :$sink);
                $count = nqp::elems($!items);
            }
        }

        # return the number of elements we have now
        $count
    }

    multi method infinite(List:D:) {
        $!infinite ||= ?$!nextiter.infinite;
    }

    method iterator() {
        # Return a reified ListIter containing our currently reified elements
        # and any subsequent iterator.
        my $iter := nqp::create(ListIter);
        nqp::bindattr($iter, ListIter, '$!nextiter', $!nextiter);
        nqp::bindattr($iter, ListIter, '$!reified', self.Parcel());
        $iter;
    }

    method munch($n is copy) {
        $n = 0 if $n < 0;
        $n = self.gimme($n) if nqp::not_i(nqp::istype($n, Int))
                               || nqp::not_i(nqp::islist($!items))
                               || nqp::islt_i(nqp::elems($!items), nqp::unbox_i($n));
        nqp::p6parcel(
            nqp::p6shiftpush(nqp::list(), $!items, nqp::unbox_i($n)),
            Any
        )
    }

    proto method pick(|) is nodal { * }
    multi method pick() {
        fail X::Cannot::Infinite.new(:action<.pick from>) if self.infinite;
        my $elems = self.elems;
        $elems ?? nqp::atpos($!items,$elems.rand.floor) !! Nil;
    }
    multi method pick(Whatever, :$eager!) {
        return self.pick(*) if !$eager;

        fail X::Cannot::Infinite.new(:action<.pick from>) if self.infinite;

        my Int $elems = self.elems;
        return unless $elems;

        my Mu $picked := nqp::clone($!items);
        my int $i;
        my Mu $val;
        while $elems {
            $i     = $elems.rand.floor;
            $elems = $elems - 1;
            # switch them
            $val  := nqp::atpos($picked,$i);
            nqp::bindpos($picked,$i,nqp::atpos($picked,nqp::unbox_i($elems)));
            nqp::bindpos($picked,nqp::unbox_i($elems),$val);
        }
        nqp::p6parcel($picked,Any);
    }
    multi method pick(Whatever) {
        fail X::Cannot::Infinite.new(:action<.pick from>) if self.infinite;

        my Int $elems = self.elems;
        return unless $elems;

        my Mu $rpa := nqp::clone($!items);
        my int $i;
        gather while $elems {
            $i     = $elems.rand.floor;
            $elems = $elems - 1;
            take-rw nqp::atpos($rpa,$i);
            # replace selected element with last unpicked one
            nqp::bindpos($rpa,$i,nqp::atpos($rpa,nqp::unbox_i($elems)));
        }
    }
    multi method pick(\number) {
        fail X::Cannot::Infinite.new(:action<.pick from>) if self.infinite;
        ## We use a version of Fisher-Yates shuffle here to
        ## replace picked elements with elements from the end
        ## of the list, resulting in an O(n) algorithm.

        my Int $elems = self.elems;
        return unless $elems;

        my int $n = number > $elems ?? $elems !! number.Int;
        return nqp::atpos($!items,$elems.rand.floor) if $n == 1;

        my Mu $rpa := nqp::clone($!items);
        my int $i;
        gather while $n {
            $i     = $elems.rand.floor;
            $elems = $elems - 1;
            $n     = $n - 1;
            take-rw nqp::atpos($rpa,$i);
            # replace selected element with last unpicked one
            nqp::bindpos($rpa,$i,nqp::atpos($rpa,nqp::unbox_i($elems)));
        }
    }

    method pop() is parcel is nodal {
        my $elems = self.gimme(*);
        fail X::Cannot::Infinite.new(:action<.pop from>) if $!nextiter.defined;
        $elems > 0
          ?? nqp::pop($!items)
          !! fail X::Cannot::Empty.new(:action<.pop>, :what(self.^name));
    }

    method shift() is parcel is nodal {
        # make sure we have at least one item, then shift+return it
        nqp::islist($!items) && nqp::existspos($!items, 0) || self.gimme(1)
          ?? nqp::shift($!items)
          !! fail X::Cannot::Empty.new(:action<.shift>, :what(self.^name));
    }

    my &list_push = multi method push(List:D: *@values) {
        fail X::Cannot::Infinite.new(:action<.push>, :what(self.^name))
          if @values.infinite;
        nqp::p6listitems(self);
        my $elems = self.gimme(*);
        fail X::Cannot::Infinite.new(:action<.push to>) if $!nextiter.DEFINITE;

        # push is always eager
        @values.gimme(*);

        # need type checks?
        my $of := self.of;

        unless $of =:= Mu {
            X::TypeCheck.new(
              operation => '.push',
              expected  => $of,
              got       => $_,
            ).throw unless nqp::istype($_, $of) for @values;
        }

        nqp::splice($!items,
                nqp::getattr(@values, List, '$!items'),
                $elems, 0);

        self;
    }

    multi method push(List:D: \value) {
        if nqp::iscont(value) || nqp::not_i(nqp::istype(value, Iterable)) && nqp::not_i(nqp::istype(value, Parcel)) {
            $!nextiter.DEFINITE && self.gimme(*);
            fail X::Cannot::Infinite.new(:action<.push to>)
              if $!nextiter.DEFINITE;
            nqp::p6listitems(self);
            nqp::istype(value, self.of)
                ?? nqp::push($!items, nqp::assign(nqp::p6scalarfromdesc(nqp::null), value))
                !! X::TypeCheck.new(
                      operation => '.push',
                      expected  => self.of,
                      got       => value,
                    ).throw;
            self
        }
        else {
            list_push(self, value)
        }
    }

    multi method unshift(List:D: \value) {
        if nqp::iscont(value) || !(nqp::istype(value, Iterable) || nqp::istype(value, Parcel)) {
            nqp::p6listitems(self);
            value.gimme(*) if nqp::istype(value, List); # fixes #121994
            nqp::istype(value, self.of)
                ?? nqp::unshift($!items, my $ = value)
                !! X::TypeCheck.new(
                      operation => '.push',
                      expected  => self.of,
                      got       => value,
                    ).throw;
            self
        }
        else {
            callsame();
        }
    }

    multi method unshift(List:D: *@values) {
        fail X::Cannot::Infinite.new(:action<.unshift>, :what(self.^name))
          if @values.infinite;
        nqp::p6listitems(self);

        # don't bother with type checks
        my $of := self.of;
        if ( $of =:= Mu ) {
            nqp::unshift($!items, @values.pop) while @values;
        }

        # we must check types
        else {
            while @values {
                my $value := @values.pop;
                if nqp::istype($value, $of) {
                    nqp::unshift($!items, $value);
                }

                # huh?
                else {
                    X::TypeCheck.new(
                      operation => '.unshift',
                      expected  => $of,
                      got       => $value,
                    ).throw;
                }
            }
        }

        self
    }

    method plan(List:D: |args) is nodal {
        nqp::p6listitems(self);
        my $elems = self.gimme(*);
        fail X::Cannot::Infinite.new(:action<.plan to>) if $!nextiter.defined;

#        # need type checks?
#        my $of := self.of;
#
#        unless $of =:= Mu {
#            X::TypeCheck.new(
#              operation => '.push',
#              expected  => $of,
#              got       => $_,
#            ).throw unless nqp::istype($_, $of) for @values;
#        }

        nqp::bindattr(self, List, '$!nextiter', nqp::p6listiter(nqp::list(args.list), self));
        Nil;
    }

    proto method roll(|) is nodal { * }
    multi method roll() {
        fail X::Cannot::Infinite.new(:action<.roll from>) if self.infinite;
        my $elems = self.elems;
        $elems ?? nqp::atpos($!items,$elems.rand.floor) !! Nil;
    }
    multi method roll(Whatever) {
        fail X::Cannot::Infinite.new(:action<.roll from>) if self.infinite;
        my $elems = self.elems;
        return unless $elems;

        my $list := gather loop {
            take nqp::atpos($!items,$elems.rand.floor);
        }
        nqp::bindattr($list,List,'$!infinite',True);
        $list;
    }
    multi method roll(\number) {
        fail X::Cannot::Infinite.new(:action<.roll from>) if self.infinite;
        my $elems = self.elems;
        return unless $elems;

        my int $n = number.Int;
        return nqp::atpos($!items,$elems.rand.floor) if $n == 1;

        gather while $n > 0 {
            take nqp::atpos($!items,$elems.rand.floor);
            $n = $n - 1;
        }
    }

    method reverse() is nodal {
        self.gimme(*);
        fail X::Cannot::Infinite.new(:action<.reverse>) if $!nextiter.defined;
        my Mu $rev  := nqp::list();
        my Mu $orig := nqp::clone($!items);
        nqp::push($rev, nqp::pop($orig)) while $orig;
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $rev);
        $rlist;
    }

    method rotate(Int $n is copy = 1) is nodal {
        self.gimme(*);
        fail X::Cannot::Infinite.new(:action<.rotate>) if $!nextiter.defined;
        my $items = nqp::p6box_i(nqp::elems($!items));
        return self if !$items;

        $n %= $items;
        return self if $n == 0;

        my Mu $res := nqp::clone($!items);
        if $n > 0 {
            nqp::push($res, nqp::shift($res)) while $n--;
        }
        elsif $n < 0 {
            nqp::unshift($res, nqp::pop($res)) while $n++;
        }
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $res);
        $rlist;
    }

    method splice($offset = 0, $size?, *@values) is nodal {
        self.gimme(*);
        my $o = $offset;
        my $s = $size;
        my $elems = self.elems;
        $o = $o($elems) if nqp::istype($o, Callable);
        X::OutOfRange.new(
            what => 'offset argument to List.splice',
            got  => $offset,
            range => (0..^self.elems),
        ).fail if $o < 0;
        $s //= self.elems - ($o min $elems);
        $s = $s(self.elems - $o) if nqp::istype($s, Callable);
        X::OutOfRange.new(
            what => 'size argument to List.splice',
            got  => $size,
            range => (0..^(self.elems - $o)),
        ).fail if $s < 0;

        my @ret = self[$o..($o + $s - 1)];
        nqp::splice($!items,
                    nqp::getattr(@values.eager, List, '$!items'),
                    $o.Int, $s.Int);
        @ret;
    }

    method sort($by = &infix:<cmp>) is nodal {
        fail X::Cannot::Infinite.new(:action<.sort>) if self.infinite; #MMD?

        # Instead of sorting elements directly, we sort a Parcel of
        # indices from 0..^$list.elems, then use that Parcel as
        # a slice into self. This is for historical reasons: on
        # Parrot we delegate to RPA.sort. The JVM implementation
        # uses a Java collection sort. MoarVM has its sort algorithm
        # implemented in NQP.

        # nothing to do here
        my $elems := self.elems;
        return self if $elems < 2;

        # Range is currently optimized for fast Parcel construction.
        my $index := Range.new(0, $elems, :excludes-max).reify(*);
        my Mu $index_rpa := nqp::getattr($index, Parcel, '$!storage');

        # if $by.arity < 2, then we apply the block to the elements
        # for sorting.
        if ($by.?count // 2) < 2 {
            my $list = self.map($by).eager;
            nqp::p6sort($index_rpa, -> $a, $b { $list.AT-POS($a) cmp $list.AT-POS($b) || $a <=> $b });
        }
        else {
            my $list = self.eager;
            nqp::p6sort($index_rpa, -> $a, $b { $by($list.AT-POS($a), $list.AT-POS($b)) || $a <=> $b });
        }
        self[$index];
    }

    multi method ACCEPTS(List:D: $topic) { self }

    method uniq(|c) is nodal {
        DEPRECATED('unique', |<2014.11 2015.09>);
        self.unique(|c);
    }

    proto method unique(|) is nodal {*}
    multi method unique() {
        my $seen := nqp::hash();
        my str $target;
        gather @.list.map: {
            $target = nqp::unbox_s($_.WHICH);
            unless nqp::existskey($seen, $target) {
                nqp::bindkey($seen, $target, 1);
                take $_;
            }
        }
    }
    multi method unique( :&as!, :&with! ) {
        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather @.list.map: {
            $target = &as($_);
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push($target);
                take $_;
            }
        };
    }
    multi method unique( :&as! ) {
        my $seen := nqp::hash();
        my str $target;
        gather @.list.map: {
            $target = &as($_).WHICH;
            unless nqp::existskey($seen, $target) {
                nqp::bindkey($seen, $target, 1);
                take $_;
            }
        }
    }
    multi method unique( :&with! ) {
        nextwith() if &with === &[===]; # use optimized version

        my @seen;  # should be Mu, but doesn't work in settings :-(
        my Mu $target;
        gather @.list.map: {
            $target := $_;
            if first( { with($target,$_) }, @seen ) =:= Nil {
                @seen.push($target);
                take $_;
            }
        }
    }

    my @secret;
    proto method squish(|) is nodal {*}
    multi method squish( :&as!, :&with = &[===] ) {
        my $last = @secret;
        my str $which;
        gather @.list.map: {
            $which = &as($_).Str;
            unless with($which,$last) {
                $last = $which;
                take $_;
            }
        }
    }
    multi method squish( :&with = &[===] ) {
        my $last = @secret;
        gather @.list.map: {
            unless with($_,$last) {
                $last = $_;
                take $_;
            }
        }
    }

    method rotor(*@cycle, :$partial) is nodal {
        my $finished = 0;
        # (Note, the xx should be harmless if the cycle is already infinite by accident.)
        my @c := @cycle.infinite ?? @cycle !! @cycle xx *;
        gather for @c -> $s {
            my $elems;
            my $gap;
            if $s ~~ Pair { $elems = +$s.key; $gap = +$s.value; }
            else          { $elems = +$s;     $gap = 0; }

            if $finished + $elems <= self.gimme($finished + $elems) {
                take self[$finished ..^ $finished + $elems];
                $finished += $elems + $gap;
            }
            else {
                take self[$finished .. *] if $partial and $finished < self.elems;
                last;
            }
        }
    }

    multi method gist(List:D:) {
        @(self).map( -> $elem {
            given ++$ {
                when 101 { '...' }
                when 102 { last }
                default  { $elem.gist }
            }
        } ).join: ' ';
    }
    multi method perl(List:D \SELF:) {
        self.gimme(*);
        (nqp::iscont(SELF) ?? '$' !! '') ~ self.Parcel.perl;
    }

    method REIFY(Parcel \parcel, Mu \nextiter) {
        nqp::splice($!items, nqp::getattr(parcel, Parcel, '$!storage'),
                    nqp::elems($!items), 0);
        nqp::bindattr(self, List, '$!nextiter', nextiter);
        parcel
    }

    method FLATTENABLE_LIST() { self.gimme(*); $!items }
    method FLATTENABLE_HASH() { nqp::hash() }

    multi method DUMP(List:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my $flags    := ("\x221e" if self.infinite);
        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!flattens');
        nqp::push($attrs,  $!flattens );
        nqp::push($attrs, '$!items'   );
        nqp::push($attrs,  $!items    );
        nqp::push($attrs, '$!nextiter');
        nqp::push($attrs,  $!nextiter );
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx, :$flags);
    }

    multi method keys(List:D:) {
        self.values.map: { (state $)++ }
    }
    multi method kv(List:D:) {
        gather self.values.map: {
            take (state $)++;
            take-rw $_;
        }
    }
    multi method values(List:D:) {
        my Mu $rpa := nqp::clone(nqp::p6listitems(self));
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6list($rpa, List, self.flattens);
    }
    multi method pairs(List:D:) {
        self.values.map: { (state $)++ => $_ }
    }
    multi method antipairs(List:D:) {
        self.values.map: { $_ => (state $)++ }
    }

    multi method invert(List:D:) {
        self.map({ nqp::decont(.value) »=>» .key }).flat
    }

    method reduce(List: &with) is nodal {
        return unless self.DEFINITE;
        return self.values if self.elems < 2;
        if &with.count > 2 and &with.count < Inf {
            my $moar = &with.count - 1;
            my \vals = self.values;
            if try &with.prec<assoc> eq 'right' {
                my Mu $val = vals.pop;
                $val = with(|vals.splice(*-$moar,$moar), $val) while vals >= $moar;
                return $val;
            }
            else {
                my Mu $val = vals.shift;
                $val = with($val, |vals.splice(0,$moar)) while vals >= $moar;
                return $val;
            }
        }
        my $reducer = find-reducer-for-op(&with);
        $reducer(&with)(self) if $reducer;
    }

    method sink() {
        self.gimme(*, :sink) if self.DEFINITE && $!nextiter.DEFINITE;
        Nil;
    }

    # this is a remnant of a previous implementation of .push(), which
    # apparently is used by LoL.  Please remove when no longer necessary.
    method STORE_AT_POS(Int \pos, Mu \v) is rw is nodal {
        nqp::bindpos($!items, nqp::unbox_i(pos), v)
    }

    proto method combinations($?) is nodal {*}
    multi method combinations( Int $of ) {
        combinations(self.elems, $of).eager.map: { self[@$_] }
    }
    multi method combinations( Range $ofrange = 0 .. * ) {
        gather for $ofrange.min .. ($ofrange.max min self.elems) -> $of {
            # XXX inside of gather should already sink
            sink combinations(self.elems, $of).eager.map: { take self[@$_] }
        }
    }

    method permutations() is nodal {
        # need block on Moar because of RT#121830
        permutations(self.elems).eager.map: { self[@$_] }
    }

    method CALL-ME(List:U: |c) {
        self.new(|c);
    }
}

# internal, caps to not hide 'eager' keyword
sub EAGER(|) {
    nqp::p6list(nqp::p6argvmarray(), List, Bool::True).eager
}

sub flat(|) {
    nqp::p6list(nqp::p6argvmarray(), List, Bool::True)
}

sub list(|) {
    nqp::p6list(nqp::p6argvmarray(), List, Mu)
}

proto sub infix:<xx>(|)       { * }
multi sub infix:<xx>()        { fail "No zero-arg meaning for infix:<xx>" }
multi sub infix:<xx>(Mu \x)   { x }
multi sub infix:<xx>(Mu \x, Int() $n is copy, :$thunked!) {
    GatherIter.new({ take x.() while --$n >= 0; }, :infinite($n == Inf)).list
}
multi sub infix:<xx>(Mu \x, Whatever, :$thunked!) {
    GatherIter.new({ loop { take x.() } }, :infinite(True)).list
}
multi sub infix:<xx>(Mu \x, Whatever) {
    GatherIter.new({ loop { take x } }, :infinite(True)).list
}
multi sub infix:<xx>(Mu \x, Int() $n) {
    my int $size = $n;

    my Mu $rpa := nqp::list();
    if $size > 0 {
        nqp::setelems($rpa, $size);
        my int $i;
        while $i < $size {
            nqp::bindpos($rpa,$i,x);
            $i = $i + 1;
        }
    }

    nqp::p6parcel($rpa, Any);
}

proto sub pop(@) {*}
multi sub pop(@a) { @a.pop }

proto sub shift(@) {*}
multi sub shift(@a) { @a.shift }

proto sub unshift(|) {*}
multi sub unshift(\a, \elem) { a.unshift: elem }
multi sub unshift(\a, *@elems) { a.unshift: @elems }

proto sub push(|) {*}
multi sub push(\a, \elem) { a.push: elem }
multi sub push(\a, *@elems) { a.push: @elems }

sub reverse(*@a)            { @a.reverse }
sub rotate(@a, Int $n = 1)  { @a.rotate($n) }
sub reduce (&with, *@list)  { @list.reduce(&with) }
sub splice(@arr, $offset = 0, $size?, *@values) {
    @arr.splice($offset, $size, @values)
}

multi sub infix:<cmp>(@a, @b) { (@a Zcmp @b).first(&prefix:<?>) || @a <=> @b }

# vim: ft=perl6 expandtab sw=4
