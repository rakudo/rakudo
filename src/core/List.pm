# for our tantrums
my class X::TypeCheck { ... }
my class X::TypeCheck::Splice { ... }
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
                # XXX GLR why .Parcel here?
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

sub find-reducer-for-op($op) {
    try my %prec := $op.prec;
    return &METAOP_REDUCE_LEFT if (nqp::isnull(%prec) or ! %prec);
    my $reducer = %prec<prec> eq 'f='
        ?? 'listinfix'
        !! %prec<assoc> // 'left';
    ::('&METAOP_REDUCE_' ~ $reducer.uc);
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
        fail X::Cannot::Infinite.new(:action('.pick from')) if self.infinite;
        my $elems = self.elems;
        $elems ?? nqp::atpos($!items,$elems.rand.floor) !! Nil;
    }
    multi method pick(Whatever, :$eager!) {
        return self.pick(*) if !$eager;

        fail X::Cannot::Infinite.new(:action('.pick from')) if self.infinite;

        my Int $elems = self.elems;
        return () unless $elems;

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
        fail X::Cannot::Infinite.new(:action('.pick from')) if self.infinite;

        my Int $elems = self.elems;
        return () unless $elems;

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
        fail X::Cannot::Infinite.new(:action('.pick from')) if self.infinite;
        ## We use a version of Fisher-Yates shuffle here to
        ## replace picked elements with elements from the end
        ## of the list, resulting in an O(n) algorithm.

        my Int $elems = self.elems;
        return () unless $elems;

        my int $n = number > $elems ?? $elems !! number.Int;

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

    proto method roll(|) is nodal { * }
    multi method roll() {
        fail X::Cannot::Infinite.new(:action('.roll from')) if self.infinite;
        my $elems = self.elems;
        $elems ?? nqp::atpos($!items,$elems.rand.floor) !! Nil;
    }
    multi method roll(Whatever) {
        fail X::Cannot::Infinite.new(:action('.roll from')) if self.infinite;
        my $elems = self.elems;
        return () unless $elems;

        my $list := gather loop {
            take nqp::atpos($!items,$elems.rand.floor);
        }
        nqp::bindattr($list,List,'$!infinite',True);
        $list;
    }
    multi method roll(\number) {
        return self.roll(*) if number == Inf;

        fail X::Cannot::Infinite.new(:action('.roll from')) if self.infinite;
        my $elems = self.elems;
        return () unless $elems;

        my int $n = number.Int;

        gather while $n > 0 {
            take nqp::atpos($!items,$elems.rand.floor);
            $n = $n - 1;
        }
    }

    method reverse() is nodal {
        self.gimme(*);
        fail X::Cannot::Infinite.new(:action<reverse>) if $!nextiter.defined;
        my Mu $rev  := nqp::list();
        my Mu $orig := nqp::clone($!items);
        nqp::push($rev, nqp::pop($orig)) while $orig;
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $rev);
        $rlist;
    }

    method rotate(Int(Cool) $n is copy = 1) is nodal {
        self.gimme(*);
        fail X::Cannot::Infinite.new(:action<rotate>) if $!nextiter.defined;
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

    multi method ACCEPTS(List:D: $topic) { self }

    method rotor(List:D: *@cycle, :$partial) is nodal {
        die "Must specify *how* to rotor a List"
          unless @cycle.infinite || @cycle;

        my $finished = 0;
        # (Note, the xx should be harmless if the cycle is already infinite by accident.)
        my @c := @cycle.infinite ?? @cycle !! @cycle xx *;
        gather for @c -> $s {
            my $elems;
            my $gap;
            if $s ~~ Pair {
                $elems = +$s.key;
                $gap   = +$s.value;
            }
            elsif $s < 1 {
                die "Cannot have elems < 1, did you mean to specify a Pair with => $s?";
            }
            else {
                $elems = +$s;
                $gap   = 0;
            }

            if $finished + $elems <= self.gimme($finished + $elems) {
                take self[$finished ..^ $finished + $elems];
                $finished += $elems + $gap;
            }
            else {
                take self[$finished .. *]
                  if $partial and $finished < self.elems;
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

proto sub infix:<xx>(|)     { * }
multi sub infix:<xx>()      { fail "No zero-arg meaning for infix:<xx>" }
multi sub infix:<xx>(Mu \x) { x }
multi sub infix:<xx>(Mu \x, Num $n, :$thunked!) {
    infix:<xx>(x, $n == Inf ?? Whatever !! $n.Int, :$thunked);
}
multi sub infix:<xx>(Mu \x, Whatever, :$thunked!) {
    GatherIter.new({ loop { take x.() } }, :infinite).list
}
multi sub infix:<xx>(Mu \x, Int() $n, :$thunked!) {
    my int $todo = $n;
    GatherIter.new({ take x.() while ($todo = $todo - 1) >= 0 }).list
}
multi sub infix:<xx>(Mu \x, Num $n) {
    infix:<xx>(x, $n == Inf ?? Whatever !! $n.Int);
}
multi sub infix:<xx>(Mu \x, Whatever) {
    GatherIter.new({ loop { take x } }, :infinite).list
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
sub splice(@arr, |c)        { @arr.splice(|c) }

multi sub infix:<cmp>(@a, @b) { (@a Zcmp @b).first(&prefix:<?>) || @a <=> @b }

# vim: ft=perl6 expandtab sw=4
