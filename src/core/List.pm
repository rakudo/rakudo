my class List does Positional {
    # declared in BOOTSTRAP.pm:
    #   is Iterable;           # parent class
    #   has Mu $!items;        # RPA of our reified elements
    #   has $!flattens;        # true if this list flattens its parcels
    #   has $!nextiter;        # iterator for generating remaining elements

    method new(|) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        nqp::p6list($args, self.WHAT, Mu);
    }

    method Bool()       { self.gimme(1).Bool }
    method Int()        { self.elems }
    method end()        { self.elems - 1 }
    multi method Numeric(List:D:)  { self.elems }
    multi method Str(List:D:)      { self.join(' ') }

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

    my &itemify = { .elems == 1 ?? $_ !! [.list] };
    proto method tree(|) {*}
    multi method tree(List:U:) { self }
    multi method tree(List:D:) {
        MapIter.new(self, &itemify, Mu).list;
    }
    multi method tree(List:D: Cool $count as Int) {
           $count <= 0 ?? self
        !! $count == 1 ?? self.tree
        !!  MapIter.new(
                self,
                {.elems == 1 ?? $_ !! [.tree($count - 1)]},
                Mu
            ).list;
    }
    multi method tree(List:D: &c) {
        MapIter.new(self, &c, Mu).list;
    }
    # uncommenting causes "Circularity detected in multi sub types"
#    multi method tree(List:D: *@ [$first, *@rest] where {.elems >= 2 }) {
#        MapIter.new(:list(self), :block(*.list(|@rest))).list.tree($first)
#    }

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

    proto method Set(|) {*}
    multi method Set() {
        set self;
    }

    proto method Bag(|) {*}
    multi method Bag() {
        bag self;
    }

    multi method at_pos(List:D: $pos is copy) is rw {
        $pos = $pos.Int;
        self.exists($pos)
          ?? nqp::atpos($!items, nqp::unbox_i($pos))
          !! Nil
    }
    multi method at_pos(List:D: int $pos) is rw {
        self.exists($pos)
            ?? nqp::atpos($!items, $pos)
            !! Nil;
    }

    method eager() { self.gimme(*); self }

    method elems() {
        # Get as many elements as we can.  If gimme stops before
        # reaching the end of the list, assume the list is infinite.
        my $n = self.gimme(*);
        $!nextiter.defined ?? $Inf !! $n
    }

    method exists(\pos) {
        self.gimme(pos + 1);
        nqp::p6bool(nqp::existspos($!items, nqp::unbox_i(pos)))
    }

    method gimme($n, :$sink) {
        # loop through iterators until we have at least $n elements
        my int $count = nqp::elems(nqp::p6listitems(self));
        my $eager = nqp::p6bool(nqp::istype($n, Whatever) || $n == $Inf);
        while $!nextiter.defined && ($eager 
                                       ?? !$!nextiter.infinite 
                                       !! ($count < $n)) {
            $!nextiter.reify($eager ?? Whatever !! $n - $count, :$sink);
            $count = nqp::elems($!items);
        }

        # return the number of elements we have now
        $count
    }

    method infinite() { 
        self.DEFINITE && $!nextiter.defined && $!nextiter.infinite;
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

    method pick($n is copy = 1) {
        fail "Cannot .pick from infinite list" if self.infinite; #MMD?
        ## We use a version of Fisher-Yates shuffle here to
        ## replace picked elements with elements from the end
        ## of the list, resulting in an O(n) algorithm.
        my $elems = self.elems;
        return unless $elems;
        $n = +$Inf if nqp::istype($n, Whatever);
        $n = $elems if $n > $elems;
        return self.at_pos($elems.rand.floor) if $n == 1;
        my Mu $rpa := nqp::clone($!items);
        my $i;
        my Mu $v;
        gather while $n > 0 {
            $i = nqp::rand_I(nqp::decont($elems), Int);
            $elems--; $n--;
            $v := nqp::atpos($rpa, nqp::unbox_i($i));
            # replace selected element with last unpicked one
            nqp::bindpos($rpa, nqp::unbox_i($i),
                         nqp::atpos($rpa, nqp::unbox_i($elems)));
            take-rw $v;
        }
    }

    method pop() is rw {
        fail 'Cannot .pop from an infinite list' if self.infinite; #MMD?
        my $elems = self.elems;
        $elems > 0
          ?? nqp::pop($!items)
          !! fail 'Element popped from empty list';
    }

    multi method push(List:D: *@values) {
        fail 'Cannot .push to an infinite list' if self.infinite; #MMD?
        fail 'Cannot .push an infinite list' if @values.infinite;
        my $pos = self.elems;
        self.STORE_AT_POS($pos++, @values.shift) while @values.gimme(1);
        self;
    }

    method roll($n is copy = 1) {
        fail "Cannot .roll from an infinite list" if self.infinite; #MMD?
        my $elems = self.elems;
        return unless $elems;
        $n = +$Inf if nqp::istype($n, Whatever);
        return self.at_pos($elems.rand.floor) if $n == 1;
        gather while $n > 0 {
            take nqp::atpos($!items, nqp::unbox_i($elems.rand.floor.Int));
            $n--;
        }
    }

    method reverse() {
        fail 'Cannot .reverse an infinite list' if self.infinite; #MMD?
        self.gimme(*);
        my Mu $rev  := nqp::list();
        my Mu $orig := nqp::clone($!items);
        nqp::push($rev, nqp::pop($orig)) while $orig;
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $rev);
        $rlist;
    }

    method rotate(Int $n is copy = 1) {
        fail 'Cannot .rotate an infinite list' if self.infinite; # MMD?
        self.gimme(*);
        my Mu $res := nqp::clone($!items);
        $n %= nqp::p6box_i(nqp::elems($!items));
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

    method shift() is rw {
        # make sure we have at least one item, then shift+return it
        nqp::islist($!items) && nqp::existspos($!items, 0) || self.gimme(1)
          ?? nqp::shift($!items) 
          !! fail 'Element shifted from empty list';
    }

    multi method unshift(List:D: *@elems) {
        nqp::p6listitems(self);
        while @elems {
            nqp::unshift($!items, @elems.pop)
        }
        self
    }

    method splice($offset = 0, $size?, *@values) {
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

    method sort($by = &infix:<cmp>) {
        fail 'Cannot .sort an infinite list' if self.infinite; #MMD?
        # We defer to Parrot's ResizablePMCArray.sort method here.
        # Instead of sorting elements directly, we sort a Parcel of
        # indices from 0..^$list.elems, then use that Parcel as
        # a slice into self.

        # Range is currently optimized for fast Parcel construction.
        my $index := Range.new(0, self.elems, :excludes_max).reify(*);
        my Mu $index_rpa := nqp::getattr($index, Parcel, '$!storage');

        # if $by.arity < 2, then we apply the block to the elements
        # for sorting.
        if ($by.?count // 2) < 2 {
            my $list = self.map($by).eager;
            nqp::p6sort($index_rpa, -> $a, $b { $list[$a] cmp $list[$b] || $a <=> $b });
        }
        else {
            my $list = self.eager;
            nqp::p6sort($index_rpa, -> $a, $b { $by($list[$a],$list[$b]) || $a <=> $b });
        }
        self[$index];
    }

    multi method ACCEPTS(List:D: $topic) {
        my $sseq = self;
        my $tseq = $topic.list;

        my $spos = 0;
        my $tpos = 0;
        while $spos < +$sseq {
            # if the next element is Whatever
            if $sseq[$spos] ~~ Whatever {
                # skip over all of the Whatevers
                $spos++ while $spos <= +$sseq && $sseq[$spos] ~~ Whatever;
                # if nothing left, we're done
                return True if !($spos < +$sseq);
                # find a target matching our new target
                $tpos++ while ($tpos < +$tseq) && $tseq[$tpos] !== $sseq[$spos];
                # return false if we ran out
                return False if !($tpos < +$tseq);
            }
            elsif $tpos >= +$tseq || $tseq[$tpos] !=== $sseq[$spos] {
                return False;
            }
            # skip matching elements
            $spos++;
            $tpos++;
        }
        # If nothing left to match, we're successful.
        $tpos >= +$tseq;
    }

    method classify ($test) { {}.classify( $test, @.list ) }

    method categorize ($test) { {}.categorize( $test, @.list ) }

    # This needs a way of taking a user-defined comparison
    # specifier, but AFAIK nothing has been spec'd yet.
    method uniq() {
        my $seen := nqp::hash();
        my str $which;
        gather sink for @.list {
            $which = nqp::unbox_s($_.WHICH);
            unless nqp::existskey($seen, $which) {
                take $_;
                nqp::bindkey($seen, $which, 1);
            }
        }
    }

    my @secret;
    method squish() {
        my $last = @secret;
        my $grepped;
        grep { $grepped = $_ !=== $last; $last = $_; $grepped }, @.list;
    }

    multi method gist(List:D:) { join ' ', map { $_.gist }, @(self) }
    multi method perl(List:D \SELF:) {
        self.gimme(*);
        self.Parcel.perl ~ '.list'  
          ~ (nqp::iscont(SELF) ?? '.item' !! '')
    }

    method REIFY(Parcel \parcel, Mu \nextiter) {
        nqp::splice($!items, nqp::getattr(parcel, Parcel, '$!storage'),
                    nqp::elems($!items), 0);
        nqp::bindattr(self, List, '$!nextiter', nextiter);
        parcel
    }

    method STORE_AT_POS(\pos, Mu \v) is rw {
        nqp::bindpos($!items, nqp::unbox_i(pos), v)
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

    method keys(List:D:) {
        (0..self.end).list;
    }
    method values(List:D:) {
        my Mu $rpa := nqp::clone(nqp::p6listitems(self));
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6list($rpa, List, self.flattens);
    }
    method pairs(List:D:) {
        self.keys.map: {; $_ => self.at_pos($_) };
    }
    method kv(List:D:) {
        self.keys.map: { ($_, self.at_pos($_)) };
    }

    method reduce(List:D: &with) {
        fail('can only reduce with arity 2')
            unless &with.arity <= 2 <= &with.count;
        my Mu $val;
        for self.keys {
            if $_ == 0 {
                $val = self.at_pos(0);
                next;
            }
            $val = with($val, self.at_pos($_));
        }
        $val;
    }

    method sink() {
        self.gimme(*, :sink) if self.defined;
        Nil;
    }
}

sub eager(|) {
    nqp::p6parcel(nqp::p6argvmarray(), Any).eager
}

sub flat(|) {
    nqp::p6list(nqp::p6argvmarray(), List, Bool::True)
}

sub list(|) {
    nqp::p6list(nqp::p6argvmarray(), List, Mu)
}

proto infix:<xx>(|)       { * }
multi infix:<xx>()        { fail "No zero-arg meaning for infix:<xx>" }
multi infix:<xx>(Mu \x)   {x }
multi infix:<xx>(Mu \x, $n is copy, :$thunked) {
    $n = nqp::p6bool(nqp::istype($n, Whatever)) ?? $Inf !! $n.Int;
    GatherIter.new({ take ($thunked ?? x.() !! x) while $n-- > 0; }, :infinite($n == $Inf)).list
}

proto sub pop(@) {*}
multi sub pop(@a) { @a.pop }

proto sub shift(@) {*}
multi sub shift(@a) { @a.shift }

proto sub unshift(|) {*}
multi sub unshift(\a, *@elems) { a.unshift: @elems }

proto sub push(|) {*}
multi sub push(\a, *@elems) { a.push: @elems }

sub reverse(*@a)            { @a.reverse }
sub rotate(@a, Int $n = 1)  { @a.rotate($n) }
sub reduce (&with, *@list)  { @list.reduce(&with) }
sub splice(@arr, $offset = 0, $size?, *@values) {
    @arr.splice($offset, $size, @values)
}
