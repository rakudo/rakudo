my class List does Positional {
    # declared in BOOTSTRAP.pm:
    #   is Iterable;           # parent class
    #   has Mu $!items;        # RPA of our reified elements
    #   has $!flattens;        # true if this list flattens its parcels
    #   has $!nextiter;        # iterator for generating remaining elements

    method new(|) {
        my Mu $args := pir::perl6_current_args_rpa__P();
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
        $!nextiter.defined ?? nqp::p6box_n('Inf') !! $n
    }

    method exists(\pos) {
        self.gimme(pos + 1);
        nqp::p6bool(nqp::existspos($!items, nqp::unbox_i(pos)))
    }

    method gimme($n) {
        # loop through iterators until we have at least $n elements
        my int $count = nqp::elems(nqp::p6listitems(self));
        my $eager = nqp::p6bool(nqp::istype($n, Whatever) || $n == $Inf);
        while $!nextiter.defined && ($eager 
                                       ?? !$!nextiter.infinite 
                                       !! ($count < $n)) {
            $!nextiter.reify($eager ?? Whatever !! $n - $count);
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
            pir::perl6_shiftpush__0PPi(nqp::list(), $!items, nqp::unbox_i($n)),
            Any
        )
    }

    method pick($n is copy = 1) {
        ## We use a version of Fisher-Yates shuffle here to
        ## replace picked elements with elements from the end
        ## of the list, resulting in an O(n) algorithm.
        my $elems = self.elems;
        return unless $elems;
        fail ".pick from infinite list NYI" if $!nextiter.defined;
        $n = +$Inf if nqp::istype($n, Whatever);
        $n = $elems if $n > $elems;
        return self.at_pos($elems.rand.floor) if $n == 1;
        my Mu $rpa := nqp::clone($!items);
        my $i;
        my Mu $v;
        gather while $n > 0 {
            $i = $elems.rand.floor.Int;
            $elems--; $n--;
            $v := nqp::atpos($rpa, nqp::unbox_i($i));
            # replace selected element with last unpicked one
            nqp::bindpos($rpa, nqp::unbox_i($i),
                         nqp::atpos($rpa, nqp::unbox_i($elems)));
            take-rw $v;
        }
    }

    method pop() is rw {
        my $elems = self.elems;
        fail '.pop from an infinite list NYI' if $!nextiter.defined;
        $elems > 0
          ?? nqp::pop($!items)
          !! fail 'Element popped from empty list';
    }

    multi method push(List:D: *@values) {
        my $pos = self.elems;
        fail '.push on infinite lists NYI' if $!nextiter.defined;
        self.STORE_AT_POS($pos++, @values.shift) while @values.gimme(1);
        self;
    }

    method roll($n is copy = 1) {
        my $elems = self.elems;
        return unless $elems;
        fail ".roll from infinite list NYI" if $!nextiter.defined;
        $n = +$Inf if nqp::istype($n, Whatever);
        return self.at_pos($elems.rand.floor) if $n == 1;
        gather while $n > 0 {
            take nqp::atpos($!items, nqp::unbox_i($elems.rand.floor.Int));
            $n--;
        }
    }

    method reverse() {
        self.gimme(*);
        fail 'Cannot reverse an infinite list' if self.infinite;
        my Mu $rev  := nqp::list();
        my Mu $orig := nqp::clone($!items);
        nqp::push($rev, nqp::pop($orig)) while $orig;
        my $rlist := nqp::create(self.WHAT);
        nqp::bindattr($rlist, List, '$!items', $rev);
        $rlist;
    }

    method rotate(Int $n is copy = 1) {
        self.gimme(*);
        fail 'Cannot rotate an infinite list' if self.infinite;
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
        $offset += $elems if ($offset < 0);
        X::OutOfRange.new(
            what => 'offset argument to List.splice',
            got  => $offset,
            range => (-self.elems..^self.elems),
        ).fail if $o < 0;
        $s //= self.elems - ($o min $elems);
        $s = self.elems + $s - $o if ($s < 0);
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
            $index_rpa.sort(-> $a, $b { $list[$a] cmp $list[$b] || $a <=> $b });
        }
        else {
            my $list = self.eager;
            $index_rpa.sort(-> $a, $b { $by($list[$a],$list[$b]) || $a <=> $b });
        }
        self[$index];
    }

    multi method ACCEPTS(List:D: $topic) {
        my @t = $topic.list;
        # TODO: Whatever-DWIMmery
        return False unless self.elems == @t.elems;
        for ^self.elems {
            return False unless self.at_pos($_) === @t[$_];
        }
        True;
    }

    method classify(&test) {
        my %result;
        for @.list {
            %result{test $_}.push: $_;
        }
        %result
    }

    method categorize(&test) {
        my %result;
        for @.list {
            my @k = test $_;
            for @k -> $k {
                %result{$k} //= [];
                %result{$k}.push: $_;
            }
        }
        %result.pairs;
    }

    # This needs a way of taking a user-defined comparison
    # specifier, but AFAIK nothing has been spec'd yet.
    method uniq() {
        my $seen := nqp::hash();
        gather sink for @.list {
             my str $which = nqp::unbox_s($_.WHICH);
             unless nqp::existskey($seen, $which) {
                 take $_;
                 nqp::bindkey($seen, $which, 1);
             }
        }
    }

    multi method gist(List:D:) { self.Str }
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

    multi method DUMP(List:D:) {
        self.DUMP-ID() ~ '('
          ~ ("\x221e " if self.infinite) ~
          ~ ':items(' ~ DUMP($!items) ~ '), '
          ~ ':nextiter(' ~ DUMP($!nextiter) ~ ')'
          ~ ')'
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
}

sub eager(|) {
    nqp::p6parcel(pir::perl6_current_args_rpa__P(), Any).eager
}

sub flat(|) {
    nqp::p6list(pir::perl6_current_args_rpa__P(), List, Bool::True)
}

sub list(|) {
    nqp::p6list(pir::perl6_current_args_rpa__P(), List, Mu)
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
sub categorize(&mapper, *@a){ @a.categorize(&mapper)}
sub splice(@arr, $offset = 0, $size?, *@values) {
    @arr.splice($offset, $size, @values)
}
