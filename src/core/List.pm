class Range { ... }

class List does Positional {
    # declared in BOOTSTRAP.pm:
    #   is Iterable;           # parent class
    #   has Mu $!items;        # RPA of our reified elements
    #   has $!flattens;        # true if this list flattens its parcels
    #   has $!nextiter;        # iterator for generating remaining elements

    method new(|$) {
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
    method flattens() { $!flattens }
    
    method Capture() {
        self.gimme(*);
        my $cap := nqp::create(Capture);
        nqp::bindattr($cap, Capture, '$!list', $!items);
        $cap
    }

    method Parcel() {
        pir::defined($!items) or 
            nqp::bindattr(self, List, '$!items', nqp::list());
        my Mu $rpa := nqp::clone($!items);
        nqp::push($rpa, $!nextiter) if $!nextiter.defined;
        nqp::p6parcel($rpa, Any);
    }

    method at_pos($pos is copy) is rw {
        $pos = $pos.Int;
        self.exists($pos)
          ?? nqp::atpos($!items, nqp::unbox_i($pos))
          !! Mu
    }

    method eager() { self.gimme(*); self }

    method elems() {
        # Get as many elements as we can.  If gimme stops before
        # reaching the end of the list, assume the list is infinite.
        my $n = self.gimme(*);
        $!nextiter.defined ?? nqp::p6box_n('Inf') !! $n
    }

    method exists(\$pos) {
        self.gimme($pos + 1);
        nqp::p6bool(nqp::existspos($!items, nqp::unbox_i($pos)))
    }

    method gimme($n) {
        # create $!items RPA if it doesn't already exist
        pir::defined($!items) or 
            nqp::bindattr(self, List, '$!items', nqp::list());

        # loop through iterators until we have at least $n elements
        my $count = nqp::p6box_i(nqp::elems($!items));
        my $eager = Whatever.ACCEPTS($n);
        while $!nextiter.defined && ($eager 
                                       ?? !$!nextiter.infinite 
                                       !! ($count < $n)) {
            $!nextiter.reify($eager ?? Whatever !! $n - $count);
            $count = nqp::p6box_i(nqp::elems($!items));
        }

        # return the number of elements we have now
        $count
    }

    method infinite() { 
        $!nextiter.defined && $!nextiter.infinite;
    }

    method iterator() {
        # Return a reified ListIter containing our currently reified elements
        # and any subsequent iterator.
        my $iter := nqp::create(ListIter);
        nqp::bindattr($iter, ListIter, '$!nextiter', $!nextiter);
        nqp::bindattr($iter, ListIter, '$!reified', self.Parcel());
        $iter;
    }

    method munch(\$n) {
        self.gimme($n) if nqp::not_i(nqp::istype($n, Int))
                          || nqp::not_i(pir::defined($!items))
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

    method push(*@values) {
        my $pos = self.elems;
        fail '.push on infinite lists NYI' if $!nextiter.defined;
        self.STORE_AT_POS($pos++, @values.shift) while @values;
        self;
    }

    method roll($n is copy = 1) {
        my $elems = self.elems;
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
        fail 'Cannot reverse and infinite list' if self.infinite;
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
        self.gimme(1) 
          ?? nqp::shift($!items) 
          !! fail 'Element shifted from empty list';
    }

    method unshift(*@elems) {
        while @elems.pop -> $e {
            nqp::unshift($!items, $e)
        }
        self
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
            my $k = test $_;
            %result{$k} //= [];
            %result{$k}.push: $_;
        }
        %result.pairs;
    }

    multi method gist(List:D:) { self.Str }
    multi method perl(List:D \$self:) {
        self.gimme(*);
        self.Parcel.perl ~ '.list'  
          ~ (nqp::iscont($self) ?? '.item' !! '')
    }

    method REIFY(Parcel \$parcel) {
        nqp::splice($!items, nqp::getattr($parcel, Parcel, '$!storage'),
                    nqp::elems($!items), 0);
        $parcel
    }

    method STORE_AT_POS(\$pos, Mu \$v) is rw {
        nqp::bindpos($!items, nqp::unbox_i($pos), $v)
    }

    method ARGLIST_FLATTENABLE() { self.gimme(*); $!items }

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
        self
    }
    method pairs(List:D:) {
        self.keys.map: {; $_ => self.at_pos($_) };
    }
    method kv(List:D:) {
        self.keys.map: { ($_, self.at_pos($_)) };
    }

    method reduce(List:D: &with) {
        fail('can only reduce with arity 2 for now')
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

sub eager(|$) {
    nqp::p6parcel(pir::perl6_current_args_rpa__P(), Any).eager
}

sub flat(|$) {
    nqp::p6list(pir::perl6_current_args_rpa__P(), List, Bool::True)
}

sub list(|$) {
    nqp::p6list(pir::perl6_current_args_rpa__P(), List, Mu)
}

proto infix:<xx>(|$)     { * }
multi infix:<xx>()       { fail "No zero-arg meaning for infix:<xx>" }
multi infix:<xx>(Mu \$x) { $x }
multi infix:<xx>(Mu \$x, $n is copy) {
    $n = $Inf if Whatever.ACCEPTS($n);
    GatherIter.new({ take $x while $n-- > 0; }, :infinite($n == $Inf)).list
}

proto sub pop(|$) {*}
multi sub pop(@a) { @a.pop }

proto sub shift(|$) {*}
multi sub shift(@a) { @a.shift }

proto sub unshift(|$) {*}
multi sub unshift(@a, *@elems) { @a.unshift: @elems }

proto sub push(|$) {*}
multi sub push(@a, *@elems) { @a.push: @elems }

sub reverse(*@a)            { @a.reverse }
sub rotate(@a, Int $n = 1)  { @a.rotate($n) }
sub reduce (&with, *@list)  { @list.reduce(&with) }
