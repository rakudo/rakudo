augment class List does Positional {
    # N.B.: methods defined in src/builtins/List.pir:
    #    .new, .eager, .elems, .flat, .item, .iterator,
    #    .list, .munch, .perl, !fill

    method Bool() {
        self!fill(1) ?? Bool::True !! Bool::False;
    }

    method Num() {
        self.elems;
    }

    method Str() { self.join(' ') }

    multi method exists(*@indices) {
        return False unless @indices;
        while @indices && @indices.shift -> $key {
            return False if $key < 0
                || !pir::exists__IQi(self!fill($key+1), $key);
        }
        True;
    }

    multi method fmt($format = '%s', $separator = ' ') {
        self.map({ .fmt($format) }).join($separator);
    }

    multi method map(&block) {
        Q:PIR {
            .local pmc self, mapiter, block
            mapiter = new ['MapIter']
            block = find_lex '&block'
            setattribute mapiter, '&!block', block
            self = find_lex 'self'
            $P0 = self.'iterator'()
            $P0 = $P0.'list'()
            setattribute mapiter, '@!list', $P0
            %r = mapiter.'list'()
        }
    }

    method perl() {
        '(' ~ self.map({ $^a.perl }).join(', ') ~ ')';
    }

    method reverse() {
        # XXX: fail if infinite
        Q:PIR {
            .local pmc self, items, parcel
            self = find_lex 'self'
            items = self.'!fill'()
            parcel = new ['Parcel']
            .local int n
            n = elements items
          reverse_loop:
            unless n > 0 goto reverse_done
            dec n
            $P0 = items[n]
            push parcel, $P0
            goto reverse_loop
          reverse_done:
            %r = parcel
        }
    }

    method rotate($n = 1) is export {
        my $k = $n % self.elems;
        self[$k .. self.elems-1, 0 .. $k-1];
    }

    multi method sort(&by = &infix:<cmp>) {
        # Parrot already provides a sort method that works on
        # ResizablePMCArray, so we aim to make use of that here.
        # Instead of sorting the elements directly, we sort an RPA
        # of indices (from 0 to $list.elems), then use that RPA
        # as a slice into self.

        my $index_rpa = pir::new__PS("ResizablePMCArray");
        pir::push__vPP($index_rpa, $_) for ^self.elems;

        # If &by.arity < 2, then it represents a block to be applied
        # to the elements to obtain the values for sorting.
        if (&by.?count || 2) < 2 {
            my $list = self.map(&by).eager;
            self[$index_rpa.sort(
                -> $a, $b { $list[$a] cmp $list[$b] || $a <=> $b }
            ),];
        }
        else {
            my $list = self.eager;
            self[$index_rpa.sort(
                -> $a, $b { &by($list[$a],$list[$b]) || $a <=> $b }
            ),];
        }
    }

    our method pop() {
        self!fill ?? pir::pop(@!items)
                  !! fail('Undefined value popped from empty array');
    }

    our method push(*@values) is export {
        self!fill(0);
        pir::push(@!rest, @values.Seq.eager.iterator );
        self;
    }

    our method shift() is export { 
       self!fill(1) ?? pir::shift(@!items)
                    !! fail('Undefined value shifted from empty list')
    }

    our method unshift(*@values) is export {
        self!splice(@values.Seq.eager, 0, 0);
        self;
    }

    our multi method ACCEPTS($topic) {
        my $sseq = self.Seq;
        my $tseq = $topic.Seq;
        while $sseq {
            # if the next element is Whatever
            if $sseq[0] ~~ Whatever {
                # skip over all of the Whatevers
                $sseq.shift while $sseq && $sseq[0] ~~ Whatever;
                # if nothing left, we're done
                return True if !$sseq;
                # find a target matching our new target
                $tseq.shift while $tseq && $tseq[0] !== $sseq[0];
                # return false if we ran out
                return False if !$tseq;
            }
            elsif !$tseq || $tseq[0] !=== $sseq[0] {
                return False;
            }
            # shift off matching elements
            $sseq.shift;
            $tseq.shift
        }
        # If nothing left to match, we're successful.
        !$tseq;
    }

    method at_pos($pos) {
        Q:PIR {
            .local pmc self, pos, items
            self = find_lex 'self'
            pos  = find_lex '$pos'
            $I0  = pos
            $I1  = $I0 + 1
            items = self.'!fill'($I1)
            %r   = items[$I0]
            unless null %r goto done
            %r   = get_hll_global 'Any'
          done:
        }
    }

    multi method hash() {
        my %h = self;
    }
}


