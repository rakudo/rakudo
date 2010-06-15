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

    method Str() {
        pir::join(' ', self!fill);
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
        if (&by.?arity // 2) < 2 {
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

    our multi method postcircumfix:<[ ]>(Int $index) {
        Q:PIR {
            .local pmc self, items
            self = find_lex 'self'
            $P0 = find_lex '$index'
            $I0 = $P0
            $I1 = $I0 + 1
            items = self.'!fill'($I1)
            %r = items[$I0]
            unless null %r goto done
            %r = new ['Perl6Scalar']
          done:
        }
    }
}


