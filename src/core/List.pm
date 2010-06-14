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

    method Seq() {
        Seq.new(self).eager
    }

    method Str() {
        pir::join(' ', self!fill);
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

    our multi method postcircumfix:<[ ]>($index) {
        Q:PIR {
            .local pmc self, items
            self = find_lex 'self'
            $P0 = find_lex '$index'
            $I0 = $P0
            $I1 = $I0 + 1
            items = self.'!fill'($I1)
            %r = items[$I0]
        }
    }
}


