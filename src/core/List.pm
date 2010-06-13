augment class List does Positional {
    method Bool() {
        self!fill(1) ?? Bool::True !! Bool::False;
    }

    method Num() {
        self.elems;
    }

    method Str() {
        pir::join(' ', self!fill);
    }

    multi method map(&block) {
        Q:PIR {
            .local pmc mapiter, block, list, maplist
            mapiter = new ['MapIter']
            block = find_lex '&block'
            setattribute mapiter, '&!block', block
            $P0 = find_lex 'self'
            list = $P0.'list'()
            setattribute mapiter, '@!list', list
            %r = mapiter.'list'()
        }
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


