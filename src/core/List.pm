augment class List does Positional {

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
}


