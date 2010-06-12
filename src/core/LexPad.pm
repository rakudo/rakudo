# probably wants to inherit from Stash, which is NYI
class LexPad is Associative {
    has $!parrot_lexpad;

    multi method postcircumfix:<{ }>($thing) {
        Q:PIR {
            .local pmc key
            key = find_lex '$thing'
            $P0 = find_lex 'self'
            $P1 = getattribute $P0, '$!parrot_lexpad'
            $P1 = descalarref $P1
            %r = $P1[key]
            unless null %r goto done
            %r = get_hll_global 'Mu'
          done:
        }
    }

    method keys {
        my @keys;
        Q:PIR {
            .local pmc self, it, res
            self = find_lex 'self'
            res  = find_lex '@keys'
            $P0  = getattribute self, '$!parrot_lexpad'
            it   = iter $P0
          iter_loop:
            unless it goto iter_end
            $P0 = shift it
            push res, $P0
            goto iter_loop
          iter_end:
        };
        @keys;
    }
}

