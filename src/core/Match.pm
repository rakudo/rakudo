class Match is Regex::Match is Cool does Associative {
    multi method postcircumfix:<{ }>($key) {
        Q:PIR {
            $P0 = find_lex 'self'
            $P1 = find_lex '$key'
            %r = $P0[$P1]
            unless null %r goto done
            %r = new ['Proxy']
            setattribute %r, '$!base', $P0
            setattribute %r, '$!key', $P1
          done:
        }
    }

    # We shouldn't need to provide this -- we should be able to
    # simply write "does Positional" in the class declaration
    # and it would provide us the postcircumfix:<[ ]> methods
    # for free.  But there seems to be a bug or problem in the
    # role composer that prevents us from having both "does Positional"
    # and "does Associative" in the class declaration, so we'll
    # provide the simple .[] for now.
    multi method postcircumfix:<[ ]>(Int $key) {
        Q:PIR {
            $P0 = find_lex 'self'
            $P1 = find_lex '$key'
            $I1 = $P1
            %r = $P0[$I1]
            unless null %r goto done
            %r = new ['Proxy']
            setattribute %r, '$!base', $P0
            setattribute %r, '$!key', $P1
          done:
        }
    }
}
