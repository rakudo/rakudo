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

    multi method hash() {
        # nextsame() dies here with 'Null PMC access in clone()'
        CREATE_HASH_LOW_LEVEL(self.Regex::Match::hash);
    }

    multi method list() {
        list(self.Regex::Match::list);
    }

    multi method caps() {
        my @caps = gather {
            for self.list.pairs, self.hash.pairs -> $p {
                # in regexes like [(.) ...]+, the capture for (.) is
                # a List. flatten that.
                if $p.value ~~ Array  {
                    # iterating over an RPA doesn't seem to work
                    # easily, so we iterate over the indexes instead.
                    # Ugly, but it works.
                    take ($p.key => $_) for @($p.value);
                } else {
                    take $p;
                }
            }
        }
        list(@caps.sort({ .value.from }));
    }

    multi method chunks() {
        my $prev = $.from;
        gather {
            for @.caps {
                if .value.from > $prev {
                    take '~' => self.substr($prev - $.from, .value.from - $prev)
                }
                take $_;
                $prev = .value.to;
            }
            take ('~' => self.substr($prev - $.from)) if $prev < $.to;
        }
    }
}

# vim: ft=perl6
