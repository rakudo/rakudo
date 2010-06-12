class Match is Regex::Match is Cool does Associative {
    method ast() {
        my $x = self.Regex::Match::ast;
        pir::isa__IPs($x, 'Undef') ?? Any !! $x;
    }

    method Str() {
        ~self.Regex::Match::Str;
    }

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
        CREATE_HASH_FROM_LOW_LEVEL(self.Regex::Match::hash);
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
                    for $p.value.list {
                        take $p.key => $_;
                    }
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

    multi method perl() {
        self!_perl(0);
    }

    method !_perl(Int $indent) {
        return [~] gather {
            my $sp = ' ' x $indent;
            take $sp;
            take "Match.new(\n";
            if $indent == 0 {
                take " # WARNING: this is not working perl code\n";
                take " # and for debugging purposes only\n";
            }
            take "$sp ast  => {$.ast.perl},\n" unless $.ast === Any;
            take "$sp from => $.from,\n";
            take "$sp orig => $.orig.perl(),\n";
            take "$sp to   => $.to,\n";
            if @(self) {
                take "$sp positional => [\n";
                # work around RT #64952
                for ^self.list {
                    self!_perl_quant(self.[$_], $indent);
                    take ",\n";
                }
                take "$sp  ],\n";
            }
            if %(self) {
                take "$sp named => \{\n";
                for %(self).pairs {
                    take "$sp  {.key} => ";
                    self!_perl_quant(.value, $indent);
                    take ",\n";
                }
                take "$sp \},\n";
            }
            take "$sp)";
        }
    }

    method !_perl_quant($obj, $indent) {
        my $sp = ' ' x $indent;
        if !defined($obj) {
            # do nothing; we only get here when something's not quite right...
        } elsif $obj ~~ Match {
            take $obj!_perl($indent + 3);
        } else {
            take "[\n";
            for $obj.list {
                take $_!_perl($indent + 4);
                take ",\n";
            }
            take "$sp  ]";
        }
    }

}

# vim: ft=perl6
