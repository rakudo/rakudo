class Match is Regex::Match is Cool does Positional does Associative {
    method ast() {
        my $x = self.Regex::Match::ast;
        pir::isa__IPs($x, 'Undef') ?? Any !! $x;
    }

    method Str() {
        ~self.Regex::Match::Str;
    }


    method at_key($key) {
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

    method at_pos($pos) {
        Q:PIR {
            $P0 = find_lex 'self'
            $P1 = find_lex '$pos'
            $I1 = $P1
            %r = $P0[$I1]
            unless null %r goto done
            %r = new ['Proxy']
            setattribute %r, '$!base', $P0
            setattribute %r, '$!key', $P1
          done:
        }
    }

    method of() { Mu }

    multi method hash() {
        # nextsame() dies here with 'Null PMC access in clone()'
        CREATE_HASH_FROM_LOW_LEVEL(pir::descalarref__PP(self).Regex::Match::hash);
    }

    multi method list() {
        Seq.new(pir::descalarref__PP(self).Regex::Match::list);
    }

    multi method caps() {
        my @caps;
        for self.list.pairs, self.hash.pairs -> $p {
            # in regexes like [(.) ...]+, the capture for (.) is
            # a List. flatten that.
            if $p.value ~~ Array  {
                for $p.value.list {
                    @caps.push: $p.key => $_;
                }
            } else {
                @caps.push: $p;
            }
        }
        @caps.sort({ .value.from });
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
