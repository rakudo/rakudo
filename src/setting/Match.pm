class Match is also {
    multi method perl() {
        self!_perl(0);
    }

    my method _perl(Int $indent) {
        return [~] gather {
            my $sp = ' ' x ($indent + 1);
            take "Match.new(\n";
            if $indent == 0 {
                take " # WARNING: this is not working perl code\n";
                take " # and for debugging purposes only\n";
            }
            take $sp;
            take "ast  => {$.ast.perl},\n";
            take $sp;
            take "Str => {$.Str.perl},\n";
            take $sp;
            take "from => $.from,\n";
            take $sp;
            take "to   => $.to,\n";
            if @(self) {
                take $sp;
                take "positional => [\n";
                # work around RT #64952
                for ^self.list {
                    take "$sp ";
                    self!_perl_quant(self.[$_], $indent);
                    take ",\n";
                }
                take $sp;
                take "],\n";
            }
            if %(self) {
                take $sp;
                take "named => \{\n";
                for %(self).pairs {
                    take "$sp '{.key}' => ";
                    self!_perl_quant(.value, $indent);
                    take ",\n";
                }
                take "$sp\},\n";
            }
            take ' ' x $indent;
            take ")";
        }
    }

    method !_perl_quant($obj, $indent) {
        my $sp = ' ' x $indent;
        if $obj ~~ undef {
            take 'undef';
        } elsif $obj ~~ Match {
            take $obj!_perl($indent + 3);
        } else {
            take "[\n";
            for $obj.list {
                take $sp ~ '    ';
                take $_!_perl($indent + 5);
                take ",\n";
            }
            take "$sp ]";
        }
    }

    multi method caps() {
        my @caps = gather {
            for self.list.pairs, self.hash.pairs -> $p {
                # in regexes like [(.) ...]+, the capture for (.) is 
                # a List. flatten that.
                if $p.value ~~ List {
                    take ($p.key => $_) for @($p.value);
                } else {
                    take $p;
                }
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
}

# vim: ft=perl6
