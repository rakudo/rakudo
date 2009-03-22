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
            take "text => {$.text.perl},\n";
            take $sp;
            take "from => $.from,\n";
            take $sp;
            take "to   => $.to,\n";
            if @(self) {
                take $sp;
                take "positional => [\n";
                for @(self) {
                    take "$sp ";
                    take $_!_perl($indent + 4);
                    take ",\n";
                }
                take $sp;
                take "],\n";
            }
            if %(self) {
                take $sp;
                take "named => \{\n";
                for %(self).kv -> $name, $match {
                    take "$sp '$name' => ";
                    # XXX why is this a Str, not a Match?
                    if $match ~~ Match {
                        take $match!_perl($indent + 3);
                    } else {
                        take $match.perl;
                    }
                    take ",\n";
                }
                take "$sp\},\n";
            }
            take ' ' x $indent;
            take ")";
        }
    }

    multi method ast() {
        return $(self);
    }

    multi method caps() {
        my @caps = @(self), %(self).values;
        # in regexes like [(.) ...]+, the capture for (.) is a List
        # flatten that.
        @caps = @caps.map: { $_ ~~ List ?? @($_) !! $_ };
        return @caps.sort({ .from });
    }

    multi method chunks() {
        my $prev = 0;
        gather {
            for @.caps {
                if .from > $prev {
                    take self.substr($prev, .from - $prev)
                }
                take $_;
                $prev = $_.to;
            }
            take self.substr($prev) if $prev < self.chars;
        }
    }
}

# vim: ft=perl6
