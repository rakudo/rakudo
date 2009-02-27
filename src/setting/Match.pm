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
                    take "$sp $name => ";
                    take $match!_perl($indent + 3);
                }
                take "$sp\},\n";
            }
            take ' ' x $indent;
            take ")";
        }
    }
}

# vim: ft=perl6
