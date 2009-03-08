class Any is also {
    our List multi method grep($values: Code $test) {
        gather {
            take $_ if $test($_) for $values.list;
        }
    }

    our List multi method map(Code *&expr) {
        return gather {
            my $arity = &expr.arity || 1;
            my @args;
            for self.list {
                @args.push($_);
                if (@args == $arity) {
                    take &expr(|@args);
                    @args = ();
                }
            }
        }
    }

     # RT #63700 - parse failed on &infix:<cmp>
    multi method min( $values: Code $by = sub { $^a cmp $^b } ) {
         my @list = $values.list;
         return +Inf unless @list.elems;
         my $res = @list.shift;
         for @list -> $x {
             if (&$by($res, $x) > 0) {
                 $res = $x;
             }
         }
         $res;
     };


    multi method pairs(*@indices) {
        gather {
            if @indices {
                for (self.list.keys Z self.list) -> $key, $val is rw {
                    take ($key => $val) if $key ~~ any(@indices);
                }
            } else {
                for (self.list.keys Z self.list) -> $key, $val is rw {
                    take ($key => $val)
                }
            }
        }
    }
}

our List multi grep(Code $test, *@values) {
    @values.grep($test)
}

our List multi map(Code $expr, *@values) {
    @values.map($expr)
}

our List multi pairs(@values, *@indices) {
    @values.pairs(@indices)
}

multi min(Code $by, *@values) {
    @values.min($by);
}

# vim: ft=perl6
