class Any is also {
    our List multi method grep($values: Code $test) {
        gather {
            take $_ if $test($_) for $values.list;
        }
    }

    our List of Capture multi method map(List @values: Code *&expr) {
        gather {
            my $i = 0;
            while ($i <= @values.end) {
                my @args;
                @args.push(($i <= @values.end) ?? @values[$i++] !! undef)
                    for (1..&expr.arity || 1);

                take &expr(|@args);
            }
        }
    }

    our List of Capture multi method map($value: Code *&expr) {
        ($value,).map: &expr
    }

     # RT #63700 - parse failed on &infix:<cmp>
     our Array multi method min( $values: Code $by = sub { $^a cmp $^b } ) {
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


    our List multi method pairs(@values: *@indices) {
        gather {
            for (@values.keys Z @values) -> $key, $val is rw {
                take ($key => $val)
                    unless (@indices && ($key !~~ any(@indices)));
            }
        }
    }
}

our List multi grep(Code $test, *@values) {
    @values.grep($test)
}

our List of Capture multi map(Code $expr, *@values) {
    @values.map($expr)
}

our List multi pairs(@values, *@indices) {
    @values.pairs(@indices)
}

our List multi min(*@values) {
    my $by = @values[0] ~~ Code ?? shift @values !! sub { $^a cmp $^b };
    @values.min($by);
}

# vim: ft=perl6
