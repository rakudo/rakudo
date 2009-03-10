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
            for @.list {
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
                for (@.list.keys Z @.list) -> $key, $val is rw {
                    take ($key => $val) if $key ~~ any(@indices);
                }
            } else {
                for (@.list.keys Z @.list) -> $key, $val is rw {
                    take ($key => $val)
                }
            }
        }
    }

    multi method reduce( Code $expression is rw) {
        my Int $arity = $expression.count;
        fail('Cannot reduce() using a unary or nullary function.')
            if $arity < 2;

        my $l := @.list;

        fail('Cannot reduce() empty list') unless +$l;

        my @args;
        for $l {
            @args.push($_);
            if (@args == $arity) {
                my $res = $expression.(|@args);
                @args = ($res);
            }
        }
        if @args > 1 {
            if @args < $expression.arity {
                warn (@args -1) ~ " trailing item(s) in reduce";
            } else {
                return $( $expression.(|@args) );
            }
        }
        return @args[0];
    }
}

our List multi grep(Code $test, *@values) {
    @values.grep($test)
}

our List multi map(Code $expr, *@values) {
    @values.map($expr)
}

multi min(Code $by, *@values) {
    @values.min($by);
}

our List multi pairs(@values, *@indices) {
    @values.pairs(@indices)
}

multi reduce(Code $expression, *@values) {
    @values.reduce($expression);
}

# vim: ft=perl6
