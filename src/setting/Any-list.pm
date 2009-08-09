class Any is also {
    multi method end() is export { $.list.elems - 1; }

    multi method first(Code $test) {
        return $_ if $test($_) for @.list;

        fail('No values matched');
    }

    our List multi method grep($test) {
        gather {
            take $_ if $_ ~~ $test for @.list;
        }
    }

    our Str multi method join($separator = '') {
        Q:PIR {
            $P0 = self.'list'()
            $P0.'!flatten'()
            $P1 = find_lex '$separator'
            $S1 = $P1
            $S0 = join $S1, $P0
            %r = 'prefix:~'($S0)
        }
    }

    our List multi method map(*&expr) {
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
    multi method max( $values: Code $by = sub { $^a cmp $^b } ) {
         my @list = $values.list;
         return -Inf unless @list.elems;
         if $by.arity < 2 {
            my $transform = $by;
            $by := sub { $transform($^a) cmp $transform($^b) };
         }
         my $res = @list.shift;
         for @list -> $x {
             if (&$by($res, $x) < 0) {
                 $res = $x;
             }
         }
         $res;
     };

     # RT #63700 - parse failed on &infix:<cmp>
    multi method min( $values: Code $by = sub { $^a cmp $^b } ) {
         my @list = $values.list;
         return +Inf unless @list.elems;
         if $by.arity < 2 {
            my $transform = $by;
            $by := sub { $transform($^a) cmp $transform($^b) };
         }
         my $res = @list.shift;
         for @list -> $x {
             if (&$by($res, $x) > 0) {
                 $res = $x;
             }
         }
         $res;
     };


    our List multi method pairs(*@indices) is export {
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

    our List multi method kv() {
        @.keys Z @.values
    }

    multi method reduce(Code $expression is rw) {
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

    method reverse() {
        my @result;
        for @.list {
            @result.unshift($_);
        }
        return @result;
    }
}

multi first(Code $test, *@values) {
    @values.first($test)
}

our List multi grep($test, *@values) {
    @values.grep($test)
}

our Str multi join(Str $separator = '', *@values) {
    @values.join($separator)
}

our List multi sub kv(*@values) is export {
    @values.kv();
}

our List multi map(Code $expr, *@values) {
    @values.map($expr)
}

multi max(Code $by, *@values) {
    @values.max($by);
}

multi min(Code $by, *@values) {
    @values.min($by);
}

multi reduce(Code $expression, *@values) {
    @values.reduce($expression);
}

multi reverse(*@values) {
    @values.reverse;
}

# vim: ft=perl6
