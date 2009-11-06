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
                ## We have to use PIR's 'push' here, because map can
                ## mutate the elements of the list, and @args.push()
                ## results in @args getting copies of the elements.
                ## This may all get fixed when we come up with a way
                ## to do partial bindings and not have to check .arity
                ## or .count .
                Q:PIR {
                    $P0 = find_lex '@args'
                    $P1 = find_lex '$_'
                    push $P0, $P1
                };
                if (@args == $arity) {
                    take &expr(|@args);
                    @args = ();
                }
            }
        }
    }

    multi method pick(Int $num is copy = 1, :$replace) {

        $num=floor($num);

        if ($num == 1) {
            return @.list[floor(@.list.elems.rand)];
        }

        my @l;
        if ($replace) {
            @l := @.list;
        }
        else {
            @l = @.list;
        }

        gather {
            while ($num > 0 and @l.elems > 0) {
                my $idx = floor(@l.elems.rand());
                take @l[$idx];
                @l.splice($idx,1) unless $replace;
                --$num;
            }
        }
    }

    multi method pick(Whatever $, :$replace) {
        die "Infinite lazy pick not implemented" if $replace;
        @.pick(@.elems);
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

our List multi sub kv(:@array) is export {
    @array.kv();
}

our List multi map(Code $expr, *@values) {
    @values.map($expr)
}

multi pick(Int $num, :$replace, *@values) {
    @values.pick($num, :$replace);
}

multi pick(Whatever $, :$replace, *@values) {
    @values.pick(*,:$replace);
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
