augment class Any {

    our Str multi method join($separator = '') {
        pir::join__SsP($separator, self.list.eager);
    }

    our multi method map(&block) {
        Q:PIR {
            .local pmc self, block, map
            self = find_lex 'self'
            block = find_lex '&block'
            $P0 = self.'list'()
            $P0 = $P0.'iterator'()
            map = new ['MapIterator']
            setattribute map, '$!iter', $P0
            setattribute map, '&!block', block
            %r = map
        };
    }

    multi method first($test) {
        for @.list {
            if $_ ~~ $test {
                return $_;
            }
        }

        fail('No values matched');
    }

    our multi method grep($test) {
        gather {
            for @.list {
                take $_ if $_ ~~ $test;
            }
        }
    }

    multi method reverse() {
        my @result = ();
        for @.list {
            @result.unshift($_);
        }
        return @result;
    }

    multi method end() { self.elems - 1; }

    # CHEAT: this should take an ordering parameter
    # And use the FIRST: phaser
    multi method min($by = { $^a cmp $^b}) {
        my $min = +Inf;
        my $first-time = Bool::True;
        for @.list {
            if $first-time {
                $min = $_;
                $first-time = Bool::False;
                next;
            }
            if $by($_, $min) == -1 {
                $min = $_;
            }
        }
        $min;
    }

    # CHEAT: this should take an ordering parameter
    # And use the FIRST: phaser
    multi method max($by = { $^a cmp $^b}) {
        my $max = -Inf;
        my $first-time = Bool::True;
        for @.list {
            if $first-time {
                $max = $_;
                $first-time = Bool::False;
                next;
            }
            if $by($_, $max) == 1 {
                $max = $_;
            }
        }
        $max;
    }

    #CHEAT: Simplified version which we can hopefully sneak by ng.
    multi method pick() {
        my @l = @.list.Seq;
        @l[floor(@l.elems.rand)];
    }

    # multi method pick($num is copy = 1, :$replace) {
    #     if $num ~~ Whatever {
    #         $num = +Inf;
    #     }
    #     # $num .= floor;
    #
    #     my @l = @.list.Seq;
    #
    #     if ($num == 1) {
    #         return @l[floor(@l.elems.rand)];
    #     }
    #
    #     if $replace {
    #         gather {
    #             while ($num > 0) {
    #                 my $idx = floor(@l.elems.rand());
    #                 take @l[$idx];
    #                 --$num;
    #             }
    #         }
    #     } else {
    #         die "Non-replacing pick not yet implemented";
    #         # gather {
    #         #     while ($num > 0 and @l.elems > 0) {
    #         #         my $idx = floor(@l.elems.rand());
    #         #         take @l[$idx];
    #         #         @l.splice($idx,1);
    #         #         --$num;
    #         #     }
    #         # }
    #     }
    # }

    multi method reduce(Code $expression is rw) {
        my $arity = $expression.count;
        fail('Cannot reduce() using a unary or nullary function.')
            if $arity < 2;
        fail('Can only reduce() using a binary function for now.')
            if $arity > 2;

        my @args = ();
        for @.list {
            @args.push($_);
            if (@args == $arity) {
                my $res = $expression.(@args[0], @args[1]);
                # my $res = $expression.(|@args);
                @args = ($res);
            }
        }

        fail('Cannot reduce() empty list') unless @args > 0;

        if @args > 1 {
            if @args < $expression.arity {
                warn (@args -1) ~ " trailing item(s) in reduce";
            } else {
                return $( $expression.(@args[0], @args[1]) );
                # return $( $expression.(|@args) );
            }
        }
        return @args[0];
    }
}

our proto sub join (Str $separator = '', *@values) { @values.join($separator); }
our proto sub reverse(@values) { @values.reverse; }
our multi sub reverse(*@v) { @v.reverse; }
our proto sub end(@array) { @array.end; }
our proto sub grep($test, @values) { @values.grep($test); }
our proto sub first($test, @values) { @values.first($test); }

# vim: ft=perl6
