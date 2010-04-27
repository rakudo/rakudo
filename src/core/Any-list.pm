class Range { ... }

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

    our multi method grep(Mu $test) {
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

    # CHEAT: this should take an ordering parameter
    # And use the FIRST: phaser
    multi method minmax($by = { $^a cmp $^b}) {
        my $min = +Inf;
        my $max = -Inf;
        my $excludes_min = Bool::False;
        my $excludes_max = Bool::False;

        my $first-time = Bool::True;
        for @.list {
            when Range {
                if $first-time {
                    $min = $_.min;
                    $max = $_.max;
                    $excludes_min = $_.excludes_min;
                    $excludes_max = $_.excludes_max;
                    $first-time = Bool::False;
                    next;
                }
                if $by($_.min, $min) == -1 {
                    $min = $_;
                    $excludes_min = $_.excludes_min;
                }
                if $by($_.max, $max) == 1 {
                    $max = $_;
                    $excludes_max = $_.excludes_max;
                }
            }

            if $first-time {
                $min = $_;
                $max = $_;
                $first-time = Bool::False;
                next;
            }
            if $by($_, $min) == -1 {
                $min = $_;
                $excludes_min = Bool::False;
            }
            if $by($_, $max) == 1 {
                $max = $_;
                $excludes_max = Bool::False;
            }
        }
        Range.new($min,
                  $max,
                  :excludes_min($excludes_min),
                  :excludes_max($excludes_max));
    }

    #CHEAT: Simplified version which we can hopefully sneak by ng.
    multi method pick() {
        my @l = @.list.Seq;
        @l[floor(@l.elems.rand)];
    }

    multi method pick($num is copy = 1, :$replace) {
        my @l = @.list.Seq;

        if ($num == 1) {
            return @l[floor(@l.elems.rand)];
        }

        if $replace {
            gather {
                while ($num > 0) {
                    my $idx = floor(@l.elems.rand());
                    take @l[$idx];
                    --$num;
                }
            }
        } else {
            gather {
                while ($num > 0 and @l.elems > 0) {
                    my $idx = floor(@l.elems.rand());
                    take @l[$idx];
                    @l.splice($idx,1);
                    --$num;
                }
            }
        }
    }

    multi method pick(Whatever, :$replace) {
        self.pick(Inf, :$replace);
    }

    multi method reduce(Code $expression is rw) {
        my $arity = $expression.?count || 2; # second half is a CHEAT
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

    # This needs a way of taking a user-defined comparison
    # specifier, but AFAIK nothing has been spec'd yet.
    # CHEAT: Almost certainly should be hashed on something
    # other than the stringification of the objects.
    multi method uniq() {
        my %seen;
        gather for @.list {
             unless %seen{$_} {
                 take $_;
                 %seen{$_} = 1;
             }
        }
    }

    multi method kv() {
        my $i = 0;
        gather for $.list -> $value {
            my $key = $i++;
            take $key;
            take $value;
        }
    }

    multi method keys() {
        my $i = 0;
        gather for $.list -> $value {
            my $key = $i++;
            take $key;
        }
    }

    multi method values() {
        gather for $.list -> $value {
            take $value;
        }
    }

    multi method pairs() {
        self.kv.map(-> $key, $value { $key => $value; });
    }
}

proto sub join (Str $separator = '', *@values) { @values.join($separator); }
proto sub reverse(@values) { @values.reverse; }
multi sub reverse(*@v) { @v.reverse; }
proto sub end(@array) { @array.end; }
proto sub grep(Mu $test, *@values) { @values.grep($test); }
proto sub first($test, @values) { @values.first($test); }
proto sub min($by, *@values) { @values.min($by); }
proto sub max($by, *@values) { @values.max($by); }
proto sub minmax($by, *@values) { @values.minmax($by); }
proto sub uniq(@values) { @values.uniq; }
proto sub pick ($num, :$replace, *@values) { @values.pick($num, :$replace); }
proto sub map(&mapper, @values) { @values.map(&mapper); }
proto sub kv(@array) { @array.kv; }
proto sub keys(@array) { @array.keys; }
proto sub values(@array) { @array.values; }
proto sub pairs(@array) { @array.pairs; }

# vim: ft=perl6
