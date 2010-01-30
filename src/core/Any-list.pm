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
        my @result;
        for @.list {
            @result.unshift($_);
        }
        return @result;
    }

    multi method end() { self.elems - 1; }
}

our proto sub join (Str $separator = '', *@values) { @values.join($separator); }
our proto sub reverse(@values) { @values.reverse; }
our multi sub reverse(*@v) { @v.reverse; }
our proto sub end(@array) { @array.end; }
our proto sub grep($test, @values) { @values.grep($test); }
our proto sub first($test, @values) { @values.first($test); }

# vim: ft=perl6
