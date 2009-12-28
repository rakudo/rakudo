augment class Any {

    our Str multi method join($separator = '') {
        pir::join__SsP($separator, self.list);
    }

    our multi method map(&block) {
        Q:PIR {
            .local pmc mapper
            mapper = new ['!Mapper']
            $P0 = find_lex 'self'
            $P0 = iter $P0
            setattribute mapper, '$!list_it', $P0
            $P0 = find_lex '&block'
            setattribute mapper, '&!block', $P0
            $P0 = get_hll_global ['Bool'], 'True'
            setprop mapper, 'flatten', $P0
            %r = '&list'(mapper)
        }
    }

    multi method first($test) {
        for @.list {
            if $_ ~~ $test {
                return $_;
            }
        }

        fail('No values matched');
    }

    our List multi method grep($test) {
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

our List sub infix:<xx>($item, $n) {
    if $item.isa('List') {
        if $item.list.elems == 0 || $n <= 0 { return List };
        gather {
            for 1 ... $n { for 0 ... $item.list.end { take $item.list.[$_] } }
        };
    }
    else {
        gather { for 1 ... $n { take $item } };
    }
}

# vim: ft=perl6
