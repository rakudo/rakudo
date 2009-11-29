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

    method reverse() {
        my @result;
        for @.list {
            @result.unshift($_);
        }
        return @result;
    }
}

# vim: ft=perl6
