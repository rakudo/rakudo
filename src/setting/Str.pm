class Str is also {
    our Str multi method reverse ($str: ) is export {
        return $str.split('').reverse.join('');
    }

    our List multi method comb (Code $matcher = /\S+/, $limit = *) {
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        # currently we use a copy of self and destroy it piece by piece.
        # the preferred way of doing it is using self, not destroying it,
        # and use the :pos modifier to the regex. That way the offsets into
        # self will be right
        my $s = self;
        return gather {
            while $l > 0 && $s ~~ $matcher {
                # if we have captures, return the actual match object
                take @($/) || %($/) ?? $/.clone !! ~$/;
                $l--;
                $s.=substr([max] 1, $/.to);
            }
        }
    }
}

# vim: ft=perl6
