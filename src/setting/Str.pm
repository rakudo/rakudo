class Str is also {
    our Str multi method reverse ($str: ) is export {
        return $str.split('').reverse.join('');
    }

    our List multi method split(Code $delimiter, $limit = *) {
        my $s = self;
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        return gather {
            while $l > 1 && $s ~~ $delimiter {
                take $s.substr(0, $/.from);
                $s.=substr([max] $/.to, 1);
                $l--;
            }
            take $s if $l > 0;
        }
    }

    # TODO: substitute with '$delimiter as Str' once coercion is implemented
    our List multi method split($delimiter is copy, $limit = *) {
        my Int $prev = 0;
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        $delimiter = ~$delimiter;
        return gather {
            my $pos;
            while $l > 1
                  && $pos < self.chars 
                  && defined ($pos = self.index($delimiter, $prev)) {
                take self.substr($prev, $pos - $prev);
                $prev = [max] 1 + $prev, $pos + $delimiter.chars;
                $l--;
            }
            take self.substr($prev) if $l > 0;
        }
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
