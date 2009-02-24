class Any is also {
    our List multi method split(Code $delimiter, $limit = *) {
        my $s = ~self;
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        my $keep = '';
        return gather {
            while $l > 1 && $s ~~ $delimiter {
                take $keep ~ $s.substr(0, $/.from);
                if $/.from == $/.to {
                    $keep = $s.substr($/.to, 1);
                    $s.=substr($/.to + 1);
                } else {
                    $keep = '';
                    $s.=substr($/.to)
                }
                $l--;
            }
            take $keep ~ $s if $l > 0;
        }
    }

    # TODO: substitute with '$delimiter as Str' once coercion is implemented
    our List multi method split($delimiter is copy, $limit = *) {
        my Int $prev = 0;
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        my $s = ~self;
        $delimiter = ~$delimiter;
        if $delimiter eq '' {
            return gather {
                take $s.substr($_, 1) for 0 .. $s.chars - 1;
            }
        }
        return gather {
            my $pos;
            while $l > 1
                  && $pos < $s.chars 
                  && defined ($pos = $s.index($delimiter, $prev)) {
                take $s.substr($prev, $pos - $prev);
                $prev = [max] 1 + $prev, $pos + $delimiter.chars;
                $l--;
            }
            take $s.substr($prev) if $l > 0;
        }
    }

    our List multi method comb (Code $matcher = /\S+/, $limit = *) {
        my $l = $limit ~~ Whatever ?? Inf !! $limit;
        # currently we use a copy of self and destroy it piece by piece.
        # the preferred way of doing it is using self, not destroying it,
        # and use the :pos modifier to the regex. That way the offsets into
        # self will be right
        my $s = ~self;
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
