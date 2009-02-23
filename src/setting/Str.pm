class Str is also {
    our Str multi method reverse ($str: ) is export {
        return $str.split('').reverse.join('');
    }

    our List multi method split(Code $delimiter) {
        my $s = self;
        return gather {
            # XXX is this valid in Perl 6? or just a Rakudoism?
            while $s ~~ $delimiter {
                take $s.substr(0, $/.from);
                $s.=substr([max] $/.to, 1);
            }
            take $s;
        }
    }

    our List multi method split($delimiter, Int $limit) {
        self.split($delimiter).[0..$limit];
    }

    # TODO: substitute with '$delimiter as Str' once coercion is implemented
    our List multi method split($delimiter is copy) {
        my Int $prev = 0;
        $delimiter = ~$delimiter;
        return gather {
            my $pos;
            while defined ($pos = self.index($delimiter, $prev)) {
                take self.substr($prev, $pos - $prev);
                $prev = $pos + $delimiter.chars;
            }
            take self.substr($prev);
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
