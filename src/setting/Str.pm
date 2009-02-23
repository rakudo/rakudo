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
            # work around a rakudo bug: "102030405".index(0, 10).defined is True
            while $prev <= self.chars 
                  && defined ($pos = self.index($delimiter, $prev)) {
                take self.substr($prev, $pos - $prev);
                $prev = $pos + $delimiter.chars;
            }
            take self.substr($prev);
        }
    }
}

# vim: ft=perl6
