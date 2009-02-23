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

    our List multi method split(Code $delimiter, Int $limit) {
        self.split($delimiter).[0..$limit];
    }
}

# vim: ft=perl6
