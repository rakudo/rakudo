my class Match is Capture is Cool {
    has $.orig;
    has int $.from;
    has int $.to;
    has $.CURSOR;
    has $.made;

    # new/BUILD here only for performance reasons
    method new(:$orig,:$from,:$to,:$CURSOR,:$made) {
        nqp::create(self).BUILD(:$orig,:$from,:$to,:$CURSOR,:$made);
    }
    method BUILD(:$!orig,:$from,:$to,:$!CURSOR,:$!made) {
        $!from   = $from // 0;  # cannot assign to int in sig
        $!to     = $to   // 0;  # cannot assign to int in sig
        self;
    }

    method ast(Match:D:) { $!made }

    multi method Str(Match:D:) {
        $!to > $!from ?? substr($!orig,$!from,$!to-$!from) !! ''
    }

    multi method Numeric(Match:D:) {
        self.Str.Numeric
    }
    multi method Bool(Match:D:) {
        $!to >= $!from
    }
    multi method ACCEPTS(Match:D: Any $) { self }

    method prematch(Match:D:) {
        substr($!orig,0,$!from);
    }
    method postmatch(Match:D:) {
        substr($!orig,$!to)
    }

    method caps(Match:D:) {
        my @caps;
        for self.pairs -> $p {
            if nqp::istype($p.value,Array) {
                @caps.push: $p.key => $_ for $p.value.list
            } elsif $p.value.DEFINITE {
                @caps.push: $p
            }
        }
        @caps.sort: -> $a { $a.value.from +< 32 + $a.value.to }
    }

    method chunks(Match:D:) {
        my $prev = $!from;
        gather {
            for self.caps {
                if .value.from > $prev {
                    take '~' => substr($!orig,$prev, .value.from - $prev)
                }
                take $_;
                $prev = .value.to;
            }
            take '~' => substr($!orig,$prev, $!to - $prev) if $prev < $!to;
        }
    }

    multi method perl(Match:D:) {
        my %attrs;
        for <orig from to ast list hash> {
            %attrs{$_} = self."$_"().perl;
        }

        'Match.new('
            ~ %attrs.fmt('%s => %s', ', ')
            ~ ')'
    }
    multi method gist (Match:D: $d = 0) {
        return "#<failed match>" unless self;
        my $s = ' ' x ($d + 1);
        my $r = ("=> " if $d) ~ "\x[FF62]{self}\x[FF63]\n";
        for @.caps {
            $r ~= $s ~ (.key // '?') ~ ' ' ~ .value.gist($d + 1)
        }
        $d == 0 ?? $r.chomp !! $r;
    }

    method make(Match:D: Mu \made) {
        $!made := made;
        nqp::bindattr(
            nqp::decont(self.CURSOR),
            Cursor,
            '$!made',
            made
        );
    }
}

sub make(Mu \made) {
    my $slash := nqp::getlexcaller('$/');
    nqp::bindattr( nqp::decont($slash),        Match,  '$!made', made );
    nqp::bindattr( nqp::decont($slash.CURSOR), Cursor, '$!made', made );
}


# vim: ft=perl6 expandtab sw=4
