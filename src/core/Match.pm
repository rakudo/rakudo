my class Match is Capture is Cool {
    has $.orig;
    has $.from;
    has $.to;
    has $.CURSOR;
    has $.ast;

    multi method Str(Match:D:) {
        $!to > $!from ?? $!orig.substr($!from, $!to-$!from) !! ''
    }
    multi method Numeric(Match:D:) {
        self.Str.Numeric
    }
    multi method Bool(Match:D:) {
        $!to >= $!from
    }
    multi method ACCEPTS(Match:D: Any $) { self }

    method prematch(Match:D:) {
        $!orig.substr(0, $!from);
    }
    method postmatch(Match:D:) {
        $!orig.substr($!to)
    }

    method caps(Match:D:) {
        my @caps;
        for self.pairs -> $p {
            if $p.value ~~ Parcel {
                @caps.push: $p.key => $_ for $p.value.list
            } else {
                @caps.push: $p;
            }
        }
        @caps.sort: -> $p { $p.value.from }
    }

    method chunks(Match:D:) {
        my $prev = $!from;
        gather {
            for self.caps {
                if .value.from > $prev {
                    take '~' => $!orig.substr($prev, .value.from - $prev)
                }
                take $_;
                $prev = .value.to;
            }
            take '~' => $!orig.substr($prev, $!to - $prev) if $prev < $!to;
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
        my $r = "=> <{self}>\n";
        for @.caps {
            $r ~= $s ~ (.key // '?') ~ ' ' ~ .value.gist($d + 1)
        }
        $r;
    }

    method make(Match:D: Mu $ast) {
        $!ast = $ast;
        nqp::bindattr(
            nqp::p6decont(self.CURSOR),
            Cursor,
            '$!ast',
            $ast
        );
    }
}

sub make(Mu $ast) {
    nqp::bindattr(
        nqp::p6decont(pir::find_caller_lex__Ps('$/')),
        Match,
        '$!ast',
        $ast
    );
    nqp::bindattr(
        nqp::p6decont(pir::find_caller_lex__Ps('$/').CURSOR),
        Cursor,
        '$!ast',
        $ast
    );
}

