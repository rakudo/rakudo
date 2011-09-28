my class Match is Capture {
    has $.orig;
    has $.from;
    has $.to;
    has $.CURSOR;
    has $.ast;

    multi method gist(Match:D:) {
        $!to > $!from ?? $!orig.substr($!from, $!to-$!from) !! ''
    }
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

    method perl(Match:D:) {
        my %attrs;
        for <orig from to ast list hash> {
            %attrs{$_} = self."$_"().perl;
        }

        'Match.perl('
            ~ %attrs.fmt('%s => %s', ', ')
            ~ ')'
    }
}

sub make(Mu $ast) {
    nqp::bindattr(
        pir::perl6_decontainerize__PP(pir::find_caller_lex__Ps('$/')),
        Match,
        '$!ast',
        $ast
    );
}

