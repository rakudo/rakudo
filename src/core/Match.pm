my class Match is Capture is Cool {
    has $.orig;
    has int $.from;
    has int $.to;
    has $.CURSOR;
    has $.made;

    # new/!SET-SELF here only for performance reasons
    method !SET-SELF($!orig,$from,$to,$!CURSOR,$!made) {
        $!from   = $from // 0;  # cannot assign to int in sig
        $!to     = $to   // 0;  # cannot assign to int in sig
        self;
    }
    method new(:$orig,:$from,:$to,:$CURSOR,:$made) {
        nqp::create(self)!SET-SELF($orig,$from,$to,$CURSOR,$made);
    }

    multi method WHICH (Match:D:) {
        self.Mu::WHICH # skip Capture's as Match is not a value type
    }

    method ast(Match:D:) { $!made }

    multi method Str(Match:D:) {
        nqp::if(
          nqp::isgt_i($!to,$!from),
          nqp::substr($!CURSOR.target,$!from,nqp::sub_i($!to,$!from)),
          ''
        )
    }

    multi method Numeric(Match:D:) {
        self.Str.Numeric
    }
    multi method Bool(Match:D:) {
        nqp::p6bool(nqp::isge_i($!to,$!from))
    }
    multi method ACCEPTS(Match:D: Any $) { self }

    method prematch(Match:D:) {
        nqp::substr($!CURSOR.target,0,$!from)
    }
    method postmatch(Match:D:) {
        nqp::substr($!CURSOR.target,$!to)
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
        %attrs.ASSIGN-KEY("orig", self.orig.perl);
        %attrs.ASSIGN-KEY("from", self.from.perl);
        %attrs.ASSIGN-KEY("to",   self.to.perl  );
        %attrs.ASSIGN-KEY("ast",  self.ast.perl );
        %attrs.ASSIGN-KEY("list", self.list.perl);
        %attrs.ASSIGN-KEY("hash", self.hash.perl);

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

multi sub infix:<eqv>(Match:D $a, Match:D $b) {
    $a =:= $b
    ||
    [&&] (
        $a.to   eqv $b.to,
        $a.from eqv $b.from,
        $a.orig eqv $b.orig,
        $a.made eqv $b.made,
        $a.list eqv $b.list,
        $a.hash eqv $b.hash
    );
}


sub make(Mu \made) {
    my $slash := nqp::getlexcaller('$/');
    nqp::bindattr( nqp::decont($slash),        Match,  '$!made', made );
    nqp::bindattr( nqp::decont($slash.CURSOR), Cursor, '$!made', made );
}


# vim: ft=perl6 expandtab sw=4
