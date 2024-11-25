use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 3;

my $to-eval = q:to/END/;

proto sub gated(|) is revision-gated("6.c") {*}
multi sub gated(Int $x) is revision-gated("6.c") { print "6.c ($x)" }
multi sub gated(Int $x) is revision-gated("6.d") { print "6.d ({$x+1})" }
multi sub gated(Int $x) is revision-gated("6.e") { print "6.e ({$x+2})" }

gated(6);

END

is-run 'use v6.c;' ~ $to-eval,
        :out("6.c (6)"), q|is revision-gated("6.c") candidate called for 'use v6.c;'|;
is-run 'use v6.d;' ~ $to-eval,
        :out("6.d (7)"), q|is revision-gated("6.d") candidate called for 'use v6.d;'|;
is-run 'use v6.e.PREVIEW;' ~ $to-eval,
        :out("6.e (8)"), q|is revision-gated("6.e") candidate called for 'use v6.e;'|;
