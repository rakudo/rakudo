use lib <t/packages/Test-Helpers>;
use nqp;
use Test;
use Test::Helpers;

plan 8;

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

# Superseding a candidate must not require copying its parameter names:
# the filter compares signatures structurally, not by their rendering.
my $renamed-to-eval = q:to/END/;

proto sub gated(|) is revision-gated("6.c") {*}
multi sub gated(Int $x) is revision-gated("6.c") { print "6.c ($x)" }
multi sub gated(Int $y) is revision-gated("6.e") { print "6.e ({$y+2})" }

gated(6);

END

is-run 'use v6.c;' ~ $renamed-to-eval,
        :out("6.c (6)"), 'candidates differing in parameter name only supersede (6.c caller)';
is-run 'use v6.e.PREVIEW;' ~ $renamed-to-eval,
        :out("6.e (8)"), 'candidates differing in parameter name only supersede (6.e caller)';

# Two candidates gated at the same revision cannot both be dispatched to;
# one of them wins rather than every call dying as an ambiguous dispatch.
my $clashing-to-eval = q:to/END/;

proto sub gated(|) is revision-gated("6.c") {*}
multi sub gated(Int $x) is revision-gated("6.c") { print "first ($x)" }
multi sub gated(Int $y) is revision-gated("6.c") { print "second ($y)" }

gated(6);

END

is-run 'use v6.c;' ~ $clashing-to-eval, :err(*),
        :out("first (6)"), 'same-revision equivalent candidates dispatch to one candidate';

# The compile time redeclaration worry is a RakuAST frontend check.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is-run 'use v6.c;' ~ $clashing-to-eval,
            :err(/'equivalent signatures'/), :out(*),
            'same-revision equivalent candidates get the redeclaration worry';
    is-run 'use v6.c;' ~ $renamed-to-eval,
            :err(''), :out(*),
            'candidates gated at different revisions do not get the redeclaration worry';
}
else {
    skip 'the redeclaration worry needs the RakuAST frontend', 2;
}
