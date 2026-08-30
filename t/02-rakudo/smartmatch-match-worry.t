use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 10;

# From 6.e on a concrete Match matcher compares by identity, so a matcher
# expression that statically produces a Match, like `$x ~~ /regex/` in
# matcher position, gets a compile-time worry. The worry fires only when
# compiling 6.e code: the same shape matches fine before 6.e.

my $worry = rx/'compares by identity'/;

is-run 'use v6.e.PREVIEW; my $x = "foo"; given $x { when $x ~~ /o+/ { } }',
    :err($worry),
    'a when statement matching against a smartmatch worries under 6.e';

is-run 'use v6.e.PREVIEW; my $x = "foo"; $_ = $x; Nil when $x ~~ /o+/;',
    :err($worry),
    'a when statement modifier matching against a smartmatch worries under 6.e';

is-run 'use v6.e.PREVIEW; my $x = "foo"; my $r = $_ ~~ ($x ~~ /o+/);',
    :err($worry),
    'an infix smartmatch against a parenthesized smartmatch worries under 6.e';

is-run 'use v6.e.PREVIEW; my $x = "foo"; sub f($a where $x ~~ /o+/) {}',
    :err($worry),
    'a signature where clause that is a smartmatch worries under 6.e';

is-run 'use v6.e.PREVIEW; my $x = "foo"; subset S where $x ~~ /o+/;',
    :err($worry),
    'a subset where clause that is a smartmatch worries under 6.e';

is-run 'use v6.e.PREVIEW; my $x = "foo"; given $x { when $x ~~ s/o/0/ { } }',
    :err($worry),
    'a when statement matching against a substitution worries under 6.e';

is-run 'my $x = "foo"; given $x { when $x ~~ /o+/ { print "hit" } }',
    :out('hit'), :err(''),
    'the same when statement stays silent and fires before 6.e';

is-run 'use v6.e.PREVIEW; my $x = "foo"; given $x { when /o+/ { print "hit" } }',
    :out('hit'), :err(''),
    'a when statement matching the topic against a regex directly does not worry';

is-run 'use v6.e.PREVIEW; my $x = "foo"; $_ = $x; Nil when $x !~~ /zz/;',
    :err(''),
    'a negated inner smartmatch produces a Bool and does not worry';

is-run 'use v6.e.PREVIEW; my $x = "foo"; my $y = "o"; my $r = $_ ~~ ($x ~~ $y);',
    :err(''),
    'a smartmatch matcher without a regex on its right does not worry';

# vim: expandtab shiftwidth=4
