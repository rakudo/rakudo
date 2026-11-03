use Test;

plan 16;

# https://github.com/rakudo/rakudo/issues/5761
# The separator that .split and .trans act on is the marked part of each
# match: text outside the capture markers stays in the surrounding parts.

is-deeply "foobar".split(/ o <( o )> b /).List, ("fo", "bar"),
  '.split honors both capture markers';

is-deeply "foobar".split(/ o <( ob /).List, ("fo", "ar"),
  '.split honors a <( marker without )>';

is-deeply "foobar".split(/ oo )> b /).List, ("f", "bar"),
  '.split honors a )> marker without <(';

is-deeply "foobar".split(/ oob /).List, ("f", "ar"),
  '.split without capture markers is unaffected';

is-deeply "foobarfoobar".split(/ o <( o )> b /).List, ("fo", "barfo", "bar"),
  '.split honors capture markers on repeated matches';

is-deeply "foobaroob".split(/ o <( o )> b /, 2).List, ("fo", "baroob"),
  '.split with a limit honors capture markers';

is-deeply "oobfoob".split(/ o <( o )> b /, :skip-empty).List, ("o", "bfo", "b"),
  '.split with :skip-empty honors capture markers';

is-deeply "foobar".split([/ o <( o )> b /]).List, ("fo", "bar"),
  '.split with a needle list honors capture markers';

my $v = "foobar".split(/ o <( o )> b /, :v).List;
is-deeply ($v[0], $v[2]), ("fo", "bar"),
  '.split with :v produces parts according to capture markers';
is $v[1].Str, "o",
  '.split with :v produces the marked extent as the separator Match';
is-deeply ($v[1].from, $v[1].to), (2, 3),
  '.split with :v produces a Match with the marked extent';

is "foobar".trans(/ o <( o )> b / => "z"), "fozbar",
  '.trans with a string replacement honors capture markers';

is "foobar".trans(/ o <( o )> b / => { "[$_]" }), "fo[o]bar",
  '.trans with a callable replacement honors capture markers';

is "foobarfoobar".trans([/ o <( o )> b /] => ["Z"]), "foZbarfoZbar",
  '.trans with needle and replacement lists honors capture markers';

# The extent a marker regex splits on is the same extent that .subst
# replaces.
my $string = "xxabcxxabc";
my $rx     = / a <( b )> c /;
is $string.split($rx).join("|"), $string.subst($rx, "|", :g),
  '.split and .subst agree on the extent of a marker regex';

# Markers inside an interpolated regex apply to the inner match only.
my $inner = / o <( o )> b /;
is-deeply "foobar".split(/ $inner /).List, ("f", "ar"),
  '.split does not apply capture markers of an interpolated regex';

# vim: expandtab shiftwidth=4
