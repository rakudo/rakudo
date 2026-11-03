use v6.c;
use Test;

plan 4;

# In 6.c an empty contextualizer operates on $/.

# `$()` yields $/.Str when the match made no value.
"abc" ~~ /b/;
is $(), 'b', '$() yields the matched string when $/ has no made value';

# `$()` yields the made value when the match made one.
"abc" ~~ / b { make 99 } /;
is $(), 99, '$() yields the made value of $/';

# `@()` coerces $/ to its positional captures.
"abc" ~~ /(b)(c)/;
is @().elems, 2, '@() yields the positional captures of $/';

# `%()` coerces $/ to its named captures.
"x4" ~~ /$<k>=(\w) $<v>=(\d)/;
is %()<k>.Str, 'x', '%() yields the named captures of $/';

# vim: expandtab shiftwidth=4
