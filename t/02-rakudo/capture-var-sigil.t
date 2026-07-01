use Test;

plan 10;

# `@<foo>` and `%<foo>` put a named capture into list / hash context (its
# positional / named subcaptures), the same as `@($/<foo>)` / `%($/<foo>)`.
# `$<foo>` stays the raw Match. In particular `@<foo>` of a capture with no
# positional subcaptures is empty, not a one-element list holding the Match.

ok "y" ~~ / $<a>=<.alpha> /, 'sanity: named capture with no subcaptures';
isa-ok $<a>, Match, '$<a> is the raw Match';
is @<a>.elems, 0, '@<a> of a capture with no positional subcaptures is empty';
is-deeply @<a>.List, @($/<a>).List, '@<a> matches @($/<a>)';

ok "ab" ~~ / $<p>=( (.) (.) ) /, 'sanity: named capture with positional subcaptures';
is @<p>.elems, 2, '@<p> lists the positional subcaptures';

ok "ab" ~~ / $<h>=( $<x>=(.) $<y>=(.) ) /, 'sanity: named capture with named subcaptures';
is %<h>.elems, 2, '%<h> hashes the named subcaptures';

# In list / hash context an adverb is dropped, the same as legacy, rather than
# applying to the subscript (which made `%<h>:exists` call `.hash` on a Bool).
is-deeply (@<p>:exists).List, @($/<p>).List, '@<p>:exists drops the adverb and stays the list';
is-deeply (%<h>:exists).Hash, %($/<h>).Hash, '%<h>:exists drops the adverb and stays the hash';

# vim: expandtab shiftwidth=4
