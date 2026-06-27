use Test;

plan 12;

# An adverb on a match-capture shorthand (`$0:exists`, `$<name>:exists`) applies
# to the underlying `$/[...]` / `$/<...>` access, the same as on any subscript.

ok "ab" ~~ /(.)(.)/, 'sanity match with positional captures';

is ($0:exists), True,  '$0:exists is True for a matched positional capture';
is ($1:exists), True,  '$1:exists is True for a matched positional capture';
is ($2:exists), False, '$2:exists is False for an unmatched positional capture';
is ($0:k), 0, '$0:k yields the key';

ok "xy" ~~ /$<a>=(.) $<b>=(.)/, 'sanity match with named captures';

is ($<a>:exists), True,  '$<a>:exists is True for a matched named capture';
is ($<missing>:exists), False, '$<missing>:exists is False for an absent named capture';

# A capture adverb matches the explicit `$/<...>` / `$/[...]` form it is sugar for.
is ($<a>:exists), ($/<a>:exists), '$<a>:exists matches $/<a>:exists';
is ($0:exists), ($/[0]:exists), '$0:exists matches $/[0]:exists';
is-deeply ($<a>:kv), ($/<a>:kv), '$<a>:kv matches $/<a>:kv';
is ($<missing>:!exists), ($/<missing>:!exists), '$<missing>:!exists matches the explicit form';

# vim: expandtab shiftwidth=4
