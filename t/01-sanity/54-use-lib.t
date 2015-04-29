use v6;

use Test;

plan 17;

my $start = +@?INC;

use cur <file:foo>;
is @?INC[0], 'file:foo', 'can we force a specific CURLF';
is +@?INC,   $start + 1, 'did we add it to @?INC? (1)';
use cur <bar>;
is @?INC[0], 'file:bar', 'does the CURLF id get prefixed if none given';
is @?INC[1], 'file:foo', 'do we have the previous entry moved up (1)';
is +@?INC,   $start + 2, 'did we add it to @?INC? (2)';

{
    use cur <inst:baz>;
    is @?INC[0], 'inst:baz', 'can we add in a scope with another CURLF id';
    is @?INC[1], 'file:bar', 'do we have the previous entry moved up (2)';
    is @?INC[2], 'file:foo', 'do we have the previous entry moved up (3)';
    is +@?INC,   $start + 3, 'did we add it to @?INC? (3)';
}

is @?INC[0], 'file:bar', 'did we revert to previous setting';
is @?INC[1], 'file:foo', 'do we have the previous entry moved down again';
is +@?INC,   $start + 2, 'did we revert number of elements';

throws_like( { @?INC[0] = "boom" },
  X::Assignment::RO,
  typename => 'Str',
);
for <unshift("boom") push("boom")> -> $method {
    throws_like( "@?INC.$method", X::Multi::NoMatch );
}
for <shift pop> -> $method {
    throws_like( "@?INC.$method", X::Method::NotFound );
}
