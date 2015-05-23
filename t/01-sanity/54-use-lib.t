use v6;

use Test;

plan 17;

BEGIN my $start = +@?INC;

use cur <foo>;
is (BEGIN @?INC)[0], 'file#foo', 'can we force a (default) CURLF';
is (BEGIN +@?INC), $start + 1, 'did we add it to @?INC? (1)';

use cur <file#bar>;
is (BEGIN @?INC)[0], 'file#bar', 'can we force a specific CURLF';
is (BEGIN @?INC)[1], 'file#foo', 'do we have the previous entry moved up (1)';
is (BEGIN +@?INC),   $start + 2, 'did we add it to @?INC? (2)';

{
    use cur <inst#baz>;
    is (BEGIN @?INC[0]), 'inst#baz', 'can we add in a scope';
    is (BEGIN @?INC[1]), 'file#bar', 'do we have previous entry moved up (2)';
    is (BEGIN @?INC[2]), 'file#foo', 'do we have previous entry moved up (3)';
    is (BEGIN +@?INC),   $start + 3, 'did we add it to @?INC? (3)';
}

is (BEGIN @?INC[0]), 'file#bar', 'did we revert to previous setting';
is (BEGIN @?INC[1]), 'file#foo', 'do we have previous entry moved down again';
is (BEGIN +@?INC),   $start + 2, 'did we revert number of elements';

throws-like( { @?INC[0] = "boom" },
  X::Assignment::RO,
  typename => $*VM.name eq 'jvm' ?? 'value' !! 'Str',
);
for <unshift("boom") push("boom")> -> $method {
    throws-like( "@?INC.$method", X::Multi::NoMatch );
}
for <shift pop> -> $method {
    throws-like( "@?INC.$method", X::Method::NotFound );
}
