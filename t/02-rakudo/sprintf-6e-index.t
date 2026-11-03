use v6.e.PREVIEW;
use Test;

# Explicit parameter indices in the 6.e sprintf

plan 11;

is sprintf(Q[%2$d %d %d], 1, 2, 3), '2 1 2',
    'explicit index does not advance the implicit sequence';
is sprintf(Q[%2$d %d %d], 1, 2), '2 1 2',
    'a format with an explicit index needs only the arguments it uses';
is sprintf(Q[%3$s %1$s %2$s], 'a', 'b', 'c'), 'c a b',
    'all-explicit indices reorder the arguments';
is sprintf(Q[%2$d], 1, 2), '2',
    'an explicit index allows unconsumed arguments';
is sprintf(Q[%2$s-%2$s %1$s], 'b', 'a'), 'a-a b',
    'an explicit index can reuse an argument';
is sprintf(Q[%2$*1$d], 6, 42), '    42',
    'a star width can carry its own explicit index';
throws-like { sprintf('%d', 1, 2) }, X::Str::Sprintf::Directives::Count,
    'a format without explicit indices still requires an exact count';
throws-like { sprintf(Q[%2$d %d %d], 1) }, X::Str::Sprintf::Directives::Count,
    :args-have(1), :args-used(2),
    'missing arguments for an explicit index format report the needed count';

# The same behavior through a Format object
is Format.new(Q[%2$s %1$s])('a', 'b'), 'b a',
    'a Format object with explicit indices reorders the arguments';
is Format.new(Q[%2$s %1$s]).arity, 2,
    'the arity of an explicit index format is the highest index needed';
is Format.new(Q[%2$d %d %d]).handle-iterator((1..6).iterator, ' | '),
    '2 1 2 | 4 3 4 | 6 5 6',
    'iterating an explicit index format batches by its arity';

# vim: expandtab shiftwidth=4
