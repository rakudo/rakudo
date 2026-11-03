use Test;

plan 13;

# A parenthesized WhateverCode in a `where` is the same closure as the
# unparenthesized form. `where (* > 0)` must not be mistaken for an explicit
# block `where { * > 0 }`, which is a genuine double closure. This holds for
# parameter, subset, and variable constraints alike.

sub simple($x where (* > 0)) { $x }
is simple(2), 2, 'where (* > 0) accepts a value that satisfies it';
throws-like { simple(-1) }, X::TypeCheck::Binding,
    'where (* > 0) still rejects a value that fails it';

sub chained($x where (1 <= * <= 4)) { $x }
is chained(3), 3, 'where (1 <= * <= 4) accepts an in-range value';
throws-like { chained(9) }, X::TypeCheck::Binding,
    'where (1 <= * <= 4) rejects an out-of-range value';

# Two parameters each with a parenthesized whatever constraint, as in
# Game::Concentration::Role.show.
sub two($row where (1 <= * <= 4), $col where (1 <= * <= 13)) { "$row;$col" }
is two(2, 7), '2;7', 'multiple parenthesized whatever constraints compile and bind';

# A non-whatever parenthesized constraint keeps working.
sub plain($x where (5)) { $x }
is plain(5), 5, 'where (5) still works';

# The same in a subset.
subset Positive of Int where (* > 0);
lives-ok { my Positive $s = 5 }, 'subset where (* > 0) accepts a valid value';
dies-ok  { my Positive $s = -1 }, 'subset where (* > 0) rejects an invalid value';

# And in a variable constraint, which routes through the subset machinery.
lives-ok { my $v where (1 <= * <= 4) = 3 }, 'variable where (1 <= * <= 4) accepts';
dies-ok  { my $v where (1 <= * <= 4) = 9 }, 'variable where (1 <= * <= 4) rejects';

# A bare `where *` is a Whatever constraint that accepts anything (as in
# IUP's `Bool :$copy where *`). Its synthetic ACCEPTS wrapper must not be
# mistaken for a user-written double closure.
sub bare($x where *) { $x }
is bare(-99), -99, 'where * accepts any value';
sub named(Bool :$flag where *) { $flag }
is named(:flag), True, 'a named parameter where * accepts any value';

# An explicit block wrapping a whatever is still a double closure error.
throws-like 'sub f($x where { * > 0 }) { }', X::Syntax::Malformed,
    'where { * > 0 } is still rejected as a double closure';

# vim: expandtab shiftwidth=4
