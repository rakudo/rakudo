use Test;

plan 5;

# An adverb is tighter than most infixes, so in `EXPR OP term:adverb` it can bind
# to OP rather than to the term. A short-circuit, thunky, or chaining operator
# does not compile to a call and cannot take the adverb as a named argument, so
# it is rejected instead of silently dropped.

# https://github.com/rakudo/rakudo/issues/5685
throws-like 'my %h; True && %h<a>:exists', X::Syntax::Adverb,
    "can't adverb a short-circuit infix";
throws-like 'my %h; 1 == %h<a>:exists', X::Syntax::Adverb,
    "can't adverb a chaining infix";
throws-like 'my %h; True ^^ %h<a>:exists', X::Syntax::Adverb,
    "can't adverb a thunky list infix";

# The adverb still reaches the subscript when it binds there.
my %h = a => 1;
is (%h<a>:exists), True, 'an adverb on a subscript still works';
is (True and %h<a>:exists), True, 'a loose infix leaves the adverb on the subscript';

# vim: expandtab shiftwidth=4
