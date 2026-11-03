use Test;

plan 8;

# `now` and `time` are built-in listop terms, but a lexical of the same name,
# such as the sigilless `my \time`, must shadow the built-in. RakuAST parsed
# the bareword as the built-in term regardless of the lexical.

{
    my \time = 42;
    is time, 42, 'a sigilless \time shadows the built-in time term';
}

{
    my \now = 99;
    is now, 99, 'a sigilless \now shadows the built-in now term';
}

# A same-named routine shadows the term too.
{
    sub time { 123 }
    is time, 123, 'a sub named time shadows the built-in time term';
}

{
    my &now = { 7 }
    is now, 7, 'a &now routine variable shadows the built-in now term';
}

{
    my \time = DateTime.new(:2020year, :1month, :1day);
    is time.year, 2020, 'a method call resolves against the lexical, not the built-in';
}

# Without a lexical in scope the built-ins still resolve.
is (time ~~ Int),   True, 'unshadowed time still returns the built-in';
is (now  ~~ Instant), True, 'unshadowed now still returns the built-in';

# The shadow is scoped: the built-in is back once the lexical is out of scope.
{
    my \time = 5;
}
is (time ~~ Int), True, 'the built-in returns once the lexical is out of scope';

# vim: expandtab shiftwidth=4
