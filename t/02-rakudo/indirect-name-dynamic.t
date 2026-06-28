use Test;

plan 6;

# An indirect name `::(expr)` whose expression is not compile-time known, e.g.
# one mentioning a dynamic variable, resolves at runtime rather than dying.
my $*FOO = "Int";
is ::($*FOO).^name, 'Int', 'indirect lookup through a dynamic variable';

is ::($*UNSET || "Str").^name, 'Str',
    'indirect lookup through a dynamic-variable expression';

my $*BAR = "Rat";
is ::($*BAR).^name, 'Rat', 'indirect lookup of a bare dynamic variable';

is ::("Num").^name, 'Num', 'indirect lookup through a literal string';

# An empty indirect name is the empty symbol, which does not exist.
throws-like { ::("") }, X::NoSuchSymbol, 'an empty indirect name has no symbol';

# require uses the same indirect-name resolution path.
lives-ok {
    my $*LIB;
    try require ::($*LIB || "Test");
}, 'require of an indirect name with a dynamic-variable expression';

# vim: expandtab shiftwidth=4
