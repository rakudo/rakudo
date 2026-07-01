use Test;

plan 5;

# A `$var::` designates the package that is the variable's value. The trailing
# `::` is an empty final name part; it adds no lookup, so `$p::` is the package
# and `$p::.WHO` is its stash.

my $p = Int;

is $p::.WHO.^name, 'Stash',
    'a trailing :: on a variable gives the package stash';
ok $p:: === Int,
    'the package designated by $p:: is the value of $p';
is $p::<Nonesuch>:exists, False,
    'subscripting the package designated by $p:: works';

# The other indirect-lookup forms still work.
my $n = 'Int';
is ::($n).^name, 'Int',
    'an indirect name lookup ::($n) still resolves';

module PkgTrailingEmpty { our $sym = 42 }
is ::PkgTrailingEmpty::<$sym>, 42,
    'a leading :: qualified lookup still resolves';

# vim: expandtab shiftwidth=4
