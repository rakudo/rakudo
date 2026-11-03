use Test;

plan 6;

# An `anon subset` compiles and the resulting subset still type-checks.
my $subset = (anon subset NumericDay of Int where * <= 31);
is $subset.^name, 'NumericDay', 'an anon subset carries its declared name';
ok 5 ~~ $subset, 'an anon subset accepts a matching value';
nok 99 ~~ $subset, 'an anon subset rejects a non-matching value';

# `anon` means the name is not installed in the enclosing scope.
throws-like 'anon subset Foo of Int where * < 5; Foo', X::Undeclared::Symbols,
    'an anon subset does not install its name';

# The same holds for an anon enum: both reach the shared package installer.
throws-like 'anon enum Bar <a b c>; Bar', X::Undeclared::Symbols,
    'an anon enum does not install its name';

# A named subset still installs its name and type-checks.
{
    my subset Small of Int where * < 5;
    ok 3 ~~ Small, 'a my-scoped subset still installs and type-checks';
}

# vim: expandtab shiftwidth=4
