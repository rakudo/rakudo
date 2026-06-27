use Test;

plan 4;

# A `{ }` whose only entry is a pair is a hash, not a block, even when the
# pair's value is an `anon` declaration that installs no symbol, such as an
# anon subset. RakuAST previously mis-detected this as a block, then failed to
# bind it to a hash.
my $subset-hash = { day => (anon subset NumericDay of Int where * <= 31) };
isa-ok $subset-hash, Hash,
    'a pair whose value is an anon subset makes a hash';
ok (5 ~~ $subset-hash<day>) && !(99 ~~ $subset-hash<day>),
    'the anon-subset value is stored in the hash and still type-checks';

# An anon enum still installs its enumerated values into the enclosing scope,
# so a `{ }` with such a value stays a block, the same as the legacy frontend.
my $enum-block = { k => (anon enum AnonRGB <ARED AGRN ABLU>) };
isa-ok $enum-block, Callable,
    'a pair whose value is an anon enum stays a block';

# A plain pair is a hash.
my $plain = { a => 1 };
isa-ok $plain, Hash, 'a plain pair makes a hash';

# vim: expandtab shiftwidth=4
