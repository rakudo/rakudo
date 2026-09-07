use Test;

plan 9;

# A hash interpolated into a regex is reserved. The plain `$h` form and
# every assertion form throw X::Syntax::Reserved, a hash variable at
# compile time and a hash reached through a scalar or an array at match
# time, whatever the hash holds and wherever it sits among the elements.

my $h = { a => 1 };
my $e = {};

throws-like { 'a' ~~ / <$h> / }, X::Syntax::Reserved,
    'a hash in a scalar assertion is reserved';

throws-like { 'a' ~~ / <$e> / }, X::Syntax::Reserved,
    'an empty hash in a scalar assertion is reserved';

throws-like 'my %h = a => 1; "a" ~~ / <%h> /', X::Syntax::Reserved,
    message => /'hash as a regex assertion'/,
    'a hash variable assertion is reserved';

throws-like 'my %e; "a" ~~ / <%e> /', X::Syntax::Reserved,
    message => /'hash as a regex assertion'/,
    'an empty hash variable assertion is reserved';

throws-like 'my %h; "a" ~~ / [ ||<%h> ] /', X::Syntax::Reserved,
    message => /'hash as a regex assertion'/,
    'a sequential hash variable assertion is reserved';

my @a = 'ab', $h, 'a';
throws-like { 'ab' ~~ / <@a> / }, X::Syntax::Reserved,
    'a hash element of an array assertion is reserved';

my @b = 'a', $h;
throws-like { 'a' ~~ / [ ||<@b> ] / }, X::Syntax::Reserved,
    'a hash element after a matching element of a sequential array assertion is reserved';

my @c = 'x', $h, 'a';
throws-like { 'a' ~~ / [ ||<@c> ] / }, X::Syntax::Reserved,
    'a hash element after a failing element of a sequential array assertion is reserved';

throws-like { 'a' ~~ / $h / }, X::Syntax::Reserved,
    'a hash interpolated outside an assertion is reserved';

# vim: expandtab shiftwidth=4
