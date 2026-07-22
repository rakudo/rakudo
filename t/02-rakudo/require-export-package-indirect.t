use lib <t/02-rakudo/test-packages>;
use Test;

plan 3;

# `require` loads a module and merges its EXPORT package into the caller's
# %?REQUIRE-SYMBOLS. Reading it back through an indirect lookup `::("EXPORT")`
# must run at run time so it sees those merged symbols; folding the constant
# string to this compilation unit's own (empty) export-package would report no
# exports. A direct `EXPORT::<...>` reference still resolves at compile time.

require ::("BundleExport");

ok ::("EXPORT").WHO<DEFAULT>:exists,
    'the required module EXPORT::DEFAULT is visible through ::("EXPORT")';

my @pairs = ::("EXPORT").WHO<DEFAULT>.WHO.pairs;
is @pairs.map(*.key).sort, ('&bundle-export',),
    'the required module exports are read from ::("EXPORT")';

is ::("EXPORT").WHO<DEFAULT>.WHO<&bundle-export>('x'), 'bundled x',
    'the re-read exported sub is callable';

# vim: expandtab shiftwidth=4
