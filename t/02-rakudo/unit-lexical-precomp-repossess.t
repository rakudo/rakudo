use lib <t/02-rakudo/test-packages>;
use Test;

plan 2;

# A unit lexical container is serialized with its module, and a module
# that uses it can change it while precompiling. Loading such a module
# must not bring back the state its precompilation saw, so the registry
# keeps what every loaded module registered.
#
# RegistryUnitBoth precompiles with both registrations made, and
# RegistryUnitLater with only the second one made.

use RegistryUnitBoth;
use RegistryUnitLater;
use RegistryUnit;
use RegistryFile;

is-deeply registered(), ['first', 'second'],
  'a unit module array keeps every registration after a later module loads';
is-deeply registered-in-file(), ['first', 'second'],
  'a mainline array keeps every registration after a later module loads';

# vim: expandtab shiftwidth=4
