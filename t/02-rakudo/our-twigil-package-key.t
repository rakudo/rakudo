use lib <t/02-rakudo/test-packages>;

# An `our @.foo` / `our %.foo` in a class keys its package slot by the full name
# including the twigil, as the meta-object installation does. Looking it up
# without the twigil vivified an orphan package symbol, so pulling the class in
# through two paths (directly and via a module that also uses it) died "Merging
# GLOBAL symbols failed: duplicate definition of symbol @operations". Reaching
# this file's own compilation means that diamond merged cleanly.
use OurTwigilLeaf;
use OurTwigilMid;

use Test;

plan 2;

is OurTwigilLeaf.operations.join(','), 'plus,minus',
    'the `our @.operations` accessor returns its bound value';

is OurTwigilLeaf.table<a>, 1,
    'the `our %.table` accessor returns its bound value';
