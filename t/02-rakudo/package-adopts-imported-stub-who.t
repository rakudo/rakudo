use lib <t/packages/02-rakudo/lib>;
use Test;

plan 2;

# Importing StealWhoLeaf brings in a stub StealWho package carrying Leaf.
use StealWhoLeaf;
my constant $stub-who = StealWho.WHO;

class StealWho {
}

ok StealWho.WHO<Leaf>:exists,
    'a symbol from the imported stub package is visible via the class';
ok StealWho.WHO =:= $stub-who,
    'the class adopts the imported stub package WHO rather than copying its symbols';

# vim: expandtab shiftwidth=4
