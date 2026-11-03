use lib <t/packages/02-rakudo/lib>;
use Test;

plan 1;

# Precompiling a unit whose name is nested (here `QualPkgSC::Config`) and which
# refers at BEGIN time to a qualified symbol led by its own parent package
# (`@QualPkgSC::Raw::ClassMap`) used to die with "Object of type QualPkgSC in
# QAST::WVal, but not in SC": the vivified parent package never reached the
# serialization context. Loading the module exercises that precompilation.
use QualPkgSC::Config;

is QualPkgSC::Config.names, ('Int', 'Str'),
    'a constant naming a symbol through the unit own parent package compiles';

# vim: expandtab shiftwidth=4
