use lib <t/packages/Test-Helpers t/packages/02-rakudo/lib>;
use Test;
use Test::Helpers;

plan 1;

# Referencing a CORE:: symbol at compile time inside a module (here in a role
# method body) must not pull the setting's context into the serialization
# context. If it does, precompiling the module dies with
# "Missing serialize REPR function for REPR MVMContext (BOOTContext)".
is-run 'use CoreLookupHelper; print "ok"',
    'a compile-time CORE:: reference in a precompiled module loads',
    :out("ok"),
    :compiler-args[<-I t/packages/02-rakudo/lib>];

# vim: expandtab shiftwidth=4
