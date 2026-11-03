use lib <t/02-rakudo/test-packages>;
use Test;
use UsesComposeAddsMethod;

plan 2;

# A custom metaclass that creates a closure during compose and adds it
# as a method, as OO::Monitors does, produces a class whose method table
# holds a closure over the compose invocation frame. Precompiling a
# module that declares such a class must serialize that frame chain;
# the compiler services passed to compose reach the setting context and
# must be released once composition is done, or serialization dies with
# "Missing serialize REPR function for REPR MVMContext".

is UsesComposeAddsMethod.new.m, 42,
    'a precompiled class from a compose-extending metaclass works';
is UsesComposeAddsMethod.new.compose-tag, 'composed',
    'the method added during compose is callable and sees its closure';
