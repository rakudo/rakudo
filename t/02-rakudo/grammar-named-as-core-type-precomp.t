use lib <t/02-rakudo/test-packages>;
use Test;

plan 3;

# Declaring a package whose short name matches a setting builtin (here a
# grammar named Grammar, as YAMLish does) inside an outer package must
# create a fresh, distinct package that shadows the builtin, not adopt
# the builtin's WHO. Adopting it put any nested `our` package into the
# already-serialized setting SC, which broke precompilation with
# "Object does not exist in serialization context". This test imports a
# precompiled module of that exact shape; loading it forces the
# deserialization that used to fail.

use GrammarNamedLikeCore;

ok GrammarNamedLikeCore::Grammar.parse("abc"),
    'a precompiled grammar named like a setting builtin loads and parses';
nok GrammarNamedLikeCore::Grammar =:= CORE::Grammar,
    'and it is a distinct type shadowing the builtin, not the builtin itself';
is GrammarNamedLikeCore::Grammar::Actions.greet, 'hi',
    'its nested our-scoped package precompiled and is reachable';
