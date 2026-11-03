use lib <t/02-rakudo/test-packages>;
use Test;

plan 4;

# A single-part package-ish declaration whose name is already lexically
# visible upgrades that pre-existing symbol to a top level entry in the
# compunit's GLOBALish. A `use` merges the module's GLOBALish into the
# consumer lexically, so the upgrade is what lets a consumer reach
# Gender::Gender when the module declares `enum Gender` inside
# `package Gender` under `unit module`, as Intl::CLDR does. Without it
# the consumer gets "Type 'Gender::Gender' is not declared".

use EnumNestedInSameNamePackage;

my Gender::Gender $g;
ok $g === Gender::Gender,
    'the nested enum type is reachable as Gender::Gender in the consumer';
is Gender::Gender::neuter.Int, 0,
    'an enum value is reachable through the fully qualified name';
ok Gender::Gender =:= EnumNestedInSameNamePackage::Gender::Gender,
    'the short name resolves to the same type as the fully qualified name';
ok Gender =:= EnumNestedInSameNamePackage::Gender,
    'the upgraded top level name is the package nested under the module';
