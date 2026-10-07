use lib <t/02-rakudo/test-packages>;
use Test;
use PackageDeclarations;

plan 4;

is PackageDeclarations::in-variable-trait(), 3,
    'a class declared in a trait argument of a variable in a precompiled module has its methods';
is PackageDeclarations::in-type-argument(), 4,
    'a class declared in a parameterized type in a precompiled module has its methods';
is PackageDeclarations::in-enum-value(), '55',
    'a class declared in the value of an enum in a precompiled module has its methods';
is-deeply PackageDeclarations::role-attribute-default(), [1, 2],
    'a class with an attribute default declared in a role body of a precompiled module has its default';
