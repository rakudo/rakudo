use lib <t/02-rakudo/test-packages>;
use Test;
use PackageDeclarations;

plan 11;

is PackageDeclarations::in-variable-trait(), 3,
    'a class declared in a trait argument of a variable in a precompiled module has its methods';
is PackageDeclarations::in-type-argument(), 4,
    'a class declared in a parameterized type in a precompiled module has its methods';
is PackageDeclarations::in-enum-value(), '55',
    'a class declared in the value of an enum in a precompiled module has its methods';
is-deeply PackageDeclarations::role-attribute-default(), [1, 2],
    'a class with an attribute default declared in a role body of a precompiled module has its default';
is PackageDeclarations::header-sub(), 6,
    'a sub declared in a trait argument of a class in a precompiled module is seen by its methods';
is PackageDeclarations::header-variable(), 7,
    'a variable declared in a trait argument of a class in a precompiled module is seen by its methods';
is PackageDeclarations::header-role(), 8,
    'a constant declared in a trait argument of a role in a precompiled module is seen by its methods';
is PackageDeclarations::header-in-role-method(), 9,
    'a variable declared in a trait argument of a class in a role method of a precompiled module is seen by its methods';
is PackageDeclarations::begin-closure-through-header(), '35',
    'a sub made at BEGIN time in a class with a trait argument declaring a variable in a precompiled module sees its lexicals';
is PackageDeclarations::constant-closure-through-header(), '45',
    'a sub made for a constant in a class with a trait argument declaring a variable in a precompiled module sees its lexicals';
is PackageDeclarations::class-of-role-in-header(), 'PackageDeclarations::ClassOfRole::C',
    'a class whose trait argument reads the class of the role around it precompiles with the role';
