unit module PackageDeclarations;

my $default is default(my class InDefault { method x { 3 } });
our sub in-variable-trait() { InDefault.x }

my Positional[my class InTypeArg { method x { 4 } }] $typed;
our sub in-type-argument() { InTypeArg.x }

my enum Enum (a => (my class InEnum { method x { 5 } }).x);
our sub in-enum-value() { a.value ~ InEnum.x }

my role Box[$init] { my class Item { has @.parts = 1, 2 }; method items { Item.new.parts } }
my class UsesBox does Box[1] { }
our sub role-attribute-default() { UsesBox.items }
