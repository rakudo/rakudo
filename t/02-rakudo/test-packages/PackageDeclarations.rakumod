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

my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }

my class HeaderSub is tagged(my sub s { 6 }) { method m { s() } }
our sub header-sub() { HeaderSub.m }

my class HeaderVariable is tagged(my $v = 7) { method m { $v } }
our sub header-variable() { HeaderVariable.m }

my role HeaderRole is tagged(my constant K = 8) { method m { K } }
my class DoesHeaderRole does HeaderRole { }
our sub header-role() { DoesHeaderRole.m }

my role HeaderInMethod { method r { my class C is tagged(my $v = 9) { method m { $v } }; C.m } }
my class DoesHeaderInMethod does HeaderInMethod { }
our sub header-in-role-method() { DoesHeaderInMethod.r }

my $begin-closure = BEGIN { my $w = 3; my class BeginHeader is tagged(my $v = 5) { our sub s { $w ~ $v } }; &BeginHeader::s };
our sub begin-closure-through-header() { $begin-closure() }

sub make-header-closure($n) { my class MadeHeader is tagged(my $v = 5) { our sub s { $n ~ $v } }; &MadeHeader::s }
constant MadeClosure = make-header-closure(4);
our sub constant-closure-through-header() { MadeClosure.() }

my role ClassOfRole { my class C does Positional[::?CLASS] { our $closure = -> { 1 } }; method c { C.^name } }
my class DoesClassOfRole does ClassOfRole { }
our sub class-of-role-in-header() { DoesClassOfRole.c }
