use lib <t/02-rakudo/test-packages>;
use Test;
use QualifiedOurAnchor;

plan 6;

# A qualified our-scoped declaration anchors at its leading package:
# the lexically visible one when there is one, GLOBAL otherwise, never
# the package of the enclosing scope.

class C { our $Foo::Bar::x = 42; our &Foo::Bar::f = sub { "hi" } }

is $Foo::Bar::x, 42,
    'a qualified our variable declared in a class body anchors at GLOBAL';
is Foo::Bar::f(), 'hi',
    'a qualified our sub declared in a class body anchors at GLOBAL';

my class M { }
class C2 { our $M::z = 3 }
is $M::z, 3,
    'a qualified our variable anchors at a lexically visible package';
ok M::.EXISTS-KEY('$z'),
    'the lexically visible package stash holds the variable';

is $QOA::Target::flag, False,
    'a qualified our variable from a used module is visible';
QOA::Target::set-flag();
is $QOA::Target::flag, True,
    'a qualified our sub from a used module updates the shared variable';
