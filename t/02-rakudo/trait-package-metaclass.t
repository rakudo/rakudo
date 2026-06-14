use Test;

# A trait sees the enclosing package's own metaclass through the
# dynamic $*PACKAGE.

plan 7;

role Marked { method marked { 'yes' } }

multi trait_mod:<is>(Attribute:D $a, :$mark!) {
    $*PACKAGE.HOW does Marked unless $*PACKAGE.HOW ~~ Marked;
}
class Marker {
    has $.x is mark;
}
ok Marker.HOW ~~ Marked, 'role mixed into $*PACKAGE.HOW from an attribute trait persists to the class';
is Marker.HOW.marked, 'yes', 'the mixed-in metaclass method is callable on the composed type';

# The HOW the trait sees is the enclosing package's own metaclass: a ClassHOW
# in a class body, a ParametricRoleHOW in a role body.
multi trait_mod:<is>(Attribute:D $a, :$branch!) {
    my $kind = do given $*PACKAGE.HOW {
        when Metamodel::ParametricRoleHOW { 'role' }
        when Metamodel::ClassHOW          { 'class' }
        default                           { 'other' }
    }
    $*PACKAGE.^add_method('how-kind', my method () { $kind });
}
class InClass { has $.x is branch; }
role  InRole  { has $.y is branch; }
class DoesInRole does InRole {}
is InClass.how-kind, 'class', 'an attribute trait in a class body sees the class metaclass';
is DoesInRole.how-kind, 'role', 'an attribute trait in a role body sees the role metaclass';

# A trait on the package declaration itself reaches that package's own type
# object, and for a nested package the inner type, not the enclosing one.
multi trait_mod:<is>(Mu:U \type, :$selfmark!) {
    $*PACKAGE.HOW does Marked unless $*PACKAGE.HOW ~~ Marked;
}
class SelfMarked is selfmark { }
ok SelfMarked.HOW ~~ Marked, 'a trait on the package itself reaches the package type object';

class Outer {
    class Inner is selfmark { }
}
ok Outer::Inner.HOW ~~ Marked, 'a package-self trait on a nested package reaches the nested type';
nok Outer.HOW ~~ Marked, 'and does not reach the enclosing package';

# vim: expandtab shiftwidth=4
