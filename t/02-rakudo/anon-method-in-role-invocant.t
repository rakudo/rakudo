use Test;

# A named anon or my method in a role body takes an unconstrained
# invocant, so it stays callable when added to an unrelated type.

plan 5;

role R {
    method make-anon() { anon method named() { 'anon-' ~ self.^name } }
    method make-my()   { my method named()   { 'my-' ~ self.^name } }
}

class C { }
C.^add_method('borrowed', R.make-anon);
is C.new.borrowed, 'anon-C',
    'a named anon method from a role is callable when added to an unrelated class';

class D { }
D.^add_method('borrowed', R.make-anon);
is D.new.borrowed, 'anon-D', 'the same anon method works on a different type';

# A my method in a role body takes the same unconstrained invocant.
class E { }
E.^add_method('borrowed', R.make-my);
is E.new.borrowed, 'my-E', 'a my method from a role is callable when added to an unrelated class';

nok R.make-anon.signature.params[0].type.HOW ~~ Metamodel::ParametricRoleHOW,
    'the anon method invocant is not constrained to the concrete role';
nok R.make-my.signature.params[0].type.HOW ~~ Metamodel::ParametricRoleHOW,
    'the my method invocant is not constrained to the concrete role';

# vim: expandtab shiftwidth=4
