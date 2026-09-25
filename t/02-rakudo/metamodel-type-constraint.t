use Test;

# The metamodel classes and roles are NQP types with NQP metaobjects.
# They work as parameter, variable, attribute and subset types, and a
# role can do a metamodel role.

plan 15;

is EVAL(q[sub f(Metamodel::ClassHOW $h) { $h.name(Int) }; f(Int.HOW)]), 'Int',
    'a parameter typed with a metamodel class binds a matching argument';

is EVAL(q[sub f(Metamodel::Naming $h) { $h.name(Int) }; f(Int.HOW)]), 'Int',
    'a parameter typed with a metamodel role binds a matching argument';

is EVAL(q[
    multi f(Metamodel::DefiniteHOW, Mu \t) { 'definite' }
    multi f(Metamodel::ClassHOW, Mu \t) { 'class' }
    f(Int:D.HOW, Int:D) ~ ' ' ~ f(Int.HOW, Int)
]), 'definite class',
    'a multi dispatches on a metamodel class type';

is EVAL(q[my Metamodel::ClassHOW $h = Int.HOW; $h.name(Int)]), 'Int',
    'a scalar variable typed with a metamodel class accepts a matching value';

is EVAL(q[my Metamodel::Naming $h = Int.HOW; $h.name(Int)]), 'Int',
    'a scalar variable typed with a metamodel role accepts a matching value';

throws-like q[my Metamodel::ClassHOW $h = Int:D.HOW], X::TypeCheck::Assignment,
    'a scalar variable typed with a metamodel class rejects a different metaobject';

is EVAL(q[my Metamodel::ClassHOW @a = Int.HOW, Str.HOW; @a.elems]), 2,
    'an array variable typed with a metamodel class accepts matching elements';

is EVAL(q[my Metamodel::ClassHOW %h = a => Int.HOW; %h<a>.name(Int)]), 'Int',
    'a hash variable typed with a metamodel class accepts a matching value';

is EVAL(q[my $h of Metamodel::ClassHOW = Int.HOW; $h.name(Int)]), 'Int',
    'an of trait with a metamodel class types the variable';

is EVAL(q[my class C { has Metamodel::ClassHOW $.h }; C.new(h => Int.HOW).h.name(Int)]), 'Int',
    'an attribute typed with a metamodel class accepts a matching value';

is EVAL(q[subset S of Metamodel::ClassHOW; Int.HOW ~~ S]), True,
    'a subset of a metamodel class accepts a matching value';

is EVAL(q[my role R does Metamodel::Naming { }; my class C does R { }; my $c = C.new; $c.set_name(Int, "X"); $c.name(Int)]), "X",
    "a role that does a metamodel role composes it into a class";

is EVAL(q[my role R does Metamodel::Naming { }; my class C does R { }; sub f(Metamodel::Naming $n) { "bound" }; f(C.new)]), "bound",
    "a class doing such a role type checks against the metamodel role";

is EVAL(q[my role R[::T] does Metamodel::Naming { }; my class C does R[Int] { }; my $c = C.new; $c.set_name(Int, "X"); $c.name(Int)]), "X",
    "a parametric role that does a metamodel role composes it into a class";

is EVAL(q[my role R[::T] does Metamodel::Naming { }; sub f(Metamodel::Naming $n) { "bound" }; f(R[Int])]), "bound",
    "a curried role that does a metamodel role type checks against it";

# vim: expandtab shiftwidth=4
