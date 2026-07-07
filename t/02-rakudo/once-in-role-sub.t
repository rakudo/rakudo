use Test;

plan 5;

# A `once` inside a `sub` in a role body reaches code generation before the
# role's scope has produced the once's implicit state declaration, which left
# that declaration a bare type object and the role failed to compile.

my $count = 0;
role Ronce {
    sub bump() { once { ++$count } }
    method fire() { bump() }
}
my class C does Ronce {}
my $c = C.new;

is $c.fire, 1, 'once in a role sub returns its value';
is $c.fire, 1, 'once in a role sub does not re-run on a later call';
is $count, 1, 'the once body ran exactly once';

# A `once` with a bare expression rather than a block, in a role sub.
role Rval {
    sub answer() { once 40 + 2 }
    method get() { answer() }
}
my class D does Rval {}
is D.new.get, 42, 'once with an expression in a role sub returns its value';

# An exported sub in a role, as in the module that surfaced this.
role Rexp {
    sub tag() is export { once 'TAG' }
    method t() { tag() }
}
my class E does Rexp {}
is E.new.t, 'TAG', 'once in an exported sub in a role works';

# vim: expandtab shiftwidth=4
