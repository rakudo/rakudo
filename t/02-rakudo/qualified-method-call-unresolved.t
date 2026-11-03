use Test;

plan 4;

# A qualified method call `self.Some::Type::method` whose qualifier is not
# resolvable at compile time (for example a type only loaded by the consuming
# program) must compile, deferring the qualifier to a runtime lookup, rather
# than failing to compile.
lives-ok { EVAL 'role R { method m { self.No::Such::Type::foo } }' },
    'a qualified call to a not-in-scope qualifier compiles';

# A qualifier that is in scope still dispatches to that type's method.
is do {
    class A { method m { 'A' } }
    class B is A { method m { self.A::m } }
    B.new.m
}, 'A', 'a qualified call to an in-scope type dispatches to its method';

# Deferring to runtime keeps the diagnostic: a qualifier that resolves to
# nothing reports an invalid qualifier when the call runs.
throws-like { my class C { method m { self.No::Such::Type::foo } }; C.new.m },
    X::Method::InvalidQualifier,
    'a qualifier that resolves to nothing reports an invalid qualifier at runtime';

# The hyper form takes the same path.
is do {
    class HA { method m { 'A' } }
    class HB is HA { method m { 'B' } }
    (HB.new, HB.new)>>.HA::m
}, ('A', 'A'), 'a hyper qualified call dispatches to the qualifier type';

# vim: expandtab shiftwidth=4
