use Test;

plan 8;

# A role body is code-generated when the role is composed, ahead of the
# compilation unit's check pass. A forward reference inside the body to a `sub`
# declared later must still resolve, the same as it does in a class.

# Forward reference in a parameter default.
role R1 { method m(&cb = &helper) { cb() }; sub helper { 42 } }
class C1 does R1 {}
is C1.m, 42, 'a forward sub reference in a role method parameter default resolves';

# Forward reference in an attribute default.
role R2 { has &.cb = &helper; sub helper { 43 } }
class C2 does R2 {}
is C2.new.cb.(), 43, 'a forward sub reference in a role attribute default resolves';

# Forward reference in a method body.
role R3 { method m { my &c = &helper; c() }; sub helper { 44 } }
class C3 does R3 {}
is C3.m, 44, 'a forward sub reference in a role method body resolves';

# Forward reference to a method-local declaration (deeper than the body scope).
role R4 { method m { my $c = &helper; my sub helper { 45 }; $c() } }
class C4 does R4 {}
is C4.m, 45, 'a forward reference to a method-local sub in a role resolves';

# Forward reference inside a package nested in the role body.
role R5 { my class Inner { method i(&cb = &g) { cb() }; sub g { 46 } }; method m { Inner.new.i } }
class C5 does R5 {}
is C5.m, 46, 'a forward sub reference inside a package nested in a role resolves';

# Reference to a sub declared later in the enclosing compilation unit, outside
# the role, but in scope by the time the role is consumed. (A sub declared
# after the consumer cannot be resolved at compose time and is not handled.)
role R6 { method m(&cb = &outer-helper) { cb() } }
sub outer-helper { 47 }
class C6 does R6 {}
is C6.m, 47, 'a reference to a later outer sub, in scope by consumption, resolves';

# Still works when the sub precedes the method (no regression).
role R7 { sub helper { 48 }; method m(&cb = &helper) { cb() } }
class C7 does R7 {}
is C7.m, 48, 'a backward sub reference in a role still resolves';

# A role-local sub shadows an outer sub of the same name.
sub shadowed { 100 }
role R8 { method m(&cb = &shadowed) { cb() }; sub shadowed { 200 } }
class C8 does R8 {}
is C8.m, 200, 'a role-local sub wins over an outer sub of the same name';

# vim: expandtab shiftwidth=4
