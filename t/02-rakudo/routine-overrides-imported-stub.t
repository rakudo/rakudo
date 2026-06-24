use lib <t/packages/02-rakudo/lib>;
use Test;
use MONKEY-SEE-NO-EVAL;
use SubsStubHelper;   # exports a `...` stub `&stub-export` and a real `&real-export`

plan 4;

# A routine whose body is just a stub (`...`) reports itself as a stub.
is (sub { ... }).yada, True, 'a stub sub reports .yada';
nok (sub { 42 }).yada, 'a non-stub sub does not report .yada';

# A real routine declaration may override an imported stub of the same name.
sub stub-export() { "overridden" }
is stub-export(), "overridden",
    'a real routine overrides an imported stub of the same name';

# Overriding a non-stub imported routine is still a redeclaration.
throws-like
    'use SubsStubHelper; sub real-export() { 0 }',
    X::Redeclaration,
    'overriding a non-stub imported routine is rejected';

# vim: expandtab shiftwidth=4
