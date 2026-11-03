use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 4;

# A stub routine (`...`) is a forward declaration. Defining a routine of the
# same name later replaces it, with no redeclaration warning or error.

is-run 'sub foo {...}; sub foo { 42 }; print foo()',
    'a definition replaces a forward-declared stub without a warning',
    :out("42"), :err("");

is-run 'sub foo {...}; sub foo {...}; print "ok"',
    'a second stub replaces a forward-declared stub without a warning',
    :out("ok"), :err("");

# Redefining a non-stub routine is still a redeclaration.
is-run 'sub foo { 1 }; sub foo { 2 }',
    'redefining a non-stub routine is rejected',
    :err(/'Redeclaration of routine'/),
    :exitcode(1);

is-run 'sub foo { 1 }; sub foo {...}',
    'a stub does not replace an existing non-stub routine',
    :err(/'Redeclaration of routine'/),
    :exitcode(1);

# vim: expandtab shiftwidth=4
