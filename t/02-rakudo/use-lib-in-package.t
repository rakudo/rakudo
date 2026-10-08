use Test;

plan 7;

use MONKEY-SEE-NO-EVAL;

throws-like q[class UseLibClass { use lib 'x' }], X::Package::UseLib,
    what => 'class',
    'use lib inside a class body is refused';
throws-like q[role UseLibRole { use lib 'x' }], X::Package::UseLib,
    what => 'role',
    'use lib inside a role body is refused';
throws-like q[module UseLibModule { use lib 'x' }], X::Package::UseLib,
    what => 'module',
    'use lib inside a module body is refused';
throws-like q[grammar UseLibGrammar { use lib 'x' }], X::Package::UseLib,
    what => 'grammar',
    'use lib inside a grammar body is refused';
throws-like q[unit class UseLibUnit; use lib 'x'], X::Package::UseLib,
    what => 'class',
    'use lib after a unit class declaration is refused';
throws-like q[class UseLibMethod { method m { use lib 'x' } }], X::Package::UseLib,
    what => 'class',
    'use lib inside a method of a class is refused';
lives-ok { EVAL q[class UseLibAfter { }; use lib 'x'] },
    'use lib after a class body has closed is allowed';

# vim: expandtab shiftwidth=4
