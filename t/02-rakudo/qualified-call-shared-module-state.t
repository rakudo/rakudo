use lib <t/02-rakudo/test-packages>;
use Test;

plan 4;

# Qualified calls from two precompiled consumers must reach the same
# routine and the same module state. Binding the callee at consumer
# precompilation time serialized a private copy per consumer, which is
# how Intl::CLDR's StrDecode string table read back empty.

use QualifiedCallSetter;
use QualifiedCallGetter;
use QualifiedCallState;

QualifiedCallSetter::set-it(42);
is QualifiedCallGetter::get-it(), 42,
    'module state set through one precompiled consumer is visible through another';
ok QualifiedCallSetter::whoami-via-call() === QualifiedCallGetter::whoami-via-call(),
    'qualified calls from both consumers invoke the same routine instance';
ok QualifiedCallSetter::whoami-via-call() === &QualifiedCallState::whoami,
    'the setter consumer invokes the routine in the live stash';
ok QualifiedCallGetter::whoami-via-call() === &QualifiedCallState::whoami,
    'the getter consumer invokes the routine in the live stash';
