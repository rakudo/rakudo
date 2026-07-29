use lib <t/02-rakudo/test-packages>;
use Test;

plan 2;

# Qualified calls from two precompiled consumers must reach the same
# routine and the same module state. Binding the callee at consumer
# precompilation time serialized a private copy per consumer, which is
# how Intl::CLDR's StrDecode string table read back empty.

use QualifiedCallSetter;
use QualifiedCallGetter;

QualifiedCallSetter::set-it(42);
is QualifiedCallGetter::get-it(), 42,
    'module state set through one precompiled consumer is visible through another';
is QualifiedCallSetter::whoami-via-call(), QualifiedCallGetter::whoami-via-call(),
    'qualified calls from both consumers invoke the same routine instance';
