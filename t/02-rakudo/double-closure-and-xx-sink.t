use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 3;

# A `where { ... }` clause whose body is a single curried expression
# (e.g. `where { *.foo }`) is a malformed double closure. A nested
# WhateverCode inside a method-call argument is the call's argument,
# not the block's return value, and is well-formed.

throws-like
    { EVAL 'sub f($x where { *.so }) { }' },
    X::Syntax::Malformed,
    message => /:i 'double closure' /,
    '`where { *.so }` is rejected as a double closure';

lives-ok
    { EVAL 'sub f(@x where @x.map(* ~~ Int)) { }; f([1, 2])' },
    'WhateverCode inside a method call argument inside a where clause compiles';

# Side effects on the left of `xx` are observable when the resulting
# Seq is iterated, including by sink. Suppress the sink-context
# warning, which would be a false positive when the left has side
# effects. Compile-time worries print to NQP's stderr handle and
# can't be captured via `$*ERR`, so a subprocess is the cleanest
# way to assert the warning does not fire.

is-run '5 xx 2',
    :err(''),
    '`xx` in sink context emits no compile-time warning';

# vim: expandtab shiftwidth=4
