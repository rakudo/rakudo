use Test;

plan 4;

# Issue 6071: composing a parametric role whose body declared a typed-hash
# attribute (`has T %h` or `has %!instances{Int} of T`) with a generic
# type-capture as the value type would spin forever in
# Hash::Typed.INSTANTIATE-GENERIC (and Hash::Object.INSTANTIATE-GENERIC).
#
# Root cause: the :D variant called `self.INSTANTIATE-GENERIC(...)`,
# which with a :D invocant re-entered the same :D multi candidate
# instead of dispatching to the :U candidate.  Array::Typed used the
# correct `self.WHAT.INSTANTIATE-GENERIC(...)` and so worked.
#
# A regression here will hang at compile time of the EVAL body, which a
# per-test-file timeout in CI will catch.

lives-ok
    { EVAL 'role R6071A[::T = Int] { has T %h }; class C6071A does R6071A {}; True' },
    'Issue 6071 golf: role with generic hash value, composed into class';

lives-ok
    { EVAL 'role R6071B[::T] { has T %h }; R6071B[Int].new; True' },
    'role with generic hash value, parameterized and instantiated';

lives-ok
    { EVAL 'role R6071C[::T] { has %h{Int} of T }; class C6071C does R6071C[Str] {}; True' },
    'role with `has %h{Int} of T` (the issue 6071 attribute shape)';

lives-ok
    { EVAL 'role R6071D[::T] { has T @.a }; R6071D[Int].new; True' },
    'control: role with generic array value still works';

# vim: ft=raku
