use Test;

plan 8;

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
# Issue R#6065 / PR R#6073: a *different* hang triggered when the user
# role's type capture name happened to collide with a core role's
# capture name (e.g. user `role R[::TValue]` versus
# `Array::Typed[::TValue]`).  Root cause: TypeEnv keys generic captures
# by name string, so the lookup of "TValue" returned the very same
# generic object we asked about, and GenericHOW.instantiate_generic
# recursed forever on that self-resolution.
#
# A regression here will hang at compile time of the EVAL body, which a
# per-test-file timeout in CI will catch.

# --- Issue 6071: self-recursive :D INSTANTIATE-GENERIC -----------------

lives-ok
    { EVAL 'role R6071A[::T = Int] { has T %h }; class C6071A does R6071A {}; True' },
    'Issue 6071 golf: role with generic hash value, composed into class';

lives-ok
    { EVAL 'role R6071B[::T] { has T %h }; R6071B[Int].new; True' },
    'role with generic hash value, parameterized and instantiated';

lives-ok
    { EVAL 'role R6071C[::T] { has %h{Int} of T }; class C6071C does R6071C[Str] {}; True' },
    'role with `has %h{Int} of T` (the issue 6071 attribute shape)';

# --- Name-collision self-resolution in GenericHOW ---------------------

lives-ok
    { EVAL 'role R6073A[::TValue] { has TValue @.a }; R6073A[Int].new; True' },
    'PR 6073 golf: user `::TValue` colliding with Array::Typed `::TValue`';

lives-ok
    { EVAL 'role R6073B[::TValue] { has TValue %h }; R6073B[Int].new; True' },
    'user `::TValue` colliding with Hash::Typed `::TValue`';

lives-ok
    { EVAL 'role R6073C[::TDefault] { has TDefault %h }; R6073C[Int].new; True' },
    'user `::TDefault` colliding with Hash::Typed `::TDefault`';

# Types must actually resolve, not just silently leave the attribute generic.

is do {
    EVAL 'role R6073D[::TValue] { has TValue @.a }; R6073D[Int].new.a.^name'
}, 'Array[Int]',
    'collision-named capture still resolves the array element type';

is do {
    EVAL 'role R6073E[::TValue] { has TValue %.h }; R6073E[Int].new.h.of.^name'
}, 'Int',
    'collision-named capture still resolves the hash value type';

# vim: ft=raku
