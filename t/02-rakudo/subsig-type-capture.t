use Test;

plan 10;

# A type capture declared in a sub-signature (`$p (::T, ...)`) must create a
# block-local `T`, both so the binder can store into it and so the body can
# refer to it. RakuAST only declared captures from top-level parameters, so a
# sub-signature capture was missing from the frame.

# Pointy block (the form Configuration::Utils uses).
for ((1, 'x'),) -> $p ($n, ::T) {
    is T, Str, 'a pointy-block sub-signature type capture is visible';
}

# Declared but never used in the body, as Configuration::Utils does.
lives-ok {
    for ((1, 'x'),) -> $p ($n, ::T, |) { }
}, 'an unused sub-signature type capture binds without error';

# Subroutine.
sub in-sub($p ($n, ::T)) { T }
is in-sub((1, 'x')), Str, 'a sub sub-signature type capture is visible';

# Method.
class C {
    method m($p ($n, ::T)) { T }
}
is C.m((1, 42)), Int, 'a method sub-signature type capture is visible';

# Named sub-signature capture attached to a named parameter.
sub named-cap($p (::T :$value, |)) { T }
is named-cap({ value => 'y' }), Str, 'a named sub-signature type capture is visible';

# Nested sub-signatures each declare their own capture.
sub nested($p ($n (::U), ::T)) { U.^name ~ ' ' ~ T.^name }
is nested(((1,), 'z')), 'Int Str', 'nested sub-signature captures each resolve';

# A top-level capture still works alongside.
sub both(::Top $x, $p ($n, ::T)) { Top.^name ~ ' ' ~ T.^name }
is both(1, (2, 'w')), 'Int Str', 'top-level and sub-signature captures coexist';

# List declaration `my (::T, ...) := ...` binds its captures into the
# enclosing scope.
my (::LT, $lx) := (Int, 5);
is LT, Int, 'a list-declaration type capture is visible';

# The captured type is usable as a constraint on a later declaration.
my LT $constrained = 3;
is $constrained, 3, 'a list-declaration type capture works as a constraint';

# Nested list declaration captures each resolve.
my (::NT, (::NU, $ny)) := (Int, (Str, 'x'));
is NT.^name ~ ' ' ~ NU.^name, 'Int Str', 'nested list-declaration captures each resolve';

# vim: expandtab shiftwidth=4
