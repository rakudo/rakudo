use Test;

plan 5;

# A parametric role declared more than once forms a role group across its
# variants. This must work even when the compound name's first part is a setting
# type (e.g. Hash::Sorted), where the group lives in that type's WHO; the second
# variant must not be rejected as a redeclaration.

role Hash::Variant[::T]            { method which { 'plain' } }
role Hash::Variant[::T, :$extra!]  { method which { 'extra' } }

is Hash::Variant[Int].new.which, 'plain',
    'the first variant of a role group under a setting-named package resolves';
is Hash::Variant[Int, :extra].new.which, 'extra',
    'the second variant of that role group resolves, not a redeclaration';

# The same holds for a non-setting compound name.
role My::Variant[::T]           { method which { 'a' } }
role My::Variant[::T, :$y!]     { method which { 'b' } }

is My::Variant[Str].new.which, 'a',
    'a role group under a plain compound name still resolves its first variant';
is My::Variant[Str, :y].new.which, 'b',
    'a role group under a plain compound name resolves its second variant';

# A genuine redeclaration under a setting-named package is still rejected.
throws-like 'class Hash::Dup { }; class Hash::Dup { }', X::Redeclaration,
    'redeclaring a class under a setting-named package is still an error';

# vim: expandtab shiftwidth=4
