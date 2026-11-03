use Test;

# An inline where-constraint is an anonymous refinement type, reported
# as <anon> in a failed type check.

plan 2;

throws-like { my Str $x where { $_ ne 'no' }; $x = 'no' },
    X::TypeCheck,
    message => /'<anon>'/,
    'where on a typed variable reports the constraint as <anon>';

throws-like { my class C { has Str $.y is rw where { $_ ne 'no' } }; C.new.y = 'no' },
    X::TypeCheck,
    message => /'<anon>'/,
    'where on a typed attribute reports the constraint as <anon>';

# vim: expandtab shiftwidth=4
