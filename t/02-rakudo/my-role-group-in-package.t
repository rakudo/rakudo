use Test;

plan 6;

# A `my`-scoped parametric role declared with several variants inside a package
# body forms one role group. The later variants must join the group, not be
# rejected as a redeclaration. A `my` role installs lexically under its simple
# name, so the group lookup must use that name and not the package-qualified one.

lives-ok {
    EVAL q:to/CODE/;
        class C {
            my role R[Str $c]     { method m { "one:$c" } }
            my role R[Str $c, $x] { method m { "two:$c:$x" } }
            my role R[Str $c, &a] { method m { "fn:$c" } }
        }
    CODE
}, 'three my-role variants in a class body form a group';

lives-ok {
    EVAL q:to/CODE/;
        role X {
            my role R[Str $c]     { }
            my role R[Str $c, $x] { }
        }
    CODE
}, 'my-role variants join the group inside a role body too';

# The joined group dispatches to the correct variant by signature.
my $dispatch = EVAL q:to/CODE/;
    class D {
        my role R[Str $c]     { method m { "one:$c" } }
        my role R[Str $c, $x] { method m { "two:$c:$x" } }
        method one { (Any but R["hi"]).m }
        method two { (Any but R["hi", "there"]).m }
    }
    D
CODE
is $dispatch.one, 'one:hi',        'group dispatches to the single-arg variant';
is $dispatch.two, 'two:hi:there',  'group dispatches to the two-arg variant';

# A `my` role group still shadows an outer same-named lexical rather than
# colliding with it, matching a top-level group.
lives-ok {
    EVAL q:to/CODE/;
        my class R { }
        class E {
            my role R[Str $c]     { }
            my role R[Str $c, $x] { }
        }
    CODE
}, 'a my-role group inside a package shadows an outer lexical of the same name';

# A genuine redeclaration is still rejected.
throws-like q:to/CODE/, X::Redeclaration,
    class F {
        my role R[Str $c] { }
        my class R { }
    }
    CODE
    'redeclaring the group name as a class is still an error';

# vim: expandtab shiftwidth=4
