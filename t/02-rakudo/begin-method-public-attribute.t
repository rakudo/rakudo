use Test;

plan 6;

# A method made at BEGIN time is compiled before CHECK time. The self that
# a $.foo access calls its method on must be available in it.

is EVAL(q:to/CODE/), 42, 'a $.foo access in a method made in a BEGIN statement';
    my $m;
    BEGIN $m = method { $.gs };
    my class C { method gs { 42 } }
    $m(C.new)
    CODE

is EVAL(q:to/CODE/), 42, 'a $.foo access in a method made in a BEGIN in a class';
    my class C {
        method gs { 42 }
        my $m;
        BEGIN $m = method { $.gs };
        method op { $m(self) }
    }
    C.new.op
    CODE

is-deeply EVAL(q:to/CODE/), (4, 2), 'an @.foo access in a method made at BEGIN time';
    my $m = BEGIN method { @.gs };
    my class C { method gs { 4, 2 } }
    $m(C.new).List
    CODE

is EVAL(q:to/CODE/), 5, 'a $.foo access with arguments in a method made at BEGIN time';
    my $m = BEGIN method { $.gs(5) };
    my class C { method gs($x) { $x } }
    $m(C.new)
    CODE

is EVAL(q:to/CODE/), 'v=42', 'an interpolated $.foo access in a method made at BEGIN time';
    my $m = BEGIN method { "v=$.gs()" };
    my class C { method gs { 42 } }
    $m(C.new)
    CODE

is EVAL(q:to/CODE/), 42, 'a $.foo access in a method made at BEGIN time in a role';
    my role R {
        method gs { 42 }
        my $m;
        BEGIN $m = method { $.gs };
        method op { $m(self) }
    }
    my class C does R { }
    C.new.op
    CODE

# vim: expandtab shiftwidth=4
