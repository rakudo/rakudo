use Test;

# An attribute whose default value is outside Any (e.g. a Junction) must
# bind during object construction. The generated initializer's $_ (the
# attribute's current value) was typed Any, which a Junction cannot bind to.

plan 3;

my $jc;
lives-ok { my class C { has Junction $.c = all() }; $jc = C.new.c },
    'an attribute with a Junction default value constructs';
is $jc.^name, 'Junction', 'the Junction default bound as a Junction';

lives-ok { my class C { has $.c = self.seed; method seed { 42 } }; C.new },
    'a default value referencing self still works';

# vim: expandtab shiftwidth=4
