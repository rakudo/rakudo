use Test;

plan 2;

# `my %h is SomeType` bare-creates the container only for the core immutable
# QuantHashes (Set/Bag/Mix), matched by identity against the setting. A
# user type that merely shares the name Set must be built with .new, so its
# BUILD runs.

is do {
    my class Set does Associative {
        has $.tag = 'built';   # only set when .new/BUILD runs, not bare create
        has %.store;
        method STORE(\pairs, :$INITIALIZE) {
            %!store = pairs.list.map(*.kv).flat;
            self
        }
        method AT-KEY($k) { %!store{$k} }
    }
    my %h is Set = (a => 1);
    %h.tag
}, 'built', 'a user type named Set is built with .new, not bare-created';

# The core Set is still bare-created and keeps its empty sentinel.
is do { my %h is Set = (); %h === set() }, True,
    'the core Set keeps its empty sentinel';

# vim: expandtab shiftwidth=4
