use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 6;

# A WhateverCode trait argument must survive precompilation. The closure
# used to be built by running a throwaway BEGIN-time thunk, so the
# serialized closure carried an outer frame that no longer matched its
# static frame on load:
#   provided outer frame ... does not match expected static frame '<unit>'
# Interpreting the curried argument as a static block keeps the closure
# serializable.

my $attribute-trait = q:to/EOF/;
unit module AttrTrait;
my %STORE;
multi sub trait_mod:<is>(Attribute:D $attr, :&kept!) is export {
    %STORE{$attr.name} = &kept;
}
sub kept-for($name) is export { %STORE{$name} }
class C is export { has $.x is kept(*.succ) }
EOF
is-run-precompiled 'AttrTrait', $attribute-trait, q|kept-for('$!x')(41)|, '42',
    'a WhateverCode argument to an attribute trait';

my $routine-trait = q:to/EOF/;
unit module SubTrait;
my %CHECKS;
multi sub trait_mod:<is>(Routine:D $r, :&check!) is export {
    %CHECKS{$r.name} = &check;
}
sub check-for($name) is export { %CHECKS{$name} }
sub f() is check(* > 0) is export { }
EOF
is-run-precompiled 'SubTrait', $routine-trait, q|check-for('f')(5)|, 'True',
    'a WhateverCode argument to a routine trait';

my $mixed-args = q:to/EOF/;
unit module MixedTrait;
my %STORE;
multi sub trait_mod:<is>(Attribute:D $attr, :$linked!) is export {
    %STORE{$attr.name} = $linked;
}
sub linked-for($name) is export { %STORE{$name} }
class C is export { has $.x is linked(*.succ, :name<foo>) }
EOF
is-run-precompiled 'MixedTrait', $mixed-args,
    Q[do { my ($code, $pair) = |linked-for('$!x'); "{$code(41)} {$pair.key}" }],
    '42 name',
    'a WhateverCode alongside other trait arguments';

my $two-args = q:to/EOF/;
unit module TwoStar;
my %STORE;
multi sub trait_mod:<is>(Attribute:D $attr, :&combined!) is export {
    %STORE{$attr.name} = &combined;
}
sub combined-for($name) is export { %STORE{$name} }
class C is export { has $.x is combined(* + *) }
EOF
is-run-precompiled 'TwoStar', $two-args, q|combined-for('$!x')(40, 2)|, '42',
    'a two argument WhateverCode trait argument';

my $hyper-arg = q:to/EOF/;
unit module HyperArg;
class C is export { has &.f is default(** + 1) }
EOF
is-run-precompiled 'HyperArg', $hyper-arg, q|C.new.f.((1, 2))|, '2 3',
    'a HyperWhatever trait argument maps over its argument';

{
    my class H { has &.f is default(** + 1) }
    is-deeply H.new.f.((1, 2)).List, (2, 3),
        'a HyperWhatever attribute default maps over its argument';
}

# vim: expandtab shiftwidth=4
