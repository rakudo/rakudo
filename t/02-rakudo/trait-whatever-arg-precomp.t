use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 4;

# A WhateverCode trait argument must survive precompilation. The closure
# used to be built by running a throwaway BEGIN-time thunk, so the
# serialized closure carried an outer frame that no longer matched its
# static frame on load:
#   provided outer frame ... does not match expected static frame '<unit>'
# Interpreting the curried argument as a static block keeps the closure
# serializable.

sub precomp-and-run($name, $source, $call, $expected, $desc) {
    my $tmp       = make-temp-dir;
    my $mod-store = $tmp.add('module-store');
    $mod-store.mkdir;
    $mod-store.add("$name.rakumod").spurt: $source;

    my $proc = run :out, :err,
        $*EXECUTABLE.absolute,
        '-I', $mod-store.absolute,
        '-e', "use $name; print $call";

    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);

    subtest $desc => {
        plan 3;
        is  $proc.exitcode, 0, 'exits cleanly';
        nok $err.contains('does not match expected static frame'),
            'no outer frame mismatch';
        is  $out, $expected, 'produces the expected result';
    }
}

my $attribute-trait = q:to/EOF/;
unit module AttrTrait;
my %STORE;
multi sub trait_mod:<is>(Attribute:D $attr, :&kept!) is export {
    %STORE{$attr.name} = &kept;
}
sub kept-for($name) is export { %STORE{$name} }
class C is export { has $.x is kept(*.succ) }
EOF
precomp-and-run 'AttrTrait', $attribute-trait, q|kept-for('$!x')(41)|, '42',
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
precomp-and-run 'SubTrait', $routine-trait, q|check-for('f')(5)|, 'True',
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
precomp-and-run 'MixedTrait', $mixed-args,
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
precomp-and-run 'TwoStar', $two-args, q|combined-for('$!x')(40, 2)|, '42',
    'a two argument WhateverCode trait argument';

# vim: expandtab shiftwidth=4
