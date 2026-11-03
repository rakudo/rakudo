use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 3;

# The instantiated attribute of a generic role, with its boxed type,
# container descriptor, and auto viv container, survives the
# precompilation store.

my $lib = make-temp-dir;
$lib.add('GenNat.rakumod').spurt: q:to/MODULE/;
    unit module GenNat;
    role R[::T] is export { has T $.x; method w() { $!x++; $!x } }
    class C does R[int] is export { }
    MODULE

my ($step, $constructed, $type-ok) = EVAL qq:to/CODE/;
    use lib '$lib.absolute()';
    use GenNat;
    (C.new.w,
     C.new(x => 5).x,
     C.^attributes.first(*.name eq '\$!x').type =:= Int)
    CODE

is $step, 1, 'a precompiled class steps a generic attribute instantiated with int';
is $constructed, 5, 'a precompiled class constructs a generic attribute instantiated with int';
ok $type-ok, 'a precompiled int instantiated generic attribute reports Int as its type';

# vim: expandtab shiftwidth=4
