use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use nqp;

plan 6;

# A GLOBAL package variable written at run time by a precompiled module
# must be visible outside that module. Each compilation unit has its own
# GLOBAL package that merges into the process-wide one at load time, so a
# reference compiled into the module must look the merged package up at
# run time rather than address the unit's own serialized copy.

my $mod-store = make-temp-dir;
$mod-store.add('GlobalWriter.rakumod').spurt: q:to/EOF/;
    unit module GlobalWriter;
    sub set-it is export { %GLOBAL::CROSS-UNIT-DEFAULTS = (default => 42) }
    sub get-dynamic is export { %*CROSS-UNIT-DEFAULTS<default> }
    sub set-scalar is export { $GLOBAL::cross-unit-scalar = 5 }
    EOF
my @compiler-args = '-I', $mod-store.absolute;

is-run 'use GlobalWriter; set-it; print %GLOBAL::CROSS-UNIT-DEFAULTS<default>',
    :@compiler-args, :out<42>,
    'a hash written to GLOBAL by a module is readable outside it';

is-run 'use GlobalWriter; set-it; print get-dynamic()',
    :@compiler-args, :out<42>,
    'a GLOBAL hash reaches the dynamic variable fallback';

is-run 'use GlobalWriter; set-scalar; print $*cross-unit-scalar',
    :@compiler-args, :out<5>,
    'a scalar written to GLOBAL by a module reaches the dynamic fallback';

# A run-time write through a GLOBAL-rooted name can vivify a stub package
# under a name some unit also declares. Importing that unit's GLOBALish
# must unify the two packages rather than install a lexical alias that
# shadows the stub and hides its symbols.

$mod-store.add('SelfWriter.rakumod').spurt: q:to/EOF/;
    package SelfNamespace {
        &GLOBAL::SelfNamespace::dld = &dld;
        sub dld { 42 }
    }
    EOF

is-run 'use SelfWriter; print SelfNamespace::dld()',
    :@compiler-args, :out<42>,
    'a sub a module binds into a GLOBAL-rooted package it also declares is callable by qualified name';

is-run 'use SelfWriter; print SelfNamespace.WHO =:= GLOBAL::SelfNamespace.WHO',
    :@compiler-args, :out<True>,
    'the imported package and the GLOBAL entry of the same name are one object';

$mod-store.add('StubWriter.rakumod').spurt: q:to/EOF/;
    unit module StubWriter;
    $GLOBAL::CrossUnitClass::stub-val = 5;
    EOF
$mod-store.add('RealClass.rakumod').spurt: q:to/EOF/;
    class CrossUnitClass {
        method val() { 37 }
    }
    EOF

todo 'the legacy frontend does not merge a vivified GLOBAL stub into an imported class'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is-run 'use StubWriter; use RealClass; print CrossUnitClass.val + $CrossUnitClass::stub-val',
    :@compiler-args, :out<42>,
    'a class merges symbols from a same-named stub package an earlier module vivified in GLOBAL';

# vim: expandtab shiftwidth=4
