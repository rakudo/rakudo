use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 3;

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

# vim: expandtab shiftwidth=4
