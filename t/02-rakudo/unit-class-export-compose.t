use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 2;

# Regression test for the `unit class` compose path under the RakuAST
# frontend. During body parsing of a `unit class`, the class sits on the
# resolver's open-packages stack. When the body declares any `is export`
# symbol, `Rakudo::Internals.EXPORT_SYMBOL` walks the open packages and
# previously called bare `.meta-object` on each to reach `.WHO`. That
# bare call ran `PRODUCE-META-OBJECT()` without `CompilerServices`,
# filling the cache via the degraded path. The package-def `IMPL-COMPOSE`
# that runs at end-of-file then returned the cached degraded value, and
# the MOP generated attribute accessors as runtime closures instead of
# capturing them as compile time QAST. Functionally correct, measurably
# slower. The structural assertion in `RakuAST::Package.meta-object`
# trips loudly if anyone re-introduces a bare reader before IMPL-COMPOSE.

my $tmp       = make-temp-dir;
my $mod-store = $tmp.add('module-store');
$mod-store.mkdir;

$mod-store.add('UnitClassExportCompose.rakumod').spurt: q:to/EOF/;
unit class UnitClassExportCompose is export;

has $.x = 42;
has Int $.n is rw = 7;

sub helper() is export { 'hi' }
EOF

my $proc = run :out, :err,
    $*EXECUTABLE.absolute,
    '-I', $mod-store.absolute,
    '-e', 'use UnitClassExportCompose;
           my $o = UnitClassExportCompose.new;
           say $o.x;
           say $o.n;
           $o.n = 99;
           say $o.n;
           say helper();';

my $err = $proc.err.slurp(:close);
my $out = $proc.out.slurp(:close);

is  $proc.exitcode, 0,
    'unit class with attributes plus `is export` precompiles and loads cleanly';
nok $err,
    'no errors or warnings in stderr';

# vim: expandtab shiftwidth=4
