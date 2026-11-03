use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 3;

# When -I references a custom CompUnit::Repository class, the Unknown
# placeholder is later resolved by $*REPO's REGISTER-DYNAMIC block
# calling `next-repo.need(:short-name<...>)`. That need() must NOT be
# recorded as a precomp dependency of the module currently being
# compiled, or the consumer module's precomp dep header ends up
# referencing the custom CUR.

my $tmp       = make-temp-dir;
my $cur-dir   = $tmp.add('cur-classes');
$cur-dir.add('CompUnit/Repository').mkdir(:parent);
my $mod-store = $tmp.add('module-store');
$mod-store.mkdir;

# Custom CUR. Class name is the string we look for in dep headers.
$cur-dir.add('CompUnit/Repository/SuspendTestCUR.rakumod').spurt: q:to/EOF/;
class CompUnit::Repository::SuspendTestCUR is CompUnit::Repository::FileSystem {
    method short-id(--> Str:D) { 'suspendtestcur' }
    method path-spec(::?CLASS:D: --> Str:D) {
        self.^name ~ '#' ~ self.prefix.absolute
    }
}
EOF

# Consumer that `use`s something so the compile path records at least
# one real dep. Without this, a "no deps recorded at all" bug would
# falsely pass.
$mod-store.add('SuspendTestConsumer.rakumod').spurt: q:to/EOF/;
unit module SuspendTestConsumer;
use Test;
our sub greet() { 'hello from SuspendTestConsumer' }
EOF

my $proc = run :out, :err,
    $*EXECUTABLE.absolute,
    '-I', "CompUnit::Repository::SuspendTestCUR#{$mod-store.absolute}",
    '-I', $cur-dir.absolute,
    '-e', 'use SuspendTestConsumer; say SuspendTestConsumer::greet()';

my $out      = $proc.out.slurp(:close);
my $exitcode = $proc.exitcode;
$proc.err.close;

# Scan the consumer module's .precomp tree for the custom CUR class
# name. The precomp file format is a text dep header followed by binary
# bytecode; decoding the first few KB as latin1 is enough to scan the
# header without choking on binary content.
my $leaked = False;
if $mod-store.add('.precomp').d {
    for Rakudo::Internals.DIR-RECURSE($mod-store.add('.precomp').absolute) -> $path {
        my $f = $path.IO;
        next if $f.basename eq 'CACHEDIR.TAG';
        next if $f.basename.ends-with('.lock');
        next if $f.basename.ends-with('.repo-id');
        my $bytes     = $f.slurp(:bin);
        my $head-size = $bytes.bytes min 4096;
        my $head      = $bytes.subbuf(0, $head-size).decode('latin1', :replacement('?'));
        $leaked = True if $head.contains('SuspendTestCUR');
    }
}

is  $exitcode, 0,                                  'subprocess exited cleanly';
is  $out.chomp, 'hello from SuspendTestConsumer',  'consumer module ran through the custom CUR chain';
nok $leaked,                                       'SuspendTestCUR is absent from consumer precomp dep header';

# vim: expandtab shiftwidth=4
