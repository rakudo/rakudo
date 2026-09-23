use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

$*VM.name eq 'moar' or plan :skip-all<Line coverage logging is MoarVM only>;

plan 5;

# The code that binds a routine's signature runs before any statement of
# its body, so a called routine reports its declaration line as covered.

my $script = make-temp-file :content(q:to/CODE/);
unit class Foo;
method with-signature($path) {
    $path ~ "!"
}
method without-signature {
    42
}
sub with-signature($a) {
    $a + 1
}
sub never-called($a) {
    $a - 1
}
Foo.with-signature("a");
Foo.without-signature;
with-signature(1);
CODE

my $log = make-temp-file;
my %env = |%*ENV, MVM_COVERAGE_LOG => ~$log, MVM_COVERAGE_FILES => $script.basename;
my $proc = run $*EXECUTABLE, $script, :%env, :out, :err;
$proc.out.slurp(:close);
$proc.err.slurp(:close);
is $proc.exitcode, 0, 'script run with coverage logging exits cleanly';

my $covered = $log.lines.grep(*.contains: $script.basename).map(*.words.tail.Int).Set;

ok $covered{2}, 'called method with a signature reports its declaration line';
ok $covered{5}, 'called method without a signature reports its declaration line';
ok $covered{8}, 'called sub with a signature reports its declaration line';
nok $covered{11}, 'sub that is never called does not report its declaration line';

# vim: expandtab shiftwidth=4
