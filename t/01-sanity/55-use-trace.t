use v6;

use lib <lib>;
use Test;

plan 3;

my $filename = $*TMPDIR.add: "$?FILE.IO.basename().trace";
LEAVE unlink $filename;

spurt( $filename, q:to/CODE/ );
my $a = 42;
if $a {
    use trace;
    $a++;
    no trace;
    $a--;
}
$a += 1;
use trace;
$a -= 1;
no trace;
say $a;
CODE

my $p = run($*EXECUTABLE, $filename, :out, :err);
ok $p ~~ Proc,                    "did we get a Proc?";
is $p.out.lines.join, "42",       "is the program output ok?";
is $p.err.lines.join("\n"), qq:to/STDERR/.chomp, "is the trace ok?";
4 ($filename line 4)
\$a++
10 ($filename line 10)
\$a -= 1
STDERR

# vim: expandtab shiftwidth=4
