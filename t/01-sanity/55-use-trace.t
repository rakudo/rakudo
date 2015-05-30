use v6;

use Test;

plan 3;

my $filename = "$?FILE.trace";
my $stderr   = "$filename.stderr";

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

my $p = pipe( "$*EXECUTABLE $filename 2>$stderr" );
ok $p,                            "did we get a handle?";
is $p.lines.join, "42",           "is the program output ok?";
is slurp($stderr), qq:to/STDERR/, "is the trace ok?";
4 ($filename:4)
\$a++
10 ($filename:10)
\$a -= 1
STDERR

unlink $filename, $stderr;
