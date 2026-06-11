use MONKEY-SEE-NO-EVAL;
use Test;

plan 9;

# Each EVAL below runs at BEGIN time, while this file is still being
# compiled, so the EVAL'd unit is nested inside the file's compilation.

my $slash;
BEGIN { "abc" ~~ /b/; $slash = EVAL Q[$/] }
is $slash.Str, 'b', 'BEGIN-time string EVAL sees the caller $/';

my $bang;
BEGIN { try die "boom"; $bang = EVAL Q[$!] }
is $bang.message, 'boom', 'BEGIN-time string EVAL sees the caller $!';

my $topic;
sub topic-read() { $_ = 42; EVAL Q[$_] }
BEGIN { $topic = topic-read }
is $topic, 42, 'BEGIN-time string EVAL sees the caller $_';

my $assigned;
sub topic-write() { $_ = 0; EVAL Q[$_ = 5]; $_ }
BEGIN { $assigned = topic-write }
is $assigned, 5, 'BEGIN-time string EVAL writes through to the caller $_';

my $local;
BEGIN { my $x = 99; $local = EVAL Q[$x] }
is $local, 99, 'BEGIN-time string EVAL sees a lexical of the calling block';

my $pkg;
class PackageFromEval {
    BEGIN { $pkg = EVAL Q[$?PACKAGE] }
}
ok $pkg === PackageFromEval, 'BEGIN-time string EVAL sees the caller $?PACKAGE';

class OurVarFromEval {
    BEGIN { EVAL Q[our $from-eval = 42] }
}
is OurVarFromEval::<$from-eval>, 42,
    'our-scoped variable EVALed at BEGIN time lands in the caller package';
nok GLOBAL::<$from-eval>:exists,
    'our-scoped variable EVALed at BEGIN time does not leak into GLOBAL';

class OurSubFromEval {
    BEGIN { EVAL Q[our sub from-eval() { "made-in-eval" }] }
}
is OurSubFromEval::from-eval(), 'made-in-eval',
    'our-scoped sub EVALed at BEGIN time lands in the caller package';

# vim: expandtab shiftwidth=4
