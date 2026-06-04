use Test;

plan 4;

# `$x .= &f` is the assign-meta form of `.&f`: assign the result of
# `f($x)` back to `$x`. The right-hand call goes through the variable
# form of method dispatch and must arrange the invocant as the first
# argument so the wrapping `dispatch:<.=>` can mutate `$x`.

sub double($n) { $n * 2 }

my $n = 5;
$n .= &double;
is $n, 10, '$n .= &double mutates $n with the result of double($n)';

# Blob result distinguishes the bug from generic failures, because a
# wrong-shape call put the Blob in a slot that triggered .Str.
sub head1(Blob $b) { $b.subbuf(0, 1) }

my $blob = blob8.new(0x10, 0x20, 0x30, 0x40);
$blob .= &head1;
is-deeply $blob, blob8.new(0x10),
    '$blob .= &head1 mutates $blob to the trimmed Blob result';

# Standalone `.&f` still calls the routine with the invocant.
is blob8.new(1, 2, 3).&head1, blob8.new(1),
    '$x.&f standalone calls f($x)';

# Chained `.= &f` works the same with successive routines.
my $m = 3;
$m .= &double;
$m .= &double;
is $m, 12, 'chained $x .= &f compositions apply each routine in order';

# vim: expandtab shiftwidth=4
