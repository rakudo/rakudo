use Test;
no worries;

plan 3;

# A my declaration of $/, $! or $_ in a scope that already declares that
# name for itself names the scope's own lexical. Assigning to it must
# not reach the enclosing scope's one, with the optimizer on as well as
# off, even when nothing else in the scope uses the lexical.

"abc" ~~ /b/;
sub with-match { my $/ = 5 }
with-match();
is ~$/, 'b',
  'a my $/ assigned in a sub leaves the match variable of the caller alone';

try die "outer";
sub with-error { my $! = 5 }
with-error();
is $!.?message, 'outer',
  'a my $! assigned in a sub leaves the error variable of the caller alone';

$_ = 1;
sub with-topic { my $_ = 7 }
with-topic();
is $_, 1,
  'a my $_ assigned in a sub leaves the topic of the caller alone';

# vim: expandtab shiftwidth=4
