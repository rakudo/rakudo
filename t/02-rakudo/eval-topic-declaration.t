use lib <t/packages/Test-Helpers>;
use nqp;
use Test;
use Test::Helpers;

plan 16;

# A scope whose topic is bound from the enclosing scope, such as an EVAL
# unit or a for loop body with a signature, gives that topic up to a
# my $_ of its own. Reading and writing $_ without a declaration of it
# still reaches the enclosing topic.

my $rakuast = nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

$_ = 1;
is EVAL('$_'), 1,
  'an EVAL reads the topic of the code around it';
EVAL '$_ = 2';
is $_, 2,
  'an assignment to $_ inside an EVAL writes the topic of the code around it';

$_ = 1;
is EVAL('my $_ = 6; $_'), 6,
  'a my $_ inside an EVAL holds what is assigned to it';
is $_, 1,
  'a my $_ assigned inside an EVAL leaves the topic of the code around it alone';

$_ = 1;
is EVAL('my $_; $_.raku'), 'Any',
  'a my $_ inside an EVAL without an initializer starts out empty';

$_ = 1;
for 5 -> $x { my $_ = 7 }
is $_, 1,
  'a my $_ in a for loop body with a signature leaves the topic around the loop alone';

$_ = 1;
my &with-placeholder = { $^a; my $_ = 7 };
with-placeholder(0);
is $_, 1,
  'a my $_ in a block with a placeholder parameter leaves the topic around the block alone';

is-run 'EVAL q[my $_ = 1; my $_ = 3]', :err(/'Redeclaration of symbol' .* '$_'/),
  'a second my $_ inside an EVAL is reported as a redeclaration';

# The legacy frontend keeps the caller's topic in an EVAL for the other
# declarators and cannot give it up silently.
if $rakuast {
    $_ = 1;
    is EVAL('state $_ = 6; $_'), 6,
      'a state $_ inside an EVAL holds what is assigned to it';
    is $_, 1,
      'a state $_ assigned inside an EVAL leaves the topic of the code around it alone';

    $_ = 1;
    is EVAL('my ($_, $x) = 5, 6; $_'), 5,
      'a $_ declared in a list inside an EVAL holds what is assigned to it';
    is $_, 1,
      'a $_ declared in a list inside an EVAL leaves the topic of the code around it alone';

    $_ = 1;
    is EVAL('my $_ = 5; EVAL q{$_}'), 5,
      'an EVAL nested in an EVAL reads the my $_ of the EVAL around it';
    is $_, 1,
      'a my $_ read by a nested EVAL leaves the topic outside both alone';

    # A redeclaration worry is reported at compile time, so its absence
    # can only be seen from outside the process.
    is-run '$_ = 1; EVAL q[my $_ = 6]; print $_', :out<1>, :err(''),
      'a my $_ inside an EVAL is not reported as a redeclaration';

    throws-like { EVAL '$_ = 3; my $_ = 6' }, X::Redeclaration::Outer,
      'a use of $_ above a my $_ inside an EVAL is an error as for any lexical';
}
else {
    skip 'the legacy frontend keeps the topic of the code around an EVAL', 8;
}

# vim: expandtab shiftwidth=4
