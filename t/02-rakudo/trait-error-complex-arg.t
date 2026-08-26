use Test;

# An exception thrown by a trait handler must surface as a compile-time
# error even when the trait argument cannot be interpreted at BEGIN time,
# such as a declaration like `my $` or a call like `rand`. Such arguments
# route the trait call through a compiled thunk, and the trait must still
# both run and report its errors from there.

plan 4;

use MONKEY-SEE-NO-EVAL;

throws-like q:to/CODE/,
        multi sub trait_mod:<is>(Variable:D \v, :$foo! is raw) { die "oops" }
        my @a is foo(my $);
        CODE
    Exception, message => /oops/,
    'a dying variable trait with a declaration argument reports the error';

throws-like q:to/CODE/,
        multi sub trait_mod:<is>(Variable:D \v, :$foo! is raw) { die "oops" }
        my @a is foo(rand);
        CODE
    Exception, message => /oops/,
    'a dying variable trait with a call argument reports the error';

is EVAL(q:to/CODE/), 'ran',
        my $log;
        multi sub trait_mod:<is>(Variable:D \v, :$foo! is raw) { $log = "ran" }
        my @a is foo(my $);
        $log
        CODE
    'a variable trait with a declaration argument still runs';

is EVAL(q:to/CODE/), 'Num',
        my $log;
        multi sub trait_mod:<is>(Variable:D \v, :$foo! is raw) { $log = $foo.^name }
        my @a is foo(rand);
        $log
        CODE
    'a variable trait with a call argument receives the evaluated value';

# vim: expandtab shiftwidth=4
