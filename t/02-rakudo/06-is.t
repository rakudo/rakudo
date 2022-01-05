use v6;
use Test;

plan 6;

sub check-fail (&test-to-run) {
    my $message = 'should fail due to requested comparison';
    todo $message;
    nok test-to-run(), $message;
}

check-fail { is Mu, Mu.new }
check-fail { is Mu.new, Mu }
is Mu,     Mu,     'is(Mu:U, Mu:U) passes';
is Mu.new, Mu.new, 'is(Mu:D, Mu:D) passes';

# vim: expandtab shiftwidth=4
