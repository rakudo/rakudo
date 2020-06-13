use v6;

# Checking that testing is sane: subroutines
say '1..4';

sub ok($num) {
    say "ok $num";
}

ok(1);
ok 2;

my $counter = 2;
sub ok_auto {
    ++$counter;
    say "ok $counter";
}

ok_auto();
ok_auto;

# vim: expandtab shiftwidth=4
