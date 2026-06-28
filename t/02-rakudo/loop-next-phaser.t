use Test;

plan 8;

# A NEXT phaser runs after each iteration of a value-producing loop, the same
# as it does for a sunk loop. These cover the from-loop code paths a loop in
# value context takes, which used to drop the phaser.

# C-style loop with an increment.
{
    my $n = 0;
    my @r = (loop (my $i = 0; $i < 3; $i++) { NEXT { $n++ }; $i });
    is $n, 3, 'NEXT runs each iteration of a value-producing C-style loop';
    is @r.join(','), '0,1,2', 'the C-style loop still yields its values';
}

# An explicit `next` still triggers the NEXT phaser for that iteration.
{
    my $n = 0;
    my @r = (loop (my $i = 0; $i < 5; $i++) { next if $i == 2; NEXT { $n++ }; $i });
    is $n, 5, 'NEXT runs on an explicit next as well';
    is @r.join(','), '0,1,3,4', 'the skipped value is omitted';
}

# C-style loop with an empty increment slot uses an afterwards-only thunk.
{
    my $n = 0;
    my @r = (loop (my $i = 0; $i < 3; ) { NEXT { $n++ }; my $v = $i; $i++; $v });
    is $n, 3, 'NEXT runs when the C-style increment slot is empty';
}

# A plain while loop in value context has no increment to fold into.
{
    my $n = 0;
    my $i = 0;
    my @r = (while $i < 3 { NEXT { $n++ }; my $v = $i; $i++; $v });
    is $n, 3, 'NEXT runs each iteration of a value-producing while loop';
}

# A repeat loop checks its condition after the body, so the phaser folds there.
{
    my $n = 0;
    my $i = 0;
    my @r = (repeat { NEXT { $n++ }; my $v = $i; $i++; $v } while $i < 3);
    is $n, 3, 'NEXT runs each iteration of a value-producing repeat loop';
}

# A bare infinite loop has neither condition nor increment.
{
    my $n = 0;
    my $i = 0;
    my @r = (loop { last if $i >= 3; NEXT { $n++ }; my $v = $i; $i++; $v });
    is $n, 3, 'NEXT runs each iteration of a value-producing bare loop';
}

# vim: expandtab shiftwidth=4
