our multi sub zipwith(&op, Iterable $a-iterable, Iterable $b-iterable) {
    my $ai = $a-iterable.iterator;
    my $bi = $b-iterable.iterator;
    gather loop {
        my $a = $ai.get;
        my $b = $bi.get;
        last if ($a ~~ EMPTY) || ($b ~~ EMPTY);
        take &op($a, $b);
    }
}

our multi sub crosswith(&op, Iterable $a-iterable, Iterable $b-iterable) {
    my $ai = $a-iterable.iterator;
    my @b = $b-iterable.Seq;
    gather loop {
        my $a = $ai.get;
        last if ($a ~~ EMPTY);
        for @b -> $b {
            take &op($a, $b);
        }
    }
}