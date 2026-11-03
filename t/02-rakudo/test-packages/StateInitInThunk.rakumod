unit module StateInitInThunk;

our sub enter-counts() {
    my @r;
    for 1..3 { ENTER @r.push: (state $n = 10)++ }
    @r
}

our sub check-value() {
    my $v = CHECK (state $c = 6)++;
    ($v, $c)
}
