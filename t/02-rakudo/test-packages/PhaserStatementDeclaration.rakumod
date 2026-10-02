unit module PhaserStatementDeclaration;

our sub first-scalar() {
    my @seen;
    for 1..3 { FIRST my $a = 5; @seen.push: $a }
    @seen
}

our sub first-native() {
    my @seen;
    for 1..3 { FIRST my int $n = 5; @seen.push: $n }
    @seen
}

our sub first-sub() {
    my @seen;
    for 1..2 { FIRST sub foo() { 42 }; @seen.push: foo() }
    @seen
}

our sub first-state() {
    my @seen;
    for 1..3 { FIRST state $s = 5; $s++; @seen.push: $s }
    @seen
}

our sub post-closure() {
    my $seen;
    sub f() { POST my $a = $_; $seen = { $a }; 7 }
    f();
    $seen()
}
