role Clock::POSIX {
    method Int() { floor self.Real }
    method Real() { ... }
    method sleep($seconds) { ... }
}

sub sleep($seconds = Inf) {  # fractional seconds also allowed
    my $time1 = time;
    $*CLOCK.sleep($seconds);
    my $time2 = time;
    return $time2 - $time1;
}

sub term:<time>() {
    $*CLOCK.Int;
}

