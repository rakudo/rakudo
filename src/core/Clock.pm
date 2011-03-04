role Clock::POSIX {
    # A class that implements this role must implement at least
    # one of these two methods
    method Int() { floor self.Real }
    method Real() { self.Int }

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

