role POSIX-Clock {
    method time_as_int() { floor self.time_as_real }
    method time_as_real() { self.time_as_int }
    method sleep($seconds) { ... }
}

sub sleep($seconds = Inf) {  # fractional seconds also allowed
    my $time1 = time;
    $*CLOCK.sleep($seconds);
    my $time2 = time;
    return $time2 - $time1;
}

sub term:<time>() {
    $*CLOCK.time_as_int;
}

