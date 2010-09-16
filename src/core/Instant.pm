use v6;

class Instant does Real {
    has Rat $.x;
      # A linear count of seconds since 1970-01-01T00:00:00Z, plus
      # tai-utc::initial-offset. Thus, $.x matches TAI from 1970
      # to the present.

    method new($x) { self.bless: *, x => $x.Rat }

    method from-posix($posix, Bool $prefer-leap-second = False) {
    # $posix is in general not expected to be an integer.
    # If $prefer-leap-second is true, 915148800 is interpreted to
    # mean 1998-12-31T23:59:60Z rather than 1999-01-01T00:00:00Z.
        my $p = floor $posix;
        my $offset = tai-utc::initial-offset;
        for tai-utc::leap-second-posix() {
            if $_ < $p {
                ++$offset;
            } else {
                return self.new: $posix + $offset + do
                    $_ == $p && !$prefer-leap-second 
            }
        }
        self.new: $posix + $offset;
    }

    method to-posix() {
    # The inverse of .from-posix, except that the second return
    # value is true if *and only if* this Instant is in a leap
    # second.
        my $n = floor $.x;
        my $offset = tai-utc::initial-offset;
        for tai-utc::leap-second-posix() {
            if $_ < $n - $offset {
                ++$offset;
            } else {
                return ($.x - $offset, $n - $offset == $_)
            }
        }
        ($.x - $offset, False)
    }

    method Str() {
        'Instant:' ~ default-formatter
            ::DateTime.new(self), :subseconds
    }

    method perl() {
        sprintf '(DateTime.new(year => 1970).Instant + %s)',
            ($.x - tai-utc::initial-offset).perl
    }
}

our multi sub infix:«cmp»(Instant $a, Instant $b) {
    $a.x <=> $b.x
}

our multi sub infix:«<=>»(Instant $a, Instant $b) {
    $a.x <=> $b.x
}

our multi sub infix:«==»(Instant $a, Instant $b) {
    $a.x == $b.x
}

our multi sub infix:«!=»(Instant $a, Instant $b) {
    $a.x != $b.x
}

our multi sub infix:«<»(Instant $a, Instant $b) {
    $a.x < $b.x
}

our multi sub infix:«>»(Instant $a, Instant $b) {
    $a.x > $b.x
}

our multi sub infix:«<=»(Instant $a, Instant $b) {
    $a.x <= $b.x
}

our multi sub infix:«>=»(Instant $a, Instant $b) {
    $a.x >= $b.x
}

our multi sub infix:<+>(Instant $a, Real $b) {
    Instant.new: $a.x + $b;
}
our multi sub infix:<+>(Real $a, Instant $b) {
    Instant.new: $a + $b.x;
}
our multi sub infix:<+>(Instant $a, Duration $b) {
    Instant.new: $a.x + $b.x;
}
our multi sub infix:<+>(Duration $a, Instant $b) {
    Instant.new: $a.x + $b.x;
}

our multi sub infix:<->(Instant $a, Instant $b) {
    Duration.new: $a.x - $b.x;
}
our multi sub infix:<->(Instant $a, Real $b) {
    Instant.new: $a.x - $b;
}

our sub term:<now>() {
    # FIXME: During a leap second, the returned value is one
    # second greater than it should be.
    Instant.from-posix: pir::time__n
}
