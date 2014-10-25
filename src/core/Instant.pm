my class Duration {... }

my class Instant is Cool does Real {
    has Rat $.tai;
      # A linear count of seconds since 1970-01-01T00:00:00Z, plus
      # tai-utc::initial-offset. Thus, $.tai matches TAI from 1970
      # to the present.

    method new($tai) { self.bless: tai => $tai.Rat }

    method from-posix($posix, Bool $prefer-leap-second = False) {
    # $posix is in general not expected to be an integer.
    # If $prefer-leap-second is true, 915148800 is interpreted to
    # mean 1998-12-31T23:59:60Z rather than 1999-01-01T00:00:00Z.
        my $p = $posix.floor;
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
        my $n = $.tai.floor;
        my $offset = tai-utc::initial-offset;
        for tai-utc::leap-second-posix() {
            if $_ < $n - $offset {
                ++$offset;
            } else {
                return ($.tai - $offset, $n - $offset == $_)
            }
        }
        ($.tai - $offset, False)
    }

    multi method Str(Instant:D:) {
        'Instant:' ~ $.tai
    }
    multi method perl(Instant:D:) {
        "Instant.new($.tai.perl())";
    }
    method Bridge(Instant:D:) { $.tai.Bridge }
    method Num   (Instant:D:) { $.tai.Num    }
    method Int   (Instant:D:) { $.tai.Int    }
    method narrow(Instant:D:) { $.tai.narrow }

#    TODO: should be the new .gist, probably
#    method Str() {
#        'Instant:' ~ default-formatter
#            ::DateTime.new(self), :subseconds
#    }
}

multi sub infix:«cmp»(Instant:D $a, Instant:D $b) {
    $a.tai <=> $b.tai }

multi sub infix:«<=>»(Instant:D $a, Instant:D $b) {
    $a.tai <=> $b.tai
}

multi sub infix:«==»(Instant:D $a, Instant:D $b) {
    $a.tai == $b.tai
}

multi sub infix:«!=»(Instant:D $a, Instant:D $b) {
    $a.tai != $b.tai
}

multi sub infix:«<»(Instant:D $a, Instant:D $b) {
    $a.tai < $b.tai
}

multi sub infix:«>»(Instant:D $a, Instant:D $b) {
    $a.tai > $b.tai
}

multi sub infix:«<=»(Instant:D $a, Instant:D $b) {
    $a.tai <= $b.tai
}

multi sub infix:«>=»(Instant:D $a, Instant:D $b) {
    $a.tai >= $b.tai
}

multi sub infix:<+>(Instant:D $a, Real:D $b) {
    Instant.new: $a.tai + $b;
}
multi sub infix:<+>(Real:D $a, Instant:D $b) {
    Instant.new: $a + $b.tai;
}
multi sub infix:<+>(Instant:D $a, Duration:D $b) {
    Instant.new: $a.tai + $b.tai;
}
multi sub infix:<+>(Duration:D $a, Instant:D $b) {
    Instant.new: $a.tai + $b.tai;
}

multi sub infix:<->(Instant:D $a, Instant:D $b) {
    Duration.new: $a.tai - $b.tai;
}
multi sub infix:<->(Instant:D $a, Real:D $b) {
    Instant.new: $a.tai - $b;
}

sub term:<time>() { nqp::p6box_i(nqp::time_i()) }
sub term:<now>() {
    # FIXME: During a leap second, the returned value is one
    # second greater than it should be.
    Instant.from-posix: nqp::time_n
}

#{
    my num $init-time-num = nqp::time_n;  # need find a way to not leak this
    multi sub INITIALIZE_DYNAMIC('$*INITTIME') {
        PROCESS::<$INITTIME> := Instant.from-posix: $init-time-num;
    }
#}

# vim: ft=perl6 expandtab sw=4
