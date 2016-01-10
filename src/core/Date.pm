my class Date does Dateish {

    method BUILD($!year,$!month,$!day,$!daycount = Int) { self }

    multi method new(Date: Int() $year, Int() $month, Int() $day) {
        (1..12).in-range($month,'Month');
        (1 .. self!DAYS-IN-MONTH($year,$month)).in-range($day,'Day');
        nqp::create(self).BUILD($year,$month,$day)
    }
    multi method new(Date: :$year!, :$month = 1, :$day = 1) {
        self.new($year,$month,$day)
    }
    multi method new(Date: Str $date) {
        X::Temporal::InvalidFormat.new(
          invalid-str => $date,
          target      => 'Date',
          format      => 'yyyy-mm-dd',
        ).throw unless $date ~~ /^
          (<[+-]>? \d**4 \d*)                            # year
          '-'
          (\d\d)                                         # month
          '-'
          (\d\d)                                         # day
        $/;
        self.new(+$0,+$1,+$2)
    }
    multi method new(Date: Dateish $d) {
        nqp::create(self).BUILD($d.year,$d.month,$d.day)
    }
    multi method new(Date: Instant $i) {
        self.new(DateTime.new($i))
    }

    method new-from-daycount($daycount) {
        my ($year, $month, $day) = self!ymd-from-daycount($daycount);
        nqp::create(self).BUILD($year,$month,$day,$daycount);
    }

    method today() {
        self.new(DateTime.now);
    }

    multi method WHICH(Date:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::unbox_i(self.daycount)
            ),
            ObjAt
        );
    }

    method truncated-to(Cool $unit) {
        self!VALID-UNIT($unit);
        self!clone-without-validating(|self!truncate-ymd($unit));
    }

    method later(*%unit) {
        die "More than one time unit supplied"
            if %unit.keys > 1;

        die "No time unit supplied"
            unless %unit.keys;

        my ($unit, $amount) = %unit.kv;
        self!VALID-UNIT($unit);

        my $date;

        given $unit {
            X::DateTime::InvalidDeltaUnit.new(:$unit).throw
                when 'second' | 'seconds' | 'minute' | 'minutes' | 'hour' | 'hours';

            my $day-delta;
            when 'day' | 'days' { $day-delta = $amount; proceed }
            when 'week' | 'weeks' { $day-delta = 7 * $amount; proceed }

            when 'month' | 'months' {
                my int $month = $!month;
                my int $year  = $!year;
                $month += $amount;
                $year += floor(($month - 1) / 12);
                $month = ($month - 1) % 12 + 1;
                # If we overflow on days in the month, rather than throw an
                # exception, we just clip to the last of the month
                $date = Date.new($year,$month,$!day > 28
                  ?? $!day min self!DAYS-IN-MONTH($year,$month)
                  !! $!day);
                succeed;
            }

            when 'year' | 'years' {
                my int $year = $!year + $amount;
                $date = Date.new($year,$!month,$!day > 28
                  ?? $!day min self!DAYS-IN-MONTH($year,$!month)
                  !! $!day);
                succeed;
            }

            $date = self.new-from-daycount(self.daycount + $day-delta);
        }

        $date;
    }

    method earlier(*%unit) {
        self.later(| -<< %unit);
    }

    method clone(*%_) {
        my %args = :$!year, :$!month, :$!day, %_;
        self.new(|%args);
    }
    method !clone-without-validating(*%_) { # A premature optimization.
        my $h := nqp::getattr(%_,Map,'$!storage');
        nqp::create(self).BUILD(
          nqp::existskey($h,'year')  ?? nqp::atkey($h,'year')   !! $!year,
          nqp::existskey($h,'month') ?? nqp::atkey($h,'month')  !! $!month,
          nqp::existskey($h,'day')   ?? nqp::atkey($h,'day')    !! $!day,
        )
    }

    method succ(Date:D:) {
        self.new-from-daycount(self.daycount + 1);
    }
    method pred(Date:D:) {
        self.new-from-daycount(self.daycount - 1);
    }

    multi method Str(Date:D:) { self.yyyy-mm-dd }
    multi method perl(Date:D:) {
        self.^name ~ ".new($!year,$!month,$!day)"
    }
    multi method ACCEPTS(Date:D: DateTime:D $dt) {
        $dt.day == $!day && $dt.month == $!month && $dt.year == $!year
    }
}

multi sub infix:<+>(Date:D $d, Int:D $x) {
    Date.new-from-daycount($d.daycount + $x)
}
multi sub infix:<+>(Int:D $x, Date:D $d) {
    Date.new-from-daycount($d.daycount + $x)
}
multi sub infix:<->(Date:D $d, Int:D $x) {
    Date.new-from-daycount($d.daycount - $x)
}
multi sub infix:<->(Date:D $a, Date:D $b) {
    $a.daycount - $b.daycount;
}
multi sub infix:<cmp>(Date:D $a, Date:D $b) {
    $a.daycount cmp $b.daycount
}
multi sub infix:«<=>»(Date:D $a, Date:D $b) {
    $a.daycount <=> $b.daycount
}
multi sub infix:<==>(Date:D $a, Date:D $b) {
    $a.daycount == $b.daycount
}
multi sub infix:«<=»(Date:D $a, Date:D $b) {
    $a.daycount <= $b.daycount
}
multi sub infix:«<»(Date:D $a, Date:D $b) {
    $a.daycount < $b.daycount
}
multi sub infix:«>=»(Date:D $a, Date:D $b) {
    $a.daycount >= $b.daycount
}
multi sub infix:«>»(Date:D $a, Date:D $b) {
    $a.daycount > $b.daycount
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*TZ', {
    PROCESS::<$TZ> = Rakudo::Internals.get-local-timezone-offset();
}

sub sleep($seconds = Inf --> Nil) {
    if $seconds == Inf || nqp::istype($seconds,Whatever) {
        nqp::sleep(1e16) while True;
    }
    elsif $seconds > 0 {
        nqp::sleep($seconds.Num);
    }
}

sub sleep-timer(Real() $seconds = Inf --> Duration) {
    if $seconds <= 0 {
        Duration.new(0);
    }
    else {
        my $time1 = now;
        nqp::sleep($seconds.Num);
        Duration.new( ( $seconds - (now - $time1) ) max 0 );
    }
}

sub sleep-until(Instant() $until --> Bool) {
    my $seconds = $until - now;
    return False if $seconds < 0;

    Nil while $seconds = sleep-timer($seconds);
    True;
}

# vim: ft=perl6 expandtab sw=4
