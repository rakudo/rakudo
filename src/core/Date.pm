my class Date does Dateish {

    multi method new(:$year!, :$month = 1, :$day = 1) {
        my $d = self.bless(:$year, :$month, :$day);
        $d.check-date;
        $d;
    }

    multi method new($year, $month, $day) {
        self.new(:$year, :$month, :$day);
    }

    multi method new(Str $date) {
        $date ~~ /^ \d\d\d\d '-' \d\d '-' \d\d $/
            or X::Temporal::InvalidFormat.new(
                    invalid-str => $date,
                    format      => 'yyyy-mm-dd',
            ).throw;
        self.new(|$date.split('-').map({.Int}));
    }
    multi method new() {
        fail X::Cannot::New.new(class => self);
    }

    multi method new(Dateish $d) {
        self.bless(:year($d.year), :month($d.month), :day($d.day));
    }

    multi method new(Instant $i) {
        my $dt = DateTime.new($i);
        self.new($dt);
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

    method new-from-daycount($daycount) {
        my ($year, $month, $day) = self!ymd-from-daycount($daycount);
        self.bless(:$daycount, :$year, :$month, :$day);
    }

    method today() {
        self.new(DateTime.now);
    }

    method truncated-to(Cool $unit) {
        self.clone(|self.truncate-parts($unit));
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
                my $month = $!month;
                my $year  = $!year;
                $month += $amount;
                $year += floor(($month - 1) / 12);
                $month = ($month - 1) % 12 + 1;
                # If we overflow on days in the month, rather than throw an
                # exception, we just clip to the last of the month
                my $day = $!day min $.days-in-month($year, $month);
                $date = self.new(:$year, :$month, :$day);
                succeed;
            }

            when 'year' | 'years' {
                my $year = $!year + $amount;
                $date = self.new(:$year, :$!month, :$!day);
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

    method succ() {
        self.new-from-daycount(self.daycount + 1);
    }
    method pred() {
        self.new-from-daycount(self.daycount - 1);
    }

    multi method gist(Date:D:) {
        self.Str
    }

    multi method Str(Date:D:) {
        my str $format = (0 <= $!year <= 9999 ?? '%04d-%02d-%02d' !! '%+05d-%02d-%02d');
        sprintf $format, $!year, $!month, $!day;
    }

    multi method perl(Date:D:) {
        self.^name ~ ".new($!year.perl(), $!month.perl(), $!day.perl())";
    }

    multi method ACCEPTS(Date:D: DateTime:D $dt) {
        $dt.year == $!year && $dt.month == $!month && $dt.day == $!day;
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

multi sub INITIALIZE_DYNAMIC('$*TZ') {
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
