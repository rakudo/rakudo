my class DateTime does Dateish {
    has Int $.hour      = 0;
    has Int $.minute    = 0;
    has     $.second    = 0.0;
    has     $.timezone  = 0; # UTC
    has     &.formatter;
      # Not an optimization but a necessity to ensure that
      # $dt.utc.local.utc is equivalent to $dt.utc. Otherwise,
      # DST-induced ambiguity could ruin our day.

    method !formatter() { # ISO 8601 timestamp
        my $o = $!timezone.Int;
        sprintf '%04d-%02d-%02dT%02d:%02d:%s%s',
            $!year, $!month, $!day, $!hour, $!minute,
            $!second.floor == $!second
              ?? $!second.Int.fmt('%02d')
              !! $!second.fmt('%09.6f'),
            $o
             ?? do {
                    warn "DateTime formatter: offset $o not divisible by 60"
                      unless $o %% 60;
                    sprintf '%s%02d:%02d',
                      $o < 0 ?? '-' !! '+',
                      ($o.abs / 60 / 60).floor,
                      ($o.abs / 60 % 60).floor
                   }
             !! 'Z';
    }

    multi method new() {
        fail "Required named parameter 'year' is missing for DateTime.new()";
    }

    multi method new(Int :$year!, *%_) {
        my $dt = self.bless(:$year, |%_);
        $dt.check-date;
        $dt.check-time;
        $dt;
    }

    method check-time {
        # Asserts the validity of and numifies $!hour, $!minute, and $!second.
        self.check-value($!hour, 'hour', 0 ..^ 24);
        self.check-value($!minute, 'minute', 0 ..^ 60);
        self.check-value($!second, 'second', 0 ..^ 62, :allow-nonint);
        if $!second >= 60 {
            # Ensure this is an actual leap second.
            self.second < 61
                or X::OutOfRange.new(
                        what  => 'second',
                        range => (0..^60),
                        got   => self.second,
                        comment => 'No second 61 has yet been defined',
                ).throw;
            my $dt = self.utc;
            $dt.hour == 23 && $dt.minute == 59
                or X::OutOfRange.new(
                        what  => 'second',
                        range => (0..^60),
                        got   => self.second,
                        comment => 'a leap second can occur only at hour 23 and minute 59 UTC',
                ).throw;
            my $date = sprintf '%04d-%02d-%02d',
                $dt.year, $dt.month, $dt.day;
            $date eq any(tai-utc.leap-second-dates)
                or X::OutOfRange.new(
                        what  => 'second',
                        range => (0..^60),
                        got   => self.second,
                        comment => "There is no leap second on UTC $date",
                ).throw;
        }
    }

    multi method new(Date:D :$date!, *%_) {
        self.new(year => $date.year, month => $date.month,
            day => $date.day, |%_)
    }

    multi method new(Instant:D $i, :$timezone=0, :&formatter) {
        my ($p, $leap-second) = $i.to-posix;
        my $dt = self.new: floor($p - $leap-second).Int, :&formatter;
        $dt.clone(second => ($dt.second + $p % 1 + $leap-second)
            ).in-timezone($timezone);
    }

    multi method new(Int:D $time is copy, :$timezone=0, :&formatter) {
        # Interpret $time as a POSIX time.
        my $second  = $time % 60; $time = $time div 60;
        my $minute  = $time % 60; $time = $time div 60;
        my $hour    = $time % 24; $time = $time div 24;
        # Day month and leap year arithmetic, based on Gregorian day #.
        # 2000-01-01 noon UTC == 2451558.0 Julian == 2451545.0 Gregorian
        $time += 2440588;   # because 2000-01-01 == Unix epoch day 10957
        my $a = $time + 32044;     # date algorithm from Claus Tøndering
        my $b = (4 * $a + 3) div 146097; # 146097 = days in 400 years
        my $c = $a - (146097 * $b) div 4;
        my $d = (4 * $c + 3) div 1461;       # 1461 = days in 4 years
        my $e = $c - ($d * 1461) div 4;
        my $m = (5 * $e + 2) div 153; # 153 = days in Mar-Jul Aug-Dec
        my $day   = $e - (153 * $m + 2) div 5 + 1;
        my $month = $m + 3 - 12 * ($m div 10);
        my $year  = $b * 100 + $d - 4800 + $m div 10;
        self.bless(:$year, :$month, :$day,
            :$hour, :$minute, :$second,
            :&formatter).in-timezone($timezone);
    }

    multi method new(Str $format, :$timezone is copy = 0, :&formatter) {
        $format ~~ /^ (\d**4) '-' (\d\d) '-' (\d\d) <[Tt]> (\d\d) ':' (\d\d) ':' (\d\d[<[\.,]>\d ** 1..6]?) (<[Zz]> || (<[\-\+]>) (\d\d) (':'? (\d\d))? )? $/
            or X::Temporal::InvalidFormat.new(
                    invalid-str => $format,
                    target      => 'DateTime',
                    format      => 'an ISO 8601 timestamp (yyyy-mm-ddThh:mm:ssZ or yyyy-mm-ddThh:mm:ss+01:00)',
                ).throw;
        my $year   = (+$0).Int;
        my $month  = (+$1).Int;
        my $day    = (+$2).Int;
        my $hour   = (+$3).Int;
        my $minute = (+$4).Int;
        my $second = +$5;
        if $6 {
            $timezone
                and X::DateTime::TimezoneClash.new.throw;
            if $6.chars == 1 {
                $timezone = 0;
            } else {
                if $6[2] && $6[2][0] > 59 {
                    X::OutOfRange.new(
                        what    => "minutes of timezone",
                        got     => +$6[2][0],
                        range   => 0..59,
                   ).throw;
                }
                $timezone = (($6[1]*60 + ($6[2][0] // 0)) * 60).Int;
                  # RAKUDO: .Int is needed to avoid to avoid the nasty '-0'.
                $6[0] eq '-' and $timezone = -$timezone;
            }
        }
        self.new(:$year, :$month, :$day, :$hour, :$minute,
            :$second, :$timezone, :&formatter);
    }

    method now(:$timezone=$*TZ, :&formatter) returns DateTime:D {
        self.new(now, :$timezone, :&formatter)
    }

    method clone(*%_) {
        my %args = :$!year, :$!month, :$!day, :$!hour, :$!minute,
                   :$!second, :$!timezone, :&!formatter, %_;
        self.new(|%args);
    }

    method clone-without-validating(*%_) { # A premature optimization.
        my %args = :$!year, :$!month, :$!day, :$!hour, :$!minute,
                   :$!second, :$!timezone, :&!formatter, %_;
        self.bless(|%args);
    }

    method Instant() {
        Instant.from-posix: self.posix + $!second % 1, $!second >= 60;
    }

    method posix($ignore-timezone?) {
        $ignore-timezone or self.offset == 0
            or return self.utc.posix;
        # algorithm from Claus Tøndering
        my $a = (14 - $!month.Int) div 12;
        my $y = $!year.Int + 4800 - $a;
        my $m = $!month.Int + 12 * $a - 3;
        my $jd = $!day + (153 * $m + 2) div 5 + 365 * $y
            + $y div 4 - $y div 100 + $y div 400 - 32045;
        ($jd - 2440588) * 24 * 60 * 60
            + 60*(60*$!hour + $!minute) + self.whole-second;
    }

    method offset {
        $!timezone.Int;
    }

    method offset-in-minutes {
        $!timezone.Int / 60;
    }

    method offset-in-hours {
        $!timezone.Int / 60 / 60;
    }

    method later(*%unit) {
        die "More than one time unit supplied"
            if %unit.keys > 1;

        die "No time unit supplied"
            unless %unit.keys;

        my ($unit, $amount) = %unit.kv;
        self!VALID-UNIT($unit);

        my $hour   = $!hour;
        my $minute = $!minute;
        my $date;

        given $unit {
            when 'second' | 'seconds' {
                return self.new(self.Instant + $amount, :$.timezone, :&.formatter);
            }

            when 'minute' | 'minutes' { $minute += $amount; proceed }

            $hour += floor($minute / 60);
            $minute %= 60;

            when 'hour' | 'hours' { $hour += $amount; proceed }

            my $day-delta += floor($hour / 24);
            $hour %= 24;

            when 'day' | 'days' { $day-delta += $amount; proceed }
            when 'week' | 'weeks' { $day-delta += 7 * $amount; proceed }

            when 'month' | 'months' {
                my $month = $!month;
                my $year  = $!year;
                $month += $amount;
                $year += floor(($month - 1) / 12);
                $month = ($month - 1) % 12 + 1;
                # If we overflow on days in the month, rather than throw an
                # exception, we just clip to the last of the month
                my $day = $!day min $.days-in-month($year, $month);
                $date = Date.new(:$year, :$month, :$day);
                succeed;
            }

            when 'year' | 'years' {
                my $year = $!year + $amount;
                $date = Date.new(:$year, :$!month, :$!day);
                succeed;
            }

            my $daycount = Date.new(self).daycount;
            $daycount += $day-delta;
            $date = Date.new-from-daycount($daycount);
        }

        my $second = $!second;
        if $second > 59 && $date ne any(tai-utc.leap-second-dates) {
            $second -= 60;
            $minute++;
            if $minute > 59 {
                $minute -= 60;
                $hour++;
                if $hour > 23 {
                    $hour -= 24;
                    $date++;
                }
            }
        }
        self.new(:$date, :$hour, :$minute, :$second, :$.timezone, :&.formatter);
    }

    method earlier(*%unit) {
        self.later(| -<< %unit);
    }

    method truncated-to(Cool $unit) {
        my %parts;
        given $unit {
            %parts<second> = self.whole-second;
            when 'second'   {}
            %parts<second> = 0;
            when 'minute'   {}
            %parts<minute> = 0;
            when 'hour'     {}
            %parts<hour> = 0;
            when 'day'      {}
            # Fall through to Dateish.
            %parts = self.truncate-parts($unit, %parts);
        }
        self.clone-without-validating(|%parts);
    }

    method whole-second() {
        $!second.Int
    }

    method in-timezone($timezone) {
        $timezone eqv $!timezone and return self;
        my $old-offset = self.offset;
        my $new-offset = $timezone.Int;
        my %parts;
        # Is the logic for handling leap seconds right?
        # I don't know, but it passes the tests!
        my $a = ($!second >= 60 ?? 59 !! $!second)
            + $new-offset - $old-offset;
        %parts<second> = $!second >= 60 ?? $!second !! $a % 60;
        my Int $b = $!minute + floor($a) div 60;
        %parts<minute> = $b % 60;
        my Int $c = $!hour + $b div 60;
        %parts<hour> = $c % 24;

        # Let Dateish handle any further rollover.
        if ($c div 24) {
            %parts<year month day> =
                self!ymd-from-daycount(self.get-daycount + $c div 24);
        }
        self.clone-without-validating:
            :$timezone, |%parts;
    }

    method utc() {
        self.in-timezone(0)
    }
    method local() {
        self.in-timezone($*TZ)
    }

    method Date() {
        Date.new(:$!year, :$!month, :$!day);
    }

    multi method Str(DateTime:D:) {
        &!formatter ?? &!formatter(self) !! self!formatter
    }
    multi method gist(DateTime:D:) {
        self.Str;
    }

    multi method perl(DateTime:D:) {
        self.^name
          ~ '.new('
          ~ (:$!year.perl,
             :$!month.perl,
             :$!day.perl,
             :$!hour.perl,
             :$!minute.perl,
             :$!second.perl,
             (:$!timezone.perl unless $!timezone === 0),
             (:&!formatter.perl if &!formatter),
            ).join(', ')
         ~ ')'
    }
}

multi sub infix:«<»(DateTime:D \a, DateTime:D \b) {
    a.Instant < b.Instant
}
multi sub infix:«>»(DateTime:D \a, DateTime:D \b) {
    a.Instant > b.Instant
}
multi sub infix:«<=»(DateTime:D \a, DateTime:D \b) {
    a.Instant <= b.Instant
}
multi sub infix:«>=»(DateTime:D \a, DateTime:D \b) {
    a.Instant >= b.Instant
}
multi sub infix:«==»(DateTime:D \a, DateTime:D \b) {
    a.Instant == b.Instant
}
multi sub infix:«!=»(DateTime:D \a, DateTime:D \b) {
    a.Instant != b.Instant
}
multi sub infix:«<=>»(DateTime:D \a, DateTime:D \b) {
    a.Instant <=> b.Instant
}
multi sub infix:«cmp»(DateTime:D \a, DateTime:D \b) {
    a.Instant cmp b.Instant
}

# vim: ft=perl6 expandtab sw=4
