my class DateTime { ... }
my class Date     { ... }

my role Dateish {
    has Int $.year;
    has Int $.month = 1;
    has Int $.day = 1;

    method IO(|c) { IO::Path.new(self) }

    method !VALID-UNIT($unit) {
        state %UNITS =  # core setting doesn't build if it is a my at role level
          (<second minute hour day week month year> X~ "","s").map: {$_ => 1};
        X::DateTime::InvalidDeltaUnit.new(:$unit).throw
          unless %UNITS.EXISTS-KEY($unit);
    }

    sub IS-LEAP-YEAR($y) { $y %% 4 and not $y %% 100 or $y %% 400 }
    proto method is-leap-year(|) { * }
    multi method is-leap-year(Dateish:D:) { IS-LEAP-YEAR($!year) }
    multi method is-leap-year(Dateish: $y) { IS-LEAP-YEAR($y) }

    sub DAYS-IN-MONTH($year, $month) {
        state @l = 0, 31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31;
        $month == 2 ?? 28 + IS-LEAP-YEAR($year) !! @l.AT-POS($month);
    }
    proto method days-in-month(|) { * }
    multi method days-in-month(Dateish:D:) { DAYS-IN-MONTH($!year,$!month) }
    multi method days-in-month(Dateish: $y, $m) { DAYS-IN-MONTH($y,$m) }

    method daycount-from-ymd(Int() $y is copy, Int() $m is copy, $d) {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        if $m < 3 {
            $m += 12;
            --$y;
        }
        -678973 + $d + (153 * $m - 2) div 5
            + 365 * $y + $y div 4
            - $y div 100  + $y div 400;
    }

    method ymd-from-daycount($daycount) {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        my Int $dc = $daycount.Int + 678881;
        my Int $ti = (4 * ($dc + 36525)) div 146097 - 1;
        my Int $year = 100 * $ti;
        my int $day = $dc - (36524 * $ti + ($ti div 4));
        my int $t = (4 * ($day + 366)) div 1461 - 1;
        $year = $year + $t;
        $day = $day - (365 * $t + ($t div 4));
        my int $month = (5 * $day + 2) div 153;
        $day = $day - ((2 + $month * 153) div 5 - 1);
        if ($month > 9) {
            $month = $month - 12;
            $year = $year + 1;
        }
        ($year, $month + 3, $day)
    }

    method get-daycount {
        self.daycount-from-ymd($!year, $!month, $!day)
    }

    method day-of-month() { $!day }

    method day-of-week($daycount = self.get-daycount) {
        ($daycount + 2) % 7 + 1
    }

    method week() { # algorithm from Claus Tøndering
        my $a = $!year - ($!month <= 2).floor.Int;
        my $b = $a div 4 - $a div 100 + $a div 400;
        my $c = ($a - 1) div 4 - ($a - 1) div 100 + ($a - 1) div 400;
        my $s = $b - $c;
        my $e = $!month <= 2 ?? 0 !! $s + 1;
        my $f = $!day + do $!month <= 2
         ?? 31*($!month - 1) - 1
         !! (153*($!month - 3) + 2) div 5 + 58 + $s;

        my $g = ($a + $b) % 7;
        my $d = ($f + $g - $e) % 7;
        my $n = $f + 3 - $d;

           $n < 0           ?? ($!year - 1, 53 - ($g - $s) div 5)
        !! $n > 364 + $s    ?? ($!year + 1, 1)
        !!                     ($!year,     $n div 7 + 1);
    }

    method week-year() {
        self.week.[0]
    }

    method week-number() {
        self.week.[1]
    }

    method weekday-of-month {
        ($!day - 1) div 7 + 1
    }

    method day-of-year() {
        [+] flat $!day, map { self.days-in-month($!year, $^m) }, 1 ..^ $!month
    }

    method check-value($val is copy, $name, $range, :$allow-nonint) {
        $val = $allow-nonint ?? +$val !! $val.Int;
        $val ~~ $range
            or X::OutOfRange.new(
                        what    => $name,
                        got     => $val,
                        range   => $range,
               ).throw;
    }

    method check-date {
        self.check-value($!month, 'month', 1 .. 12);
        self.check-value($!day, "day of $!year/$!month",
            1 .. self.days-in-month);
    }

    method truncate-parts(Cool:D $unit, %parts? is copy) {
        # Helper for DateTime.truncated-to and Date.truncated-to.
        self!VALID-UNIT($unit);
        if $unit eq 'week' | 'weeks' {
            my $dc = self.get-daycount;
            my $new-dc = $dc - self.day-of-week($dc) + 1;
            %parts<year month day> =
                self.ymd-from-daycount($new-dc);
        } else { # $unit eq 'month' | 'months' | 'year' | 'years'
            %parts<day> = 1;
            $unit eq 'year' and %parts<month> = 1;
        }
        %parts;
    }

}

sub default-formatter(DateTime $dt, Bool :$subseconds) {
# ISO 8601 timestamp (well, not strictly ISO 8601 if $subseconds
# is true)
    my $o = $dt.offset;
    $o %% 60
        or warn "Default DateTime formatter: offset $o not divisible by 60.\n";
    my $year = sprintf((0 <= $dt.year <= 9999 ?? '%04d' !! '%+05d'), $dt.year);
    sprintf '%s-%02d-%02dT%02d:%02d:%s%s',
        $year, $dt.month, $dt.day, $dt.hour, $dt.minute,
        $subseconds
          ?? $dt.second.fmt('%09.6f')
          !! $dt.whole-second.fmt('%02d'),
        do $o
         ?? sprintf '%s%02d:%02d',
                $o < 0 ?? '-' !! '+',
                ($o.abs / 60 / 60).floor,
                ($o.abs / 60 % 60).floor
         !! 'Z';
}

sub get-local-timezone-offset {
  my $utc = DateTime.new(now).posix.Int;
  my Mu $fia := nqp::p6decodelocaltime(nqp::unbox_i($utc));
  my $second = nqp::p6box_i(nqp::atpos_i($fia, 0));
  my $minute = nqp::p6box_i(nqp::atpos_i($fia, 1));
  my $hour   = nqp::p6box_i(nqp::atpos_i($fia, 2));
  my $day    = nqp::p6box_i(nqp::atpos_i($fia, 3));
  my $month  = nqp::p6box_i(nqp::atpos_i($fia, 4));
  my $year   = nqp::p6box_i(nqp::atpos_i($fia, 5));
  my $local  = DateTime.new(:$year, :$month, :$day, :$hour, :$minute, :$second);
  my $ltime  = $local.posix(True).Int;
  $ltime - $utc;
}

my class DateTime does Dateish {
    has Int $.hour      = 0;
    has Int $.minute    = 0;
    has     $.second    = 0.0;
    has     $.timezone  = 0; # UTC
    has     &.formatter = &default-formatter;
      # Not an optimization but a necessity to ensure that
      # $dt.utc.local.utc is equivalent to $dt.utc. Otherwise,
      # DST-induced ambiguity could ruin our day.

    multi method new() {
        fail "Required named parameter 'year' is missing for DateTime.new()";
    }

    multi method new(Int :$year!, :&formatter=&default-formatter, *%_) {
        my $dt = self.bless(:$year, :&formatter, |%_);
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
            $date eq any(tai-utc::leap-second-dates)
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

    multi method new(Instant:D $i, :$timezone=0, :&formatter=&default-formatter) {
        my ($p, $leap-second) = $i.to-posix;
        my $dt = self.new: floor($p - $leap-second).Int, :&formatter;
        $dt.clone(second => ($dt.second + $p % 1 + $leap-second)
            ).in-timezone($timezone);
    }

    multi method new(Int:D $time is copy, :$timezone=0, :&formatter=&default-formatter) {
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

    multi method new(Str $format, :$timezone is copy = 0, :&formatter=&default-formatter) {
        $format ~~ /^ (\d**4) '-' (\d\d) '-' (\d\d) <[Tt]> (\d\d) ':' (\d\d) ':' (\d\d) (<[Zz]> || (<[\-\+]>) (\d\d) (':'? (\d\d))? )? $/
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

    method now(:$timezone=$*TZ, :&formatter=&default-formatter) returns DateTime:D {
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

        my ($hour, $minute) = $!hour, $!minute;
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
                my ($month, $year) = $!month, $!year;
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
        if $second > 59 && $date ne any(tai-utc::leap-second-dates) {
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
                self.ymd-from-daycount(self.get-daycount + $c div 24);
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

    method Str() {
        &!formatter(self)
    }

    multi method perl(DateTime:D:) {
        sprintf '%s.new(%s)', self.^name, join ', ', map { "{.key} => {.value}" }, do
            :$!year, :$!month, :$!day, :$!hour, :$!minute,
            second => $!second.perl,
            (timezone => $!timezone.perl
                unless $!timezone === 0),
            (formatter => $.formatter.perl
                unless &.formatter eqv &default-formatter)
    }

    multi method gist(DateTime:D:) {
            self.Str;
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


my class Date does Dateish {
    has Int $.daycount;

    method !set-daycount($dc) { $!daycount = $dc }

    method get-daycount { $!daycount }

    multi method new(:$year!, :$month = 1, :$day = 1) {
        my $d = self.bless(:$year, :$month, :$day);
        $d.check-date;
        $d!set-daycount(self.daycount-from-ymd($year,$month,$day));
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
        my $n = self.today;
        if $n.month == 12 && $n.day >= 24 {
            self.new($n.year + 1, 12, 24);
        } else {
            self.new($n.year, 12, 24);
        }
    }

    multi method new(Dateish $d) {
        self.bless(
            :year($d.year), :month($d.month), :day($d.day),
            :daycount(self.daycount-from-ymd($d.year,$d.month,$d.day))
        );
    }

    multi method new(Instant $i) {
        my $dt = DateTime.new($i);
        self.new($dt);
    }

    multi method WHICH(Date:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::unbox_i($!daycount)
            ),
            ObjAt
        );
    }

    method new-from-daycount($daycount) {
        my ($year, $month, $day) = self.ymd-from-daycount($daycount);
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
                my ($month, $year) = $!month, $!year;
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
        self.new-from-daycount($!daycount + 1);
    }
    method pred() {
        self.new-from-daycount($!daycount - 1);
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
    PROCESS::<$TZ> = get-local-timezone-offset();
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

    1 while $seconds = sleep-timer($seconds);
    True;
}

# =begin pod
#
# =head1 SEE ALSO
# Perl 6 spec <S32-Temporal|http://design.perl6.org/S32/Temporal.html>.
# The Perl 5 DateTime Project home page L<http://datetime.perl.org>.
# Perl 5 perldoc L<doc:DateTime> and L<doc:Time::Local>.
#
# The best yet seen explanation of calendars, by Claus Tøndering
# L<Calendar FAQ|http://www.tondering.dk/claus/calendar.html>.
# Similar algorithms at L<http://www.hermetic.ch/cal_stud/jdn.htm>
# and L<http://www.merlyn.demon.co.uk/daycount.htm>.
#
# <ISO 8601|http://en.wikipedia.org/wiki/ISO_8601>
# <Time zones|http://en.wikipedia.org/wiki/List_of_time_zones>
#
# As per the recommendation, the strftime() method has bee moved into a
# loadable module called DateTime::strftime.
#
# =end pod

# vim: ft=perl6 expandtab sw=4
