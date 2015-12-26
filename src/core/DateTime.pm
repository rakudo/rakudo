my class DateTime does Dateish {
    has int $.hour;
    has int $.minute;
    has     $.second;
    has int $.timezone;  # UTC
    has     &.formatter;
      # Not an optimization but a necessity to ensure that
      # $dt.utc.local.utc is equivalent to $dt.utc. Otherwise,
      # DST-induced ambiguity could ruin our day.

    method !formatter() { # ISO 8601 timestamp
        sprintf '%s-%02d-%02dT%02d:%02d:%s%s',
            self!year-Str, $!month, $!day, $!hour, $!minute,
            $!second.floor == $!second
              ?? $!second.Int.fmt('%02d')
              !! $!second.fmt('%09.6f'),
            $!timezone == 0
              ?? 'Z'
              !! $!timezone > 0
                ?? sprintf('+%02d:%02d',
                     ($!timezone/3600).floor,
                     ($!timezone/60%60).floor)
                !! sprintf('-%02d:%02d',
                  ($!timezone.abs/3600).floor,
                  ($!timezone.abs/60%60).floor)
    }

    method BUILD(
      $!year,$!month,$!day,$hour,$minute,$!second,$timezone,&!formatter
    ) {
        # can't assign native in attributes inside signature yet
        $!hour     = $hour,
        $!minute   = $minute,
        $!timezone = $timezone;
        self
    }

    multi method new(DateTime:
      Int() $year,
      Int() $month,
      Int() $day,
      Int() $hour,
      Int() $minute,
            $second,
      Int() $timezone,
      :&formatter,
    ) {
        (1..12).in-range($month,'Month');
        (1 .. self!DAYS-IN-MONTH($year,$month)).in-range($day,'Day');
        (0..23).in-range($hour,'Hour');
        (0..59).in-range($minute,'Minute');
        (^61).in-range($second,'Second');
        my $dt = nqp::create(self).BUILD(
          $year,$month,$day,$hour,$minute,$second,$timezone,&formatter
        );

        # check leap second spec
        if $second >= 60 {
            my $utc = $timezone ?? $dt.utc !! $dt;
            X::OutOfRange.new(
              what  => 'Second',
              range => "0..^60",
              got   => $second,
              comment => 'a leap second can occur only at 23:59',
            ).throw unless $utc.hour == 23 && $utc.minute == 59;
            my $date = $utc.yyyy-mm-dd;
            X::OutOfRange.new(
              what  => 'Second',
              range => "0..^60",
              got   => $second,
              comment => "There is no leap second on UTC $date",
            ).throw unless tai-utc.leap-second-dates.first($date);
        }

        $dt
    }
    multi method new(DateTime:
      :$year!,
      :$month    = 1,
      :$day      = 1,
      :$hour     = 0,
      :$minute   = 0,
      :$second   = 0,
      :$timezone = 0,
      :&formatter,
      ) {
        self.new($year,$month,$day,$hour,$minute,$second,$timezone,:&formatter)
    }
    multi method new(DateTime: Date:D :$date!, *%_) {
        self.new(:year($date.year),:month($date.month),:day($date.day),|%_)
    }
    multi method new(DateTime: Instant:D $i, :$timezone = 0, :&formatter) {
        my ($p, $leap-second) = $i.to-posix;
        my $dt = self.new: floor($p - $leap-second).Int, :&formatter;
        $dt.clone(
          :second($dt.second + $p % 1 + $leap-second)
        ).in-timezone($timezone)
    }
    multi method new(Int:D $time is copy, :$timezone = 0, :&formatter) {
        # Interpret $time as a POSIX time.
        my int $second = $time % 60; $time = $time div 60;
        my int $minute = $time % 60; $time = $time div 60;
        my int $hour   = $time % 24; $time = $time div 24;
        # Day month and leap year arithmetic, based on Gregorian day #.
        # 2000-01-01 noon UTC == 2451558.0 Julian == 2451545.0 Gregorian
        $time += 2440588;   # because 2000-01-01 == Unix epoch day 10957
        my Int $a = $time + 32044;     # date algorithm from Claus Tøndering
        my Int $b = (4 * $a + 3) div 146097; # 146097 = days in 400 years
        my Int $c = $a - (146097 * $b) div 4;
        my Int $d = (4 * $c + 3) div 1461;       # 1461 = days in 4 years
        my Int $e = $c - ($d * 1461) div 4;
        my Int $m = (5 * $e + 2) div 153; # 153 = days in Mar-Jul Aug-Dec
        my int $day   = $e - (153 * $m + 2) div 5 + 1;
        my int $month = $m + 3 - 12 * ($m div 10);
        my Int $year  = $b * 100 + $d - 4800 + $m div 10;

        my $dt = nqp::create(self).BUILD(
          $year,$month,$day,$hour,$minute,$second,0,&formatter
        );
        $timezone ?? $dt.in-timezone($timezone) !! $dt
    }
    multi method new(Str $datetime, :$timezone, :&formatter) {
        X::Temporal::InvalidFormat.new(
          invalid-str => $datetime,
          target      => 'DateTime',
          format      => 'an ISO 8601 timestamp (yyyy-mm-ddThh:mm:ssZ or yyyy-mm-ddThh:mm:ss+01:00)',
        ).throw unless $datetime ~~ /^
          (<[+-]>? \d**4 \d*)                            # year
          '-'
          (\d\d)                                         # month
          '-'
          (\d\d)                                         # day
          <[Tt]>                                         # time separator
          (\d\d)                                         # hour
          ':'
          (\d\d)                                         # minute
          ':'
          (\d\d[<[\.,]>\d ** 1..6]?)                     # second
          (<[Zz]> || (<[\-\+]>) (\d\d) (':'? (\d\d))? )? # timezone
        $/;

        my int $tz = 0;
        if $6 {
            X::DateTime::TimezoneClash.new.throw if $timezone;
            if $6.chars != 1 {
                X::OutOfRange.new(
                  what  => "minutes of timezone",
                  got   => +$6[2][0],
                  range => "0..^60",
                ).throw if $6[2] && $6[2][0] > 59;

                $tz = (($6[1]*60 + ($6[2][0] // 0)) * 60).Int;
                  # RAKUDO: .Int is needed to avoid to avoid the nasty '-0'.
                $tz = -$tz if $6[0] eq '-';
            }
        }
        elsif $timezone {
            $tz = $timezone;
        }

        self.new(+$0,+$1,+$2,+$3,+$4,+(~$5.subst(",",".")),$tz,:&formatter)
    }

    method now(:$timezone=$*TZ, :&formatter) returns DateTime:D {
        self.new(now, :$timezone, :&formatter)
    }

    method clone(*%_) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        self.new(
          nqp::existskey($h,'year')   ?? nqp::atkey($h,'year')   !! $!year,
          nqp::existskey($h,'month')  ?? nqp::atkey($h,'month')  !! $!month,
          nqp::existskey($h,'day')    ?? nqp::atkey($h,'day')    !! $!day,
          nqp::existskey($h,'hour')   ?? nqp::atkey($h,'hour')   !! $!hour,
          nqp::existskey($h,'minute') ?? nqp::atkey($h,'minute') !! $!minute,
          nqp::existskey($h,'second') ?? nqp::atkey($h,'second') !! $!second,
          nqp::existskey($h,'timezone')
            ?? nqp::atkey($h,'timezone')  !! $!timezone,
          formatter => nqp::existskey($h,'formatter')
            ?? nqp::atkey($h,'formatter') !! &!formatter,
        )
    }
    method !clone-without-validating(*%_) { # A premature optimization.
        my $h := nqp::getattr(%_,Map,'$!storage');
        nqp::create(self).BUILD(
          nqp::existskey($h,'year')   ?? nqp::atkey($h,'year')   !! $!year,
          nqp::existskey($h,'month')  ?? nqp::atkey($h,'month')  !! $!month,
          nqp::existskey($h,'day')    ?? nqp::atkey($h,'day')    !! $!day,
          nqp::existskey($h,'hour')   ?? nqp::atkey($h,'hour')   !! $!hour,
          nqp::existskey($h,'minute') ?? nqp::atkey($h,'minute') !! $!minute,
          nqp::existskey($h,'second') ?? nqp::atkey($h,'second') !! $!second,
          nqp::existskey($h,'timezone')
            ?? nqp::atkey($h,'timezone') !! $!timezone,
          &!formatter,
        )
    }

    method Instant() {
        Instant.from-posix: self.posix + $!second % 1, $!second >= 60;
    }

    method posix($ignore-timezone?) {
        return self.utc.posix if $!timezone && !$ignore-timezone;

        # algorithm from Claus Tøndering
        my int $a = (14 - $!month) div 12;
        my int $y = $!year + 4800 - $a;
        my int $m = $!month + 12 * $a - 3;
        my int $jd = $!day + (153 * $m + 2) div 5 + 365 * $y
            + $y div 4 - $y div 100 + $y div 400 - 32045;
        ($jd - 2440588) * 86400
          + $!hour      * 3600
          + $!minute    * 60
          + self.whole-second
    }

    method offset()            { $!timezone }
    method offset-in-minutes() { $!timezone / 60 }
    method offset-in-hours()   { $!timezone / 3600 }

    method later(:$earlier, *%unit) {
        my @pairs = %unit.pairs;
        die "More than one time unit supplied" if @pairs > 1;
        die "No time unit supplied"        unless @pairs;

        my $unit   = @pairs.AT-POS(0).key;
        self!VALID-UNIT($unit);

        my $amount = @pairs.AT-POS(0).value;
        $amount = -$amount if $earlier;

        my int $hour   = $!hour;
        my int $minute = $!minute;
        my $date;

        given $unit {
            when 'second' | 'seconds' {
                return
                  self.new(self.Instant + $amount, :$!timezone, :&!formatter);
            }
            when 'minute' | 'minutes' { $minute += $amount; proceed }

            $hour += floor($minute / 60);
            $minute %= 60;

            when 'hour' | 'hours'     { $hour += $amount; proceed }

            my $day-delta += floor($hour / 24);
            $hour %= 24;

            when 'day' | 'days'       { $day-delta += $amount; proceed }
            when 'week' | 'weeks'     { $day-delta += 7 * $amount; proceed }

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

            $date = Date.new-from-daycount(self.daycount + $day-delta);
        }

        my $second = $!second;
        if $second > 59 + ?tai-utc.leap-second-dates.first(~$date) {
            $second -= 60;
            ++$minute;
            if $minute > 59 {
                $minute -= 60;
                ++$hour;
                if $hour > 23 {
                    $hour -= 24;
                    ++$date;
                }
            }
        }
        nqp::create(self).BUILD($date.year,$date.month,$date.day,
          $hour,$minute,$second,$!timezone,&!formatter)
    }

    method truncated-to(Cool $unit) {
        self!VALID-UNIT($unit);
        my %parts;
        given $unit {
            %parts<second> = self.whole-second;
            when 'second' | 'seconds' {}
            %parts<second> = 0;
            when 'minute' | 'minutes' {}
            %parts<minute> = 0;
            when 'hour'   | 'hours'   {}
            %parts<hour> = 0;
            when 'day'    | 'days'    {}
            %parts = self!truncate-ymd($unit, %parts);
        }
        self!clone-without-validating(|%parts);
    }
    method whole-second() { $!second.Int }

    method in-timezone($timezone) {
        return self if $timezone == $!timezone;

        my int $old-offset = self.offset;
        my int $new-offset = $timezone.Int;
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
        self!ymd-from-daycount(self.daycount + $c div 24,
          %parts<year>,%parts<month>,%parts<day>) if $c div 24;
        self!clone-without-validating: :$timezone, |%parts;
    }

    method utc()   { self.in-timezone(0) }
    method local() { self.in-timezone($*TZ) }

    method Date() { Date.new($!year,$!month,$!day) }

    multi method Str(DateTime:D:) {
        &!formatter ?? &!formatter(self) !! self!formatter
    }
    multi method perl(DateTime:D:) {
        self.^name
          ~ ".new($!year,$!month,$!day,$!hour,$!minute,$!second,$!timezone)"
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
