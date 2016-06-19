my class DateTime does Dateish {
    has int $.hour;
    has int $.minute;
    has     $.second;
    has int $.timezone;  # UTC
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

    my $valid-units := nqp::hash(
      'second',  0,
      'seconds', 0,
      'minute',  0,
      'minutes', 0,
      'hour',    0,
      'hours',   0,
      'day',     0,
      'days',    0,
      'week',    0,
      'weeks',   0,
      'month',   1,
      'months',  1,
      'year',    1,
      'years',   1,
    );
    method !VALID-UNIT($unit) {
        nqp::existskey($valid-units,$unit)
          ?? $unit
          !! X::DateTime::InvalidDeltaUnit.new(:$unit).throw
    }

    method !SET-SELF(
      $!year,$!month,$!day,$hour,$minute,$!second,$timezone,&!formatter
    ) {
        # can't assign native in attributes inside signature yet
        $!hour     = $hour,
        $!minute   = $minute,
        $!timezone = $timezone;
        self
    }
    method !new-from-positional(DateTime:
      Int() $year,
      Int() $month,
      Int() $day,
      Int() $hour,
      Int() $minute,
            $second,
            %extra,
      :$timezone = 0,
      :&formatter,
    ) {
        (1..12).in-range($month,'Month');
        (1 .. self!DAYS-IN-MONTH($year,$month)).in-range($day,'Day');
        (0..23).in-range($hour,'Hour');
        (0..59).in-range($minute,'Minute');
        (^61).in-range($second,'Second');
        my $dt = self === DateTime
          ?? nqp::create(self)!SET-SELF(
               $year,$month,$day,$hour,$minute,$second,$timezone,&formatter)
          !! self.bless(
               :$year,:$month,:$day,
               :$hour,:$minute,:$second,:$timezone,:&formatter,|%extra);

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
            ).throw unless Rakudo::Internals.is-leap-second-date($date);
        }

        $dt
    }

    proto method new(|) {*}
    multi method new(DateTime:
      \y,\mo,\d,\h,\mi,\s,:$timezone = 0,:&formatter,*%_) {
        self!new-from-positional(y,mo,d,h,mi,s,%_,:$timezone,:&formatter)
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
      *%_
      ) {
        self!new-from-positional(
          $year,$month,$day,$hour,$minute,$second,%_,:$timezone,:&formatter)
    }
    multi method new(DateTime: Date:D :$date!, *%_) {
        self.new(:year($date.year),:month($date.month),:day($date.day),|%_)
    }
    multi method new(DateTime: Instant:D $i, :$timezone = 0, *%_) {
        my ($p, $leap-second) = $i.to-posix;
        my $dt = self.new( floor($p - $leap-second).Int, |%_ );
        $dt.clone(
          :second($dt.second + $p % 1 + $leap-second), |%_
        ).in-timezone($timezone)
    }
    multi method new(DateTime:
      Numeric:D $time is copy, :$timezone = 0, :&formatter, *%_
    ) {
        # Interpret $time as a POSIX time.
        my     $second = $time % 60; $time = $time.Int div 60;
        my int $minute = $time % 60; $time = $time     div 60;
        my int $hour   = $time % 24; $time = $time     div 24;
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

        my $dt = self === DateTime
          ?? nqp::create(self)!SET-SELF(
               $year,$month,$day,$hour,$minute,$second,0,&formatter)
          !! self.bless(
               :$year,:$month,:$day,
               :$hour,:$minute,:$second,:timezone(0),:&formatter,|%_);
        $timezone ?? $dt.in-timezone($timezone) !! $dt
    }
    multi method new(DateTime:
      Str:D $datetime, :$timezone is copy, :&formatter, *%_
    ) {
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

        if $6 {
            X::DateTime::TimezoneClash.new.throw with $timezone;
            if $6.chars != 1 {
                X::OutOfRange.new(
                  what  => "minutes of timezone",
                  got   => +$6[2][0],
                  range => "0..^60",
                ).throw if $6[2] && $6[2][0] > 59;

                $timezone = (($6[1]*60 + ($6[2][0] // 0)) * 60).Int;
                  # RAKUDO: .Int is needed to avoid to avoid the nasty '-0'.
                $timezone = -$timezone if $6[0] eq '-';
            }
        }
        $timezone //= 0;

        self!new-from-positional(
          $0,$1,$2,$3,$4,+(~$5.subst(",",".")),%_,:$timezone,:&formatter)
    }

    method now(:$timezone=$*TZ, :&formatter) returns DateTime:D {
        self.new(nqp::time_n(), :$timezone, :&formatter)
    }

    method clone(*%_) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        self!new-from-positional(
          nqp::existskey($h,'year')   ?? nqp::atkey($h,'year')   !! $!year,
          nqp::existskey($h,'month')  ?? nqp::atkey($h,'month')  !! $!month,
          nqp::existskey($h,'day')    ?? nqp::atkey($h,'day')    !! $!day,
          nqp::existskey($h,'hour')   ?? nqp::atkey($h,'hour')   !! $!hour,
          nqp::existskey($h,'minute') ?? nqp::atkey($h,'minute') !! $!minute,
          nqp::existskey($h,'second') ?? nqp::atkey($h,'second') !! $!second,
          %_,
          timezone => nqp::existskey($h,'timezone')
            ?? nqp::atkey($h,'timezone')  !! $!timezone,
          formatter => nqp::existskey($h,'formatter')
            ?? nqp::atkey($h,'formatter') !! &!formatter,
        )
    }
    method !clone-without-validating(*%_) { # A premature optimization.
        return self.clone(|%_) unless self === DateTime;

        my $h := nqp::getattr(%_,Map,'$!storage');
        nqp::create(self)!SET-SELF(
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

        my $unit   = self!VALID-UNIT(@pairs.AT-POS(0).key);
        my $amount = @pairs.AT-POS(0).value.Int;
        $amount = -$amount if $earlier;

        # work on instant (tai)
        if $unit.starts-with('second') {
            self.new(self.Instant + $amount, :$!timezone, :&!formatter)
        }

        # on a leap second and not moving by second
        elsif $!second >= 60 {
            my $dt := self!clone-without-validating(
              :second($!second-1)).later(|($unit => $amount));
            $dt.hour == 23 && $dt.minute == 59 && $dt.second >= 59
              && Rakudo::Internals.is-leap-second-date($dt.yyyy-mm-dd)
              ?? $dt!clone-without-validating(:$!second)
              !! $dt
        }

        # month,year
        elsif nqp::atkey($valid-units,$unit) {
            my $date :=
              Date.new($!year,$!month,$!day).later(|($unit => $amount));
            nqp::create(self)!SET-SELF(
              nqp::getattr($date,Date,'$!year'),
              nqp::getattr($date,Date,'$!month'),
              nqp::getattr($date,Date,'$!day'),
              $!hour, $!minute, $!second, $!timezone, &!formatter
            )
        }
        # minute,hour,day,week
        else {
            my int $minute = $!minute;
            my int $hour   = $!hour;

            $minute += $amount if $unit.starts-with('minute');
            $hour   += floor($minute / 60);
            $minute %= 60;
            $hour   += $amount if $unit.starts-with('hour');

            my $day-delta = floor($hour / 24);
            $hour %= 24;

            $day-delta = $amount     if $unit.starts-with('day');
            $day-delta = 7 * $amount if $unit.starts-with('week');

            my $date := Date.new-from-daycount(self.daycount + $day-delta);
            nqp::create(self)!SET-SELF(
              nqp::getattr($date,Date,'$!year'),
              nqp::getattr($date,Date,'$!month'),
              nqp::getattr($date,Date,'$!day'),
              $hour, $minute, $!second, $!timezone, &!formatter)
        }
    }

    method truncated-to(Cool $unit) {
        my %parts;
        given self!VALID-UNIT($unit) {
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

    multi method perl(DateTime:D:) {
        self.^name
          ~ ".new($!year,$!month,$!day,$!hour,$!minute,$!second"
          ~ (',' ~ :$!timezone.perl if $!timezone)
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
