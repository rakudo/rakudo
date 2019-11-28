my class DateTime does Dateish {
    has Int $.hour;
    has Int $.minute;
    has     $.second;
    has Int $.timezone;  # UTC
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

#?if moar
    my constant $valid-units = nqp::hash(
#?endif
#?if !moar
    my $valid-units := nqp::hash(
#?endif
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
        Int:D \year,
        Int:D \month,
        Int:D \day,
        Int:D \hour,
        Int:D \minute,
              \second,
        Int:D \timezone,
              &formatter
    --> DateTime:D) {
        nqp::bind($!year,      year);  # R#2581
        nqp::bind($!month,     month);
        nqp::bind($!day,       day);
        nqp::bind(&!formatter, &formatter);
        $!hour      := hour;
        $!minute    := minute;
        $!second    := second;
        $!timezone  := timezone;
        self
    }

    method !new-from-positional(DateTime:
      Int() $year,
      Int() $month,
      Int() $day,
      Int() $hour,
      Int() $minute,
            $second,  # can have fractional seconds
            %extra,
      :$timezone = 0,
      :&formatter,
    --> DateTime:D) {
        1 <= $month <= 12
          || X::OutOfRange.new(:what<Month>,:got($month),:range<1..12>).throw;
        1 <= $day <= self!DAYS-IN-MONTH($year,$month)
          || X::OutOfRange.new(
               :what<Day>,
               :got($day),
               :range("1..{self!DAYS-IN-MONTH($year,$month)}")
             ).throw;
        0 <= $hour <= 23
          || X::OutOfRange.new(:what<Hour>,:got($hour),:range<0..23>).throw;
        0 <= $minute <= 59
          || X::OutOfRange.new(:what<Minute>,:got($minute),:range<0..59>).throw;
        (^61).in-range($second,'Second'); # some weird semantics need this

        my $dt = nqp::eqaddr(self.WHAT,DateTime)
          ?? nqp::create(self)!SET-SELF(
               $year,$month,$day,$hour,$minute,$second,$timezone,&formatter)
          !! self.bless(
               :$year,:$month,:$day,
               :$hour,:$minute,:$second,:$timezone,:&formatter,|%extra
             )!SET-DAYCOUNT;

        $second >= 60 ?? $dt!check-leap-second !! $dt
    }

    method !check-leap-second(--> DateTime:D) {
        my $utc := $!timezone ?? self.utc !! self;
        X::OutOfRange.new(
          what  => 'Second',
          range => "0..^60",
          got   => $!second,
          comment => 'a leap second can occur only at 23:59',
        ).throw unless $utc.hour == 23 && $utc.minute == 59;

        my $date := $utc.yyyy-mm-dd;
        X::OutOfRange.new(
          what  => 'Second',
          range => "0..^60",
          got   => $!second,
          comment => "There is no leap second on UTC $date",
        ).throw unless Rakudo::Internals.is-leap-second-date($date);

        self
    }

    proto method new(|) {*}
    multi method new(DateTime:
      \y,\mo,\d,\h,\mi,\s,:$timezone = 0,:&formatter,*%_
    --> DateTime:D) {
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
    --> DateTime:D) {
        self!new-from-positional(
          $year,$month,$day,$hour,$minute,$second,%_,:$timezone,:&formatter)
    }
    multi method new(DateTime:
      Date:D :$date!, *%_
    --> DateTime:D) {
        self.new(:year($date.year),:month($date.month),:day($date.day),|%_)
    }
    multi method new(DateTime:
      Instant:D $i, :$timezone = 0, *%_
    --> DateTime:D) {
        my ($p, $leap-second) = $i.to-posix;
        my $dt = self.new( floor($p - $leap-second).Int, |%_ );
        $dt.clone(
          :second($dt.second + $p % 1 + $leap-second), |%_
        ).in-timezone($timezone)
    }
    multi method new(DateTime:
      Numeric:D $epoch, :$timezone = 0, :&formatter, *%_
    --> DateTime:D) {

        # Interpret $time as a POSIX time.
        my $second := $epoch % 60;
        my Int $minutes := nqp::div_I($epoch.Int, 60, Int);
        my Int $minute  := nqp::mod_I($minutes, 60, Int);
        my Int $hours   := nqp::div_I($minutes, 60, Int);
        # XXX changing this with a nqp::mod_I causes execution error:
        # Cannot unbox a type object (Int) to int.  Go figure!
        my Int $hour    := $hours % 24;
        my Int $days    := nqp::div_I($hours, 24, Int);

        # Day month and leap year arithmetic, based on Gregorian day #.
        # 2000-01-01 noon UTC == 2451558.0 Julian == 2451545.0 Gregorian
        my Int $julian := $days + 2440588;   # because 2000-01-01 == Unix epoch day 10957

        my Int $a := $julian + 32044;     # date algorithm from Claus Tøndering
        my Int $b := nqp::div_I(4 * $a + 3, 146097, Int);  # 146097 = days in 400 years
        my Int $c := $a - nqp::div_I(146097 * $b, 4, Int);
        my Int $d := nqp::div_I(4 * $c + 3, 1461, Int);  # 1461 = days in 4 years
        my Int $e := $c - nqp::div_I($d * 1461, 4, Int);
        my Int $m := nqp::div_I(5 * $e + 2, 153, Int); # 153 = days in Mar-Jul Aug-Dec
        my Int $day   := $e - nqp::div_I(153 * $m + 2, 5, Int) + 1;
        my Int $month := $m + 3 - 12 * nqp::div_I($m, 10, Int);
        my Int $year  := $b * 100 + $d - 4800 + nqp::div_I($m, 10, Int);

        my $dt := nqp::eqaddr(self.WHAT,DateTime)
          ?? ( nqp::elems(nqp::getattr(%_,Map,'$!storage'))
            ?? die "Unexpected named parameter{"s" if %_ > 1} "
                 ~ %_.keys.map({"`$_`"}).join(", ") ~ " passed. Were you "
                 ~ "trying to use the named parameter form of .new() but "
                 ~ "accidentally passed one named parameter as a positional?"
            !! nqp::create(self)!SET-SELF(
                 $year,$month,$day,$hour,$minute,$second,0,&formatter)
             )
          !! self.bless(
               :$year,:$month,:$day,
               :$hour,:$minute,:$second,:timezone(0),:&formatter,|%_
             )!SET-DAYCOUNT;
        $timezone ?? $dt.in-timezone($timezone) !! $dt
    }
    multi method new(DateTime:
      Str:D $datetime, :$timezone is copy, :&formatter, *%_
    --> DateTime:D) {
        X::Temporal::InvalidFormat.new(
          invalid-str => $datetime,
          target      => 'DateTime',
          format      => 'an ISO 8601 timestamp (yyyy-mm-ddThh:mm:ssZ or yyyy-mm-ddThh:mm:ss+01:00)',
        ).throw unless $datetime.chars == $datetime.codes and $datetime ~~ /^
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

    method now(:$timezone=$*TZ, :&formatter --> DateTime:D) {
        self.new(nqp::time_n(), :$timezone, :&formatter)
    }

    method clone(DateTime:D: *%_ --> DateTime:D) {
        my \h := nqp::getattr(%_,Map,'$!storage');
        self!new-from-positional(
          nqp::ifnull(nqp::atkey(h,'year'),  $!year),
          nqp::ifnull(nqp::atkey(h,'month'), $!month),
          nqp::ifnull(nqp::atkey(h,'day'),   $!day),
          nqp::ifnull(nqp::atkey(h,'hour'),  $!hour),
          nqp::ifnull(nqp::atkey(h,'minute'),$!minute),
          nqp::ifnull(nqp::atkey(h,'second'),$!second),
          %_,
          timezone  => nqp::ifnull(nqp::atkey(h,'timezone'),$!timezone),
          formatter => nqp::ifnull(nqp::atkey(h,'formatter'),&!formatter),
        )
    }

    # A premature optimization.
    method !clone-without-validating(DateTime:D: *%_ --> DateTime:D) {
        nqp::if(
          nqp::eqaddr(self.WHAT,DateTime),
          nqp::stmts(
            (my \h := nqp::getattr(%_,Map,'$!storage')),
            nqp::create(self)!SET-SELF(
              nqp::ifnull(nqp::atkey(h,'year'),    $!year),
              nqp::ifnull(nqp::atkey(h,'month'),   $!month),
              nqp::ifnull(nqp::atkey(h,'day'),     $!day),
              nqp::ifnull(nqp::atkey(h,'hour'),    $!hour),
              nqp::ifnull(nqp::atkey(h,'minute'),  $!minute),
              nqp::ifnull(nqp::atkey(h,'second'),  $!second),
              nqp::ifnull(nqp::atkey(h,'timezone'),$!timezone),
              &!formatter,
            )
          ),
          self.clone(|%_)
        )
    }

    method Instant(DateTime:D: --> Instant:D) {
        Instant.from-posix: self.posix + $!second % 1, $!second >= 60;
    }

    method posix(DateTime:D: $ignore-timezone? --> Int:D) {
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

    method offset(DateTime:D:            --> Int:D) { $!timezone        }
    method offset-in-minutes(DateTime:D: --> Rat:D) { $!timezone / 60   }
    method offset-in-hours(DateTime:D:   --> Rat:D) { $!timezone / 3600 }

    method hh-mm-ss(DateTime:D: --> Str:D) {
        sprintf "%02d:%02d:%02d", $!hour,$!minute,$!second
    }

    method later(DateTime:D: :$earlier, *%unit --> DateTime:D) {

        # basic sanity check
        nqp::if(
          nqp::eqaddr(
            (my \later := (my \iterator := %unit.iterator).pull-one),
            IterationEnd
          ),
          (die "No time unit supplied"),
          nqp::unless(
            nqp::eqaddr(iterator.pull-one,IterationEnd),
            (die "More than one time unit supplied")
          )
        );
        my $unit  := later.key;
        my $amount = later.value;
        $amount = -$amount if $earlier;

        # work on instant (tai)
        if $unit.starts-with('second') {
            self.new(self.Instant + $amount, :$!timezone, :&!formatter)
        }
        else {
            $amount .= Int;
            # on a leap second and not moving by second
            if $!second >= 60 {
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
    }

    method truncated-to(DateTime:D: Cool $unit --> DateTime:D) {
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
    method whole-second(DateTime:D: --> Int:D) { $!second.Int }

    method in-timezone(DateTime:D: Int(Cool) $timezone --> DateTime:D) {
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

    method utc(  DateTime:D: --> DateTime:D) { self.in-timezone(0)    }
    method local(DateTime:D: --> DateTime:D) { self.in-timezone($*TZ) }

    proto method Date() {*}
    multi method Date(DateTime:D: --> Date:D) { Date.new($!year,$!month,$!day) }
    multi method Date(DateTime:U: --> Date:U) { Date }
    method DateTime(--> DateTime) { self }

    multi method perl(DateTime:D: --> Str:D) {
        self.^name
          ~ ".new($!year,$!month,$!day,$!hour,$!minute,$!second"
          ~ (',' ~ :$!timezone.perl if $!timezone)
          ~ ')'
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*TZ', {
    PROCESS::<$TZ> = Rakudo::Internals.get-local-timezone-offset
}

multi sub infix:«<»(DateTime:D \a, DateTime:D \b --> Bool:D) {
    a.Instant < b.Instant
}
multi sub infix:«>»(DateTime:D \a, DateTime:D \b --> Bool:D) {
    a.Instant > b.Instant
}
multi sub infix:«<=»(DateTime:D \a, DateTime:D \b --> Bool:D) {
    a.Instant <= b.Instant
}
multi sub infix:«>=»(DateTime:D \a, DateTime:D \b --> Bool:D) {
    a.Instant >= b.Instant
}
multi sub infix:«==»(DateTime:D \a, DateTime:D \b --> Bool:D) {
    a.Instant == b.Instant
}
multi sub infix:«!=»(DateTime:D \a, DateTime:D \b --> Bool:D) {
    a.Instant != b.Instant
}
multi sub infix:«<=>»(DateTime:D \a, DateTime:D \b --> Order:D) {
    a.Instant <=> b.Instant
}
multi sub infix:«cmp»(DateTime:D \a, DateTime:D \b --> Order:D) {
    a.Instant cmp b.Instant
}
multi sub infix:<->(DateTime:D \a, DateTime:D \b --> Duration:D) {
    a.Instant - b.Instant
}
multi sub infix:<->(DateTime:D \a, Duration:D \b --> DateTime:D) {
    a.new(a.Instant - b).in-timezone(a.timezone)
}
multi sub infix:<+>(DateTime:D \a, Duration:D \b --> DateTime:D) {
    a.new(a.Instant + b).in-timezone(a.timezone)
}
multi sub infix:<+>(Duration:D \a, DateTime:D \b --> DateTime:D) {
    b.new(b.Instant + a).in-timezone(b.timezone)
}

# vim: ft=perl6 expandtab sw=4
