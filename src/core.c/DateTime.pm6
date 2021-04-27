my class DateTime does Dateish {
    has int $.hour;
    has int $.minute;
    has     $.second;
    has int $.timezone;  # UTC
      # Not an optimization but a necessity to ensure that
      # $dt.utc.local.utc is equivalent to $dt.utc. Otherwise,
      # DST-induced ambiguity could ruin our day.

    my int $last-dst = -1;  # never matches initially
    my int $TZ-was-set-explicitly;
    my int $TZ-offset;
    sub get-local-timezone-offset() {
        if $TZ-was-set-explicitly {
            $TZ-offset
        }

        # not set explicitly
        else {
            my int $utc = nqp::div_i(nqp::time,1000000000);
            my $lt     := nqp::decodelocaltime($utc);

            # first time, or possible DST change
            if nqp::isne_i(nqp::atpos_i($lt,8),$last-dst) {
                $last-dst = nqp::atpos_i($lt,8);

                # algorithm from Claus Tøndering
                my int $a = (14 - nqp::atpos_i($lt,4)) div 12;
                my int $y = nqp::atpos_i($lt,5) + 4800 - $a;
                my int $m = nqp::atpos_i($lt,4) + 12 * $a - 3;
                my int $jd = nqp::atpos_i($lt,3) + (153 * $m + 2) div 5
                  + 365 * $y + $y div 4 - $y div 100 + $y div 400 - 32045;
                $TZ-offset = (
                  ($jd - 2440588) * 86400
                    + nqp::atpos_i($lt,2) * 3600
                    + nqp::atpos_i($lt,1) * 60
                    + nqp::atpos_i($lt,0)
                ) - $utc
            }

            # cannot have been a DST change
            else {
                $TZ-offset
            }
        }
    }

    Rakudo::Internals.REGISTER-DYNAMIC: '$*TZ', sub TZ is raw {
        PROCESS::<$TZ> := Proxy.new(
          FETCH => -> $ {
              get-local-timezone-offset
          },
          STORE => -> $, int $offset {
              $TZ-was-set-explicitly = 1;
              $TZ-offset             = $offset;
          }
        )
    }

    method !formatter() { # ISO 8601 timestamp
        my $parts := nqp::list_s;
        nqp::islt_i($!year,1000) || nqp::isgt_i($!year,9999)
          ?? nqp::push_s($parts,self!year-Str)
          !! nqp::push_s($parts,nqp::getattr_i(self,DateTime,'$!year'));

        nqp::push_s($parts,'-');
        nqp::push_s($parts,nqp::x('0',nqp::islt_i($!month,10)));
        nqp::push_s($parts,nqp::getattr_i(self,DateTime,'$!month'));

        nqp::push_s($parts,'-');
        nqp::push_s($parts,nqp::x('0',nqp::islt_i($!day,10)));
        nqp::push_s($parts,nqp::getattr_i(self,DateTime,'$!day'));

        nqp::push_s($parts,'T');
        nqp::push_s($parts,nqp::x('0',nqp::islt_i($!hour,10)));
        nqp::push_s($parts,nqp::getattr_i(self,DateTime,'$!hour'));

        nqp::push_s($parts,':');
        nqp::push_s($parts,nqp::x('0',nqp::islt_i($!minute,10)));
        nqp::push_s($parts,nqp::getattr_i(self,DateTime,'$!minute'));

        nqp::push_s($parts,':');
        my int $second = $!second.floor;
        if $second == $!second {
            nqp::push_s($parts,nqp::x('0',nqp::islt_i($second,10)));
            nqp::push_s($parts,$second);
        }
        elsif $second {
            my int $int   = ($!second * 1000000 + .5).Int;
            my int $whole = nqp::substr($int,0,nqp::chars($int) - 6);
            nqp::push_s($parts,nqp::x('0',nqp::islt_i($whole,10)));
            nqp::push_s($parts,$whole);
            nqp::push_s($parts,'.');
            nqp::push_s($parts,nqp::substr($int,nqp::chars($int) - 6));
        }
        else {
            my int $int = ($!second * 1000000 + .5).Int;
            nqp::push_s($parts,'00.');
            nqp::push_s($parts,nqp::x('0',6 - nqp::chars($int)));
            nqp::push_s($parts,$int);
        }

        if nqp::getattr_i(self,DateTime,'$!timezone') -> int $tz {
            nqp::push_s($parts,nqp::islt_i($tz,0) ?? '-' !! '+');
            my int $hours = nqp::div_i(nqp::abs_i($tz),3600);
            nqp::push_s($parts,nqp::x('0',nqp::islt_i($hours,10)));
            nqp::push_s($parts,$hours);

            nqp::push_s($parts,':');
            my int $minutes = nqp::div_i(nqp::mod_i(nqp::abs_i($tz),3600),60);
            nqp::push_s($parts,nqp::x('0',nqp::islt_i($minutes,10)));
            nqp::push_s($parts,$minutes);
        }
        else {
            nqp::push_s($parts,'Z');
        }

        nqp::join('',$parts)
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

    method !SET-SELF(
        int \year,
        int \month,
        int \day,
        int \hour,
        int \minute,
            \second,
        int \timezone,
            &formatter
    --> DateTime:D) {
        nqp::bindattr_i(self,DateTime,'$!year',year);
        nqp::bindattr_i(self,DateTime,'$!month',month);
        nqp::bindattr_i(self,DateTime,'$!day',day);
        nqp::bindattr_i(self,DateTime,'$!hour',hour);
        nqp::bindattr_i(self,DateTime,'$!minute',minute);
        nqp::bindattr(  self,DateTime,'$!second',second);
        nqp::bindattr(  self,DateTime,'&!formatter',&formatter);
        nqp::bindattr_i(self,DateTime,'$!timezone',timezone);
        self
    }

    method !new-from-positional(DateTime:
      Int() $year,
      Int() $month,
      Int() $day,
      Int() $hour,
      Int() $minute,
            $second,  # can have fractional seconds
      Int() $timezone,
            &formatter,
            %extra,
    --> DateTime:D) {
        self!oor("Month",$month,"1..12")
          if nqp::islt_I(nqp::decont($month),1)
          || nqp::isgt_I(nqp::decont($month),12);

        my $DIM := self!DAYS-IN-MONTH($year,$month);
        self!oor("Day",$day,"1..$DIM")
          if nqp::islt_I(nqp::decont($day),1)
          || nqp::isgt_I(nqp::decont($day),$DIM);

        self!oor("Hour",$hour,"0..23")
          if nqp::islt_I(nqp::decont($hour),0)
          || nqp::isgt_I(nqp::decont($hour),23);

        self!oor("Minute",$minute,"0..59")
          if nqp::islt_I(nqp::decont($minute),0)
          || nqp::isgt_I(nqp::decont($minute),59);

        (^61).in-range($second,'Second'); # some weird semantics need this

        my $dt := nqp::eqaddr(self.WHAT,DateTime)
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
        self!new-from-positional(y,mo,d,h,mi,s,$timezone,&formatter,%_)
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
          $year,$month,$day,$hour,$minute,$second,$timezone,&formatter,%_)
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
      Numeric:D $epoch is copy, :$timezone = 0, :&formatter, *%_
    --> DateTime:D) {

        # allow for timezone offset
        $epoch = $epoch + $timezone;

        # Interpret $time as a POSIX time.
        my $second := $epoch % 60;
        my Int $minutes := nqp::div_I($epoch.Int, 60, Int);
        my Int $minute  := nqp::mod_I($minutes, 60, Int);
        my Int $hours   := nqp::div_I($minutes, 60, Int);
        my Int $hour    := nqp::mod_I($hours, 24, Int);
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

        nqp::eqaddr(self.WHAT,DateTime)
          ?? ( nqp::elems(nqp::getattr(%_,Map,'$!storage'))
            ?? die "Unexpected named parameter{"s" if %_ > 1} "
                 ~ %_.keys.map({"`$_`"}).join(", ") ~ " passed. Were you "
                 ~ "trying to use the named parameter form of .new() but "
                 ~ "accidentally passed one named parameter as a positional?"
            !! nqp::create(self)!SET-SELF(
                 $year,$month,$day,$hour,$minute,$second,$timezone,&formatter)
             )
          !! self.bless(
               :$year,:$month,:$day,
               :$hour,:$minute,:$second,:$timezone,:&formatter,|%_
             )!SET-DAYCOUNT;
    }
    multi method new(DateTime:
      Str:D $datetime, :$timezone is copy, :&formatter, *%_
    --> DateTime:D) {
        self!tif(
          $datetime,
          'DateTime',
          'an ISO 8601 timestamp (yyyy-mm-ddThh:mm:ssZ or yyyy-mm-ddThh:mm:ss+01:00)'
        ) unless $datetime.chars == $datetime.codes and $datetime ~~ /^
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
          (\d\d[<[\.,]>\d ** 1..12]?)                    # second
          [<[Zz]> | (<[\-\+]> \d\d) [':'? (\d\d)]? ]?    # timezone
        $/;

        my $string := $5.Str;
        my $second := $string.Numeric;
        $second := $string.subst(",",".").Numeric
          if nqp::istype($second,Failure);

        if $6 {
            X::DateTime::TimezoneClash.new.throw with $timezone;
            my $seconds := $7 ?? $7.Int * 60 !! 0;
            X::OutOfRange.new(
              what  => "minutes of timezone",
              got   => $seconds / 60,
              range => "0..^60",
            ).throw if $seconds >= 3600;

            $timezone := $6.Int * 3600;
            $timezone := $timezone < 0
              || $timezone == 0 && $6.Str.starts-with('-')
              ?? $timezone - $seconds
              !! $timezone + $seconds;
        }
        else {
            $timezone := 0 unless nqp::isconcrete($timezone);
        }

        self!new-from-positional(
          $0,$1,$2,$3,$4,$second,$timezone,&formatter,%_)
    }

    method now(:$timezone, :&formatter --> DateTime:D) {
        self.new(nqp::div_n(nqp::time(),1000000000e0),
          timezone => $timezone // get-local-timezone-offset,
          :&formatter
        )
    }

    method clone(DateTime:D: *%_ --> DateTime:D) {
        my \h := nqp::getattr(%_,Map,'$!storage');
        self!new-from-positional(
          nqp::ifnull(nqp::atkey(h,'year'),     $!year),
          nqp::ifnull(nqp::atkey(h,'month'),    $!month),
          nqp::ifnull(nqp::atkey(h,'day'),      $!day),
          nqp::ifnull(nqp::atkey(h,'hour'),     $!hour),
          nqp::ifnull(nqp::atkey(h,'minute'),   $!minute),
          nqp::ifnull(nqp::atkey(h,'second'),   $!second),
          nqp::ifnull(nqp::atkey(h,'timezone'), $!timezone),
          nqp::ifnull(nqp::atkey(h,'formatter'),&!formatter),
          %_,
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

    method day-fraction(DateTime:D: --> Real:D) {
        (nqp::add_i(
          nqp::mul_i($!hour,3600),
          nqp::mul_i($!minute,60)
        ) + $!second) / nqp::add_i(
          86400,
          Rakudo::Internals.daycount-leapseconds(self.daycount)
        )
    }

    method modified-julian-date(DateTime:D: --> Real:D) {
        self.daycount + self.day-fraction
    }

    method julian-date(DateTime:D: --> Real:D) {
        self.modified-julian-date + 2_400_000.5
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

    # workhorse method of moving a DateTime
    method move-by-unit(str $unit, \amount) is implementation-detail {

        # work on instant (tai)
        return self.new(self.Instant + amount, :$!timezone, :&!formatter)
          if nqp::eqat($unit,'second',0);

        my int $amount = amount.Int;

        # on a leap second and not moving by second
        if $!second >= 60 {
            my $dt := self!clone-without-validating(
              :second($!second-1)
            ).move-by-unit($unit, $amount);
            $dt.hour == 23 && $dt.minute == 59 && $dt.second >= 59
              && Rakudo::Internals.is-leap-second-date($dt.yyyy-mm-dd)
              ?? $dt!clone-without-validating(:$!second)
              !! $dt
        }

        # month,year
        elsif nqp::atkey($valid-units,$unit) {
            my $date :=
              Date.new($!year,$!month,$!day).move-by-unit($unit,$amount);
            nqp::create(self)!SET-SELF(
              nqp::getattr_i($date,Date,'$!year'),
              nqp::getattr_i($date,Date,'$!month'),
              nqp::getattr_i($date,Date,'$!day'),
              $!hour, $!minute, $!second, $!timezone, &!formatter
            )
        }
        # minute,hour,day,week
        else {
            my int $minute = $!minute;
            my int $hour   = $!hour;

            $minute = nqp::add_i($minute,$amount)
              if nqp::eqat($unit,'minute',0);
            $hour   = nqp::add_i($hour,nqp::div_i($minute,60));
            $minute = nqp::islt_i($minute,0)
              ?? nqp::add_i(nqp::mod_i($minute,60),60)
              !! nqp::mod_i($minute,60);
            $hour   = nqp::add_i($hour,$amount)
              if nqp::eqat($unit,'hour',0);

            my int $day-delta = nqp::div_i($hour,24);
            $hour = nqp::islt_i($hour,0)
              ?? nqp::add_i(nqp::mod_i($hour,24),24)
              !! nqp::mod_i($hour,24);

            $day-delta = $amount               if nqp::eqat($unit,'day',0);
            $day-delta = nqp::mul_i($amount,7) if nqp::eqat($unit,'week',0);

            my $date :=
              Date.new-from-daycount(nqp::add_i(self.daycount,$day-delta));
            nqp::create(self)!SET-SELF(
              nqp::getattr_i($date,Date,'$!year'),
              nqp::getattr_i($date,Date,'$!month'),
              nqp::getattr_i($date,Date,'$!day'),
              $hour, $minute, $!second, $!timezone, &!formatter)
        }
    }

    method truncated-to(DateTime:D: str $unit --> DateTime:D) {
        my $truncated := nqp::clone(self);
        my $what      := self.WHAT;
        nqp::if(
          nqp::eqat($unit,'second',0),
          nqp::bindattr($truncated,$what,'$!second',$!second.Int),
          nqp::stmts(
            nqp::bindattr($truncated,$what,'$!second',0),
            nqp::unless(
              nqp::eqat($unit,'minute',0),
              nqp::stmts(
                nqp::bindattr_i($truncated,$what,'$!minute',0),
                nqp::unless(
                  nqp::eqat($unit,'hour',0),
                  nqp::stmts(
                    nqp::bindattr_i($truncated,$what,'$!hour',0),
                    nqp::unless(
                      nqp::eqat($unit,'day',0),
                      nqp::stmts(
                        nqp::bindattr_i($truncated,$what,'$!daycount',0),
                        nqp::if(
                          nqp::eqat($unit,'week',0),
                          ($truncated := $truncated.move-by-unit(
                            'day',
                            nqp::sub_i(1,$truncated.day-of-week)
                          )),
                          nqp::stmts(
                            nqp::bindattr_i($truncated,$what,'$!day',1),
                            nqp::unless(
                              nqp::eqat($unit,'month',0),
                              nqp::stmts(
                                nqp::bindattr_i($truncated,$what,'$!month',1),
                                nqp::unless(
                                  nqp::eqat($unit,'year',0),
                                  die "Cannot truncate {self.^name} object to '$unit'"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        );

        $truncated
    }
    method whole-second(DateTime:D: --> Int:D) { $!second.Int }

    method in-timezone(DateTime:D: Int(Cool) $timezone --> DateTime:D) {
        if $timezone == $!timezone {
            self
        }
        else {
            my int $old-offset = self.offset;
            my int $new-offset = $timezone;
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
    }

    method utc(  DateTime:D: --> DateTime:D) { self.in-timezone(0)    }
    method local(DateTime:D: --> DateTime:D) { self.in-timezone($*TZ) }

    proto method Date() {*}
    multi method Date(DateTime:D: --> Date:D) { Date.new($!year,$!month,$!day) }
    multi method Date(DateTime:U: --> Date:U) { Date }
    method DateTime() { self }

    multi method raku(DateTime:D: --> Str:D) {
        self.^name
          ~ ".new($!year,$!month,$!day,$!hour,$!minute,$!second"
          ~ (',' ~ :$!timezone.raku if $!timezone)
          ~ ')'
    }
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
multi sub infix:«<=>»(DateTime:D \a, DateTime:D \b) {
    a.Instant <=> b.Instant
}
multi sub infix:«cmp»(DateTime:D \a, DateTime:D \b) {
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
multi sub infix:<eqv>(DateTime:D \a, DateTime:D \b --> Bool:D) {
    nqp::hllbool(
          nqp::eqaddr(nqp::decont(a),nqp::decont(b))
      || (nqp::eqaddr(a.WHAT,b.WHAT) && a == b)
    )
}

# vim: expandtab shiftwidth=4
