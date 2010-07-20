use v6;

class Dateish {
    has Int $.year;
    has Int $.month = 1;
    has Int $.day = 1;

    multi method is-leap-year($y = $!year) {
        $y %% 4 and not $y %% 100 or $y %% 400
    }

    multi method days-in-month($year = $!year, $month = $!month) {
           $month == 2        ?? self.is-leap-year($year) ?? 29 !! 28
        !! $month == 4|6|9|11 ?? 30
        !! 31
    }

    method daycount-from-ymd($y is copy, $m is copy, $d) {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        $y .= Int;
        $m .= Int; 
        if ($m < 3) {
            $m += 12;
            --$y;
        }
        -678973 + $d + (153 * $m - 2) div 5
            + 365 * $y + $y div 4
            - $y div 100  + $y div 400;
    }

    method set-ymd-from-daycount($daycount) {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        $!day = $daycount.Int + 678881;
        my $t = (4 * ($!day + 36525)) div 146097 - 1;
        $!year = 100 * $t;
        $!day -= 36524 * $t + ($t +> 2);
        $t = (4 * ($!day + 366)) div 1461 - 1;
        $!year += $t;
        $!day -= 365 * $t + ($t +> 2);
        $!month = (5 * $!day + 2) div 153;
        $!day -= (2 + $!month * 153) div 5 - 1;
        if ($!month > 9) {
            $!month -= 12;
            $!year++;
        }
        $!month += 3;
    }

    multi method get-daycount {
        self.daycount-from-ymd($.year, $.month, $.day)
    }

    method day-of-month() { $.day }
  
    method day-of-week($daycount = self.get-daycount) {
        ($daycount + 2) % 7 + 1
    }

    multi method week() { # algorithm from Claus Tøndering
        my $a = $.year - ($.month <= 2).floor;
        my $b = $a div 4 - $a div 100 + $a div 400;
        my $c = ($a - 1) div 4 - ($a - 1) div 100 + ($a - 1) div 400;
        my $s = $b - $c;
        my $e = $.month <= 2 ?? 0 !! $s + 1;
        my $f = $.day + do $.month <= 2
         ?? 31*($.month - 1) - 1
         !! (153*($.month - 3) + 2) div 5 + 58 + $s;

        my $g = ($a + $b) % 7;
        my $d = ($f + $g - $e) % 7;
        my $n = $f + 3 - $d;

           $n < 0           ?? ($.year - 1, 53 - ($g - $s) div 5)
        !! $n > 364 + $s    ?? ($.year + 1, 1)
        !!                     ($.year,     $n div 7 + 1);
    }

    multi method week-year() {
        self.week.[0]
    }

    multi method week-number() {
        self.week.[1]
    }

    multi method weekday-of-month {
        ($.day - 1) div 7 + 1
    }

    multi method day-of-year() {
        [+] $.day, map { self.days-in-month($.year, $^m) }, 1 ..^ $.month
    }

    method try-assignment($lvalue is rw, $rvalue, $name, $range) {
        $rvalue.defined or return;
        +$rvalue ~~ $range or
            or die "$name must be in {$range.perl}\n";
        $lvalue = +$rvalue;
    }
  
    method try-setting-date(:$year, :$month, :$day) {
        $year.defined and $!year = +$year;
        self.try-assignment($!month, $month, 'month', 1 .. 12);
        self.try-assignment($!day, $day, "day of $!year/$!month",
            1 .. self.days-in-month);
     }

}

sub default-formatter(::DateTime $dt) {
# ISO 8601 timestamp
    my $o = $dt.offset;
    $o %% 60
        or warn "Default DateTime formatter: offset $o not divisible by 60.\n";
    sprintf '%04d-%02d-%02dT%02d:%02d:%02d%s',
        $dt.year, $dt.month, $dt.day,
        $dt.hour, $dt.minute, $dt.whole-second,
        do $o
         ?? sprintf '%s%02d%02d',
                $o < 0 ?? '-' !! '+',
                ($o.abs / 60 / 60).floor,
                ($o.abs / 60 % 60).floor
         !! 'Z'
}

class DateTime is Dateish {
    has Int $.hour      = 0;
    has Int $.minute    = 0;
    has     $.second    = 0.0;
    has     $.timezone  = 0; # UTC
    has     &.formatter; # = &default-formatter; # Doesn't work (not in scope?).

    multi method new(Int :$year!, :$timezone=0, :&formatter=&default-formatter, *%_) {
        # Rather than directly constructing the object we want,
        # take advantage of the validation login in DateTime.set.
        # But don't let $timezone get to .set, or it might change
        # the hour, etc.
        my $dt = self.bless(*, :$year, :$timezone, :&formatter);
        $dt.set(|%_);
        $dt;
    }

    multi method new(::Date :$date!, *%_) {
        self.new(year => $date.year, month => $date.month,
            day => $date.day, |%_)
    }

    # TODO: multi method new(Instant $i, ...) { ... }

    multi method new(Int $time is copy, :$timezone, :&formatter=&default-formatter) {
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
        my $dt = self.new(:$year, :$month, :$day,
            :$hour, :$minute, :$second, :&formatter);
        $timezone.defined and $dt.set(timezone => $timezone);
        $dt;
    }

    multi method new(Str $format, :$timezone is copy = 0, :&formatter=&default-formatter) {
        $format ~~ /^ (\d**4) '-' (\d\d) '-' (\d\d) T (\d\d) ':' (\d\d) ':' (\d\d) (Z || (<[\-\+]>) (\d\d)(\d\d))? $/
            or die "DateTime.new(Str) expects an ISO 8601 string\n";
        my $year   = (+$0).Int;
        my $month  = (+$1).Int;
        my $day    = (+$2).Int;
        my $hour   = (+$3).Int;
        my $minute = (+$4).Int;
        my $second = +$5;
        if $6 {
            $timezone
                and die "DateTime.new(Str): :timezone argument not allowed with a timestamp offset";
            if $6 eq 'Z' {
                $timezone = 0;                
            } else {
                $timezone = (($6[0][1]*60 + $6[0][2]) * 60).Int;
                  # RAKUDO: .Int is needed to avoid to avoid the nasty '-0'.
                $6[0][0] eq '-' and $timezone = -$timezone;
            }
        }
        DateTime.new(:$year, :$month, :$day, :$hour, :$minute,
            :$second, :$timezone, :&formatter);
    }

    multi method now(:$timezone=0, :&formatter=&default-formatter) {
    # FIXME: Default to the user's time zone instead of UTC.
    # FIXME: Include fractional seconds.
        self.new(time, :$timezone, :&formatter)
    }

    # TODO: multi method Instant() { ... }

    multi method posix() {
        self.offset
            and return self.clone.set(timezone => 0).posix;
        # algorithm from Claus Tøndering
        my $a = (14 - $.month.Int) div 12;
        my $y = $.year.Int + 4800 - $a;
        my $m = $.month.Int + 12 * $a - 3;
        my $jd = $.day + (153 * $m + 2) div 5 + 365 * $y
            + $y div 4 - $y div 100 + $y div 400 - 32045;
        ($jd - 2440588) * 24 * 60 * 60
            + 60*(60*$.hour + $.minute) + self.whole-second;
    }

    method offset($tz = $!timezone) {
        $tz ~~ Callable ?? $tz(self, True) !! $tz
    }

    multi method truncate(:$to) {
    # RAKUDO: When supplying positional arguments by name works
    # again, this won't be necessary.
        self.truncate($to);
    }

    multi method truncate($to) {
        die 'Unknown truncation unit'
            if $to eq none(<second minute hour day week month year>);
        given $to {
            $!second .= floor;
            when 'second'     {}
            $!second = 0;
            when 'minute'     {}
            $!minute = 0;
            when 'hour'       {}
            $!hour = 0;
            when 'week' {
                my $dc = self.get-daycount;
                my $new-dc = $dc - self.day-of-week($dc) + 1;
                self.set-ymd-from-daycount($new-dc);
            }
            when 'day'        {}
            $!day = 1;
            when 'month'      {}
            $!month = 1;
        }
        self;
    }

    multi method whole-second() {
        floor $.second
    }

    method set(:$year, :$month, :$day,
               :$hour, :$minute, :$second,
               :$timezone, :&formatter) {
        # Do this first so that the other nameds have a chance to
        # override.
        if defined $timezone and $timezone !eqv $!timezone {
            my $old-offset = self.offset;
            my $new-offset = $timezone ~~ Callable
              ?? $timezone(self.clone.set(timezone => 0), False)
              !! $timezone;
            my $c = $!second + $new-offset - $old-offset;
            $!second = $c % 60;
            my $a = $!minute + floor $c / 60;
            $!minute = $a % 60;
            my $b = $!hour + floor $a / 60;
            $!hour = $b % 24;
            # Let Dateish handle any further rollover.
            floor $b / 24 and self.set-ymd-from-daycount\
               (self.get-daycount + floor $b / 24);
            $!timezone = $timezone;
        }

        self.try-setting-date(:$year, :$month, :$day);
        self.try-assignment($!hour,   $hour,   'hour',   0 ..^ 24);
        self.try-assignment($!minute, $minute, 'minute', 0 ..^ 60);
        self.try-assignment($!second, $second, 'second', 0 ..^ 62);
        &formatter.defined and &!formatter = &formatter;

        self;
    }

    # RAKUDO: These setters are temporary, until we have Proxy
    #         objects with a STORE method
    method set-year($year)             { self.set(:$year) }
    method set-month($month)           { self.set(:$month) }
    method set-day($day)               { self.set(:$day) }
    method set-day-of-month($day)      { self.set(:$day) }
    method set-hour($hour)             { self.set(:$hour) }
    method set-minute($minute)         { self.set(:$minute) }
    method set-second($second)         { self.set(:$second) }
    method set-timezone($timezone)     { self.set(:$timezone) }
    method set-formatter(&formatter)   { self.set(:&formatter) }

    method Date() {
        return ::Date.new(self);
    }

    method Str() {
        &!formatter(self)
    }

    multi method perl() {
        "DateTime.new(year => $.year, month => $.month, day => $.day, " ~
        "hour => $.hour, minute => $.minute, second => $.second, " ~
        "timezone => $.timezone.perl()" ~
        do $.formatter eqv &default-formatter
         ?? ')'
         !! ", formatter => $.formatter.perl())"
    }

}

class Date is Dateish {
    has Int $.daycount;

    method !set-daycount($dc) { $!daycount = $dc }

    multi method get-daycount { $!daycount }

    multi method new(:$year!, :$month, :$day) {
        my $d = self.bless(*, :$year);
        $d.try-setting-date(:$month, :$day);
        $d!set-daycount(self.daycount-from-ymd($year,$month,$day));
        $d;
    }

    multi method new($year, $month, $day) {
        self.new(:$year, :$month, :$day);
    }

    multi method new(Str $date where { $date ~~ /
            ^ <[0..9]>**4 '-' <[0..9]>**2 '-' <[0..9]>**2 $
        /}) {
        self.new(|$date.split('-').map(*.Int));
    }

    multi method new(::DateTime $dt) {
        self.bless(*, 
            :year($dt.year), :month($dt.month), :day($dt.day),
            :daycount(self.daycount-from-ymd($dt.year,$dt.month,$dt.day))
        );
    }

    multi method new-from-daycount($daycount) {
        my $d = self.bless(*, :$daycount);
        $d.set-ymd-from-daycount($daycount);
        $d;
    }

    multi method today() {
        my $dt = ::DateTime.now();
        self.new($dt);
    }

    multi method succ() {
        Date.new-from-daycount($!daycount + 1);
    }
    multi method pred() {
        Date.new-from-daycount($!daycount - 1);
    }

    multi method Str() {
        sprintf '%04d-%02d-%02d', $.year, $.month, $.day;
    }

    multi method perl() {
        "Date.new($.year.perl(), $.month.perl(), $.day.perl())";
    }

}

multi infix:<+>(Date $d, Int $x) is export {
    Date.new-from-daycount($d.daycount + $x)
}
multi infix:<+>(Int $x, Date $d) is export {
    Date.new-from-daycount($d.daycount + $x)
}
multi infix:<->(Date $d, Int $x) is export {
    Date.new-from-daycount($d.daycount - $x)
}
multi infix:<->(Date $a, Date $b) is export {
    $a.daycount - $b.daycount;
}
multi infix:<cmp>(Date $a, Date $b) is export {
    $a.daycount cmp $b.daycount
}
multi infix:«<=>»(Date $a, Date $b) is export {
    $a.daycount <=> $b.daycount
}
multi infix:<==>(Date $a, Date $b) is export {
    $a.daycount == $b.daycount
}
multi infix:<!=>(Date $a, Date $b) is export {
    $a.daycount != $b.daycount
}
multi infix:«<=»(Date $a, Date $b) is export {
    $a.daycount <= $b.daycount
}
multi infix:«<»(Date $a, Date $b) is export {
    $a.daycount < $b.daycount
}
multi infix:«>=»(Date $a, Date $b) is export {
    $a.daycount >= $b.daycount
}
multi infix:«>»(Date $a, Date $b) is export {
    $a.daycount > $b.daycount
}

=begin pod

=head1 SEE ALSO
Perl 6 spec <S32-Temporal|http://perlcabal.org/syn/S32/Temporal.html>.
The Perl 5 DateTime Project home page L<http://datetime.perl.org>.
Perl 5 perldoc L<doc:DateTime> and L<doc:Time::Local>.
 
The best yet seen explanation of calendars, by Claus Tøndering
L<Calendar FAQ|http://www.tondering.dk/claus/calendar.html>.
Similar algorithms at L<http://www.hermetic.ch/cal_stud/jdn.htm>
and L<http://www.merlyn.demon.co.uk/daycount.htm>.
 
<ISO 8601|http://en.wikipedia.org/wiki/ISO_8601>
<Time zones|http://en.wikipedia.org/wiki/List_of_time_zones>

As per the recommendation, the strftime() method has bee moved into a
loadable module called DateTime::strftime.

=end pod
