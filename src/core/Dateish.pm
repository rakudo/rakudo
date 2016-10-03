my role Dateish {
    has Int $.year;
    has Int $.month;     # should be int
    has Int $.day;       # should be int
    has Int $.daycount;
    has     &.formatter;

    method IO(Dateish:D: |c) { IO::Path.new(~self) }  # because Dateish is not Cool

    # this sub is also used by !DAYS-IN-MONTH, which is used by other types
    sub IS-LEAP-YEAR($y) { $y %% 4 and not $y %% 100 or $y %% 400 }
    method is-leap-year(Dateish:D:) { IS-LEAP-YEAR($!year) }

    my $days-in-month := nqp::list_i(
      0, 31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    );
    # This method is used by Date and DateTime:
    method !DAYS-IN-MONTH(\year, \month) {
        nqp::atpos_i($days-in-month,month) ||
          ( month == 2 ?? 28 + IS-LEAP-YEAR(year) !! Nil );
    }
    method days-in-month(Dateish:D:) { self!DAYS-IN-MONTH($!year,$!month) }

    method !year-Str() {
        sprintf 0 <= $!year <= 9999 ?? '%04d' !! '%+05d', $!year;
    }

    multi method new(Dateish:) {
        Failure.new(
            "Cannot call {self.^name}.new with "
                ~ (%_ ?? "these named parameters: {%_.keys}" !! "no parameters")
        )
    }

    multi method Str(Dateish:D:) {
        &!formatter ?? &!formatter(self) !! self!formatter
    }
    multi method gist(Dateish:D:) { self.Str }

    method daycount() {
        $!daycount //= do {
            # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
            my int $m = $!month < 3 ?? $!month + 12 !! $!month;
            my int $y = $!year - ($!month < 3);
            -678973 + $!day + (153 * $m - 2) div 5
              + 365 * $y + $y div 4
              - $y div 100  + $y div 400;
        }
    }

    method !ymd-from-daycount($daycount,\year,\month,\day --> Nil) {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        my Int $dc = $daycount.Int + 678881;
        my Int $ti = (4 * ($dc + 36525)) div 146097 - 1;
        my Int $year = 100 * $ti;
        my int $day = $dc - (36524 * $ti + ($ti div 4));
        my int $t = (4 * ($day + 366)) div 1461 - 1;
        year = $year + $t;
        $day  = $day - (365 * $t + ($t div 4));
        my int $month = (5 * $day + 2) div 153;
        day = $day - ((2 + $month * 153) div 5 - 1);
        if ($month > 9) {
            month = $month - 9;
            year  = year + 1;
        }
        else {
            month = $month + 3;
        }
    }

    method day-of-month() { $!day }
    method day-of-week(Dateish:D:) { (self.daycount + 2) % 7 + 1 }

    method week() { # algorithm from Claus Tøndering
        my int $a = $!year - ($!month <= 2).floor.Int;
        my int $b = $a div 4 - $a div 100 + $a div 400;
        my int $c = ($a - 1) div 4 - ($a - 1) div 100 + ($a - 1) div 400;
        my int $s = $b - $c;
        my int $e = $!month <= 2 ?? 0 !! $s + 1;
        my int $f = $!day
          + ($!month <= 2
              ?? 31*($!month - 1) - 1
              !! (153*($!month - 3) + 2) div 5 + 58 + $s);

        my int $g = ($a + $b) % 7;
        my int $d = ($f + $g - $e) % 7;
        my int $n = $f + 3 - $d;

           $n < 0        ?? ($!year - 1, 53 - ($g - $s) div 5)
        !! $n > 364 + $s ?? ($!year + 1, 1                   )
        !!                  ($!year,     $n div 7 + 1        );
    }
    method week-year()   { self.week.AT-POS(0) }
    method week-number() { self.week.AT-POS(1) }

    method weekday-of-month {
        ($!day - 1) div 7 + 1
    }

    my $days-at-start-of-month := nqp::list_i(
      0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
    );
    method day-of-year() {
        $!day
          + nqp::atpos_i($days-at-start-of-month,$!month)
          + ($!month > 2 && IS-LEAP-YEAR($!year));
    }

    method yyyy-mm-dd() { sprintf '%04d-%02d-%02d',$!year,$!month,$!day }

    method earlier(*%unit) { self.later(:earlier, |%unit) }

    method !truncate-ymd(Cool:D $unit, %parts? is copy) {
        if $unit eq 'week' | 'weeks' {
            my $new-dc = self.daycount - self.day-of-week + 1;
            self!ymd-from-daycount($new-dc,
              %parts<year>,%parts<month>,%parts<day>);
        }
        else { # $unit eq 'month' | 'months' | 'year' | 'years'
            %parts<day>   = 1;
            %parts<month> = 1 if $unit eq 'year' | 'years';
        }
        %parts;
    }
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
