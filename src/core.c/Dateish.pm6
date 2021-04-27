my role Dateish {
    has int $.year;
    has int $.month;
    has int $.day;
    has int $.daycount;
    has     &.formatter;

    method IO(Dateish:D: --> IO::Path:D) {  # because Dateish is not Cool
        IO::Path.new(~self)
    }

    method CALL-ME(Dateish:U: \dateish) { self.new(dateish) }

    # this sub is also used by DAYS-IN-MONTH, which is used by other types
    sub IS-LEAP-YEAR(int $y --> Bool:D) {
        $y %% 4 and not $y %% 100 or $y %% 400
    }
    method is-leap-year(Dateish:D: --> Bool:D) { IS-LEAP-YEAR($!year) }

    my constant $days-in-month = nqp::list_i(
      0, 31, 0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    );
    # This method is used by Date and DateTime:
    method !DAYS-IN-MONTH(\year, \month --> Int:D) {
        nqp::atpos_i($days-in-month,month) ||
          ( month == 2 ?? 28 + IS-LEAP-YEAR(year) !! Nil );
    }
    method days-in-month(Dateish:D: --> Int:D) {
        self!DAYS-IN-MONTH($!year,$!month)
    }

    method !year-Str(--> Str:D) {
        sprintf 0 <= $!year <= 9999 ?? '%04d' !! '%+05d', $!year;
    }

    # noop for subclasses
    method !SET-DAYCOUNT() { self }

    # shortcut for out of range throwing
    method !oor($what, $got, $range) {
        X::OutOfRange.new(:$what, :$got, :$range).throw
    }

    # shortcut for invalid format throwing
    method !tif($invalid-str, $target, $format) {
        X::Temporal::InvalidFormat.new(:$invalid-str,:$target,:$format).throw
    }

    multi method new(Dateish:) {
        Failure.new(
            "Cannot call {self.^name}.new with "
                ~ (%_ ?? "these named parameters: {%_.keys}" !! "no parameters")
        )
    }

    multi method Str(Dateish:D: --> Str:D) {
        &!formatter ?? &!formatter(self) !! self!formatter
    }
    multi method gist(Dateish:D: --> Str:D) { self.Str }

    method daycount(--> Int:D) {
        $!daycount
          ?? $!daycount
          !! ($!daycount = self!calculate-daycount)
    }
    method !calculate-daycount() {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        my int $y = $!year;
        my int $m = $!month;
        nqp::if(
          nqp::islt_i($m,3),
          nqp::stmts(
            ($y = nqp::sub_i($y,1)),
            ($m = nqp::add_i($m,12))
          )
        );

        -678973 + $!day + (153 * $m - 2) div 5
          + 365 * $y + $y div 4
          - $y div 100  + $y div 400
    }

    method !ymd-from-daycount(int $daycount, \year, \month, \day --> Nil) {
        # taken from <http://www.merlyn.demon.co.uk/daycount.htm>
        my Int $dc = $daycount + 678881;
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

    method day-of-month(--> Int:D) { $!day }
    method day-of-week(Dateish:D:--> Int:D) { (self.daycount + 2) % 7 + 1 }

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
    method week-year(--> Int:D)   { self.week.AT-POS(0) }
    method week-number(--> Int:D) { self.week.AT-POS(1) }

    method weekday-of-month(--> Int:D) {
        ($!day - 1) div 7 + 1
    }

    my constant $days-at-start-of-month = nqp::list_i(
      0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
    );
    method day-of-year(--> Int:D) {
        $!day
          + nqp::atpos_i($days-at-start-of-month,$!month)
          + ($!month > 2 && IS-LEAP-YEAR($!year));
    }

    method yyyy-mm-dd(str $sep = "-" --> Str:D) {
        my $parts := nqp::list_s;
        nqp::push_s($parts, $!year < 1000 || $!year > 9999
          ?? self!year-Str
          !! nqp::tostr_I(nqp::getattr_i(self,$?CLASS,'$!year'))
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!month,10)),$!month)
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!day,10)),$!day)
        );
        nqp::join($sep,$parts)
    }

    method dd-mm-yyyy(str $sep = "-" --> Str:D) {
        my $parts := nqp::list_s;
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!day,10)),$!day)
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!month,10)),$!month)
        );
        nqp::push_s($parts, $!year < 1000 || $!year > 9999
          ?? self!year-Str
          !! nqp::tostr_I(nqp::getattr_i(self,$?CLASS,'$!year'))
        );
        nqp::join($sep,$parts)
    }

    method mm-dd-yyyy(str $sep = "-" --> Str:D) {
        my $parts := nqp::list_s;
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!month,10)),$!month)
        );
        nqp::push_s($parts,
          nqp::concat(nqp::x('0',nqp::islt_i($!day,10)),$!day)
        );
        nqp::push_s($parts, $!year < 1000 || $!year > 9999
          ?? self!year-Str
          !! nqp::tostr_I(nqp::getattr_i(self,$?CLASS,'$!year'))
        );
        nqp::join($sep,$parts)
    }

    proto method earlier(|) {*}
    multi method earlier(Dateish:D: *%unit --> Dateish:D) {
        my $units := nqp::getattr(%unit,Map,'$!storage');
        nqp::iseq_i(nqp::elems($units),1)
          ?? self.move-by-unit(
               (my str $u = nqp::iterkey_s(nqp::shift(nqp::iterator($units)))),
               -nqp::atkey($units,$u)  # must be HLL negation
             )
          !! self!move-die(nqp::elems($units))
    }
    multi method earlier(Dateish:D: @pairs) {
        my $dateish  := self;
        my int $elems = @pairs.elems;   # reifies
        my $reified  := nqp::getattr(@pairs,List,'$!reified');
        my int $i     = -1;
        nqp::while(
          nqp::bitand_i(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::istype((my $pair := nqp::decont(nqp::atpos($reified,$i))),Pair)
          ),
          $dateish := $dateish.move-by-unit(
            nqp::getattr($pair,Pair,'$!key'),
            -nqp::getattr($pair,Pair,'$!value')  # must be HLL negation
          )
        );

        nqp::islt_i($i,$elems)
          ?? (die "Can not use a {$pair.raku} as a time unit")
          !! $dateish
    }

    proto method later(|) {*}
    multi method later(Dateish:D: *%unit --> Dateish:D) {
        my $units := nqp::getattr(%unit,Map,'$!storage');
        nqp::iseq_i(nqp::elems($units),1)
          ?? self.move-by-unit(
               (my str $u = nqp::iterkey_s(nqp::shift(nqp::iterator($units)))),
               nqp::atkey($units,$u)
             )
          !! self!move-die(nqp::elems($units))
    }
    multi method later(Dateish:D: @pairs) {
        my $dateish  := self;
        my int $elems = @pairs.elems;   # reifies
        my $reified  := nqp::getattr(@pairs,List,'$!reified');
        my int $i     = -1;
        nqp::while(
          nqp::bitand_i(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::istype((my $pair := nqp::decont(nqp::atpos($reified,$i))),Pair)
          ),
          $dateish := $dateish.move-by-unit(
            nqp::getattr($pair,Pair,'$!key'),
            nqp::getattr($pair,Pair,'$!value')
          )
        );

        nqp::islt_i($i,$elems)
          ?? (die "Can not use a {$pair.raku} as a time unit")
          !! $dateish
    }

    # die for improper number of units when moving a Dateish
    method !move-die(int $elems) {
        die $elems
          ?? "More than one time unit supplied. Please provide these as a List of Pairs to indicate order of application if this is intended.".naive-word-wrapper
          !! die "No time unit supplied";
    }
}

# =begin pod
#
# =head1 SEE ALSO
# Raku spec <S32-Temporal|https://design.raku.org/S32/Temporal.html>.
# The Perl DateTime Project home page L<http://datetime.perl.org>.
# Perl perldoc L<doc:DateTime> and L<doc:Time::Local>.
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

# vim: expandtab shiftwidth=4
