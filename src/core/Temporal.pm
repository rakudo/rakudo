subset DateTime::Formatter where { .can('format_datetime') };

# RAKUDO: When we have anonymous classes, we don't need to do it like this
class DefaultFormatter {
    method format_datetime($dt) { # should be typed 'DateTime'
        $dt.iso8601();
    }
}

class DateTime {
    has $.year;
    has $.month       = 1;
    has $.day         = 1;
    has $.hour        = 0;
    has $.minute      = 0;
    has $.second      = 0.0;

    has $.time_zone   = '+0000';

    has DateTime::Formatter $!formatter; # = DefaultFormatter.new;

    multi method new(:$year!, *%_) {
        self.bless(*, :$year, :formatter(DefaultFormatter.new), |%_);
    }

    multi method new(Str $format) {
        die "ISO8601 format constructor not implemented yet";
    }

    multi method from_epoch($epoch, :$timezone, :$formatter) {
        my $time = floor($epoch);
        my $fracsecond = $epoch - $time;
        my $second  = $time % 60; $time = $time div 60;
        my $minute  = $time % 60; $time = $time div 60;
        my $hour    = $time % 24; $time = $time div 24;
        $second += $fracsecond;
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
        self.new(:$year, :$month, :$day,
                 :$hour, :$minute, :$second,
                 :$timezone, :$formatter);
    }

    multi method to_epoch {
        my ( $a, $y, $m, $jd ); # algorithm from Claus Tøndering
        $jd = $.day + floor((153 * $m + 2) / 5) + 365 * $y
            + floor( $y / 4 ) - floor( $y / 100 ) + floor( $y / 400 ) - 32045;
        $a = (14 - $.month) div 12;
        $y = $.year + 4800 - $a;
        $m = $.month + 12 * $a - 3;
        $jd = $.day + (153 * $m + 2) div 5 + 365 * $y
            + $y div 4 - $y div 100 + $y div 400 - 32045;
        return ($jd - 2440588) * 24 * 60 * 60
               + ($.hour*60 + $.minute)*60 + $.second;
    }

    multi method now() {
        self.from_epoch(
            time(),
            :timezone('+0000'),
            :formatter(DefaultFormatter.new)
        );
    }

    multi method ymd($sep = '-') {
        $!year ~ $sep ~ ($!month, $!day).fmt('%02d', $sep);
    }

    multi method hms($sep = ':') {
        ($!hour, $!minute, $!second).fmt('%02d', $sep);
    }

    method iso8601() {
        self.ymd ~ 'T' ~ self.hms ~ $!time_zone;
    }

    method Str() {
        $!formatter.format_datetime(self);
    }

    multi method truncate($unit) {
        die 'Unknown truncation unit'
            if $unit eq none(<second minute hour day month>);
        given $unit {
            when 'second'     {}
            $!second = 0;
            when 'minute'     {}
            $!minute = 0;
            when 'hour'       {}
            $!hour = 0;
            when 'day'        {}
            $!day = 1;
            when 'month'      {}
            $!month = 1;
        }
    }

    multi method today() {
        self.now().truncate('day');
    }

    multi method day-of-week { # returns DayOfWeek {
        my ( $a, $y, $m, $jd ); # algorithm from Claus Tøndering
        $a = (14 - $.month) div 12;
        $y = $.year + 4800 - $a;
        $m = $.month + 12 * $a - 3;
        $jd = $.day + (153 * $m + 2) div 5 + 365 * $y + $y div 4
              - $y div 100 +$y div 400 - 32045;
        return ($jd + 1) % 7 + 1;
    }

    multi method month-name {
        return <January February March April May June July August
                September October November December>[$.month-1];
    }

    multi method day-name {
        return <Sunday Monday Tuesday Wednesday Thursday Friday
                Saturday>[self.day-of-week-1];
    }

    method set(:$year, :$month, :$day,
               :$hour, :$minute, :$second,
               :$time_zone, :$formatter) {
        # Do this first so that the other nameds have a chance to
        # override.
        if defined $time_zone {
            # First attempt. Probably wrong.
            my $difference = $time_zone - $!time_zone;
            $!hour += $difference;
            $!time_zone = $time_zone;
        }

        $!year       = $year       // $!year;
        $!month      = $month      // $!month;
        $!day        = $day        // $!day;
        $!hour       = $hour       // $!hour;
        $!minute     = $minute     // $!minute;
        $!second     = $second     // $!second;
        $!formatter  = $formatter  // $!formatter;
    }

    # RAKUDO: These setters are temporary, until we have Proxy
    #         objects with a STORE method
    method set_year($year)             { self.set(:$year) }
    method set_month($month)           { self.set(:$month) }
    method set_day($day)               { self.set(:$day) }
    method set_hour($hour)             { self.set(:$hour) }
    method set_minute($minute)         { self.set(:$minute) }
    method set_second($second)         { self.set(:$second) }
    method set_time_zone($time_zone)   { self.set(:$time_zone) }
    method set_formatter($formatter)   { self.set(:$formatter) }
}

=begin pod
 
=head1 SEE ALSO
Perl 6 spec <S32-Temporal|http://perlcabal.org/syn/S32/Temporal.html>.
Perl 5 perldoc L<doc:DateTime> and L<doc:Time::Local>.
 
The best yet seen explanation of calendars, by Claus Tøndering
L<Calendar FAQ|http://www.tondering.dk/claus/calendar.html>.
Similar algorithms at L<http://www.hermetic.ch/cal_stud/jdn.htm>
and L<http://www.merlyn.demon.co.uk/daycount.htm>.
 
<ISO 8601|http://en.wikipedia.org/wiki/ISO_8601>
<Time zones|http://en.wikipedia.org/wiki/List_of_time_zones>
 
=end pod

