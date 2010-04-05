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
    has $.second      = 0;
    has $.nanosecond  = 0;

    has $.time_zone   = '+00';

    has DateTime::Formatter $!formatter = DefaultFormatter.new;

    multi method new(:$year!, *%_) {
        self.bless(*, :$year, |%_);
    }

    multi method new(Str $format) {
        die "ISO8601 format constructor not implemented yet";
    }

    multi method from_epoch($epoch, :$timezone, :$formatter) {
        my $time = floor($epoch);
        my $nanosecond = floor(($epoch - $time) * 1e9);
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
        self.new(:$year, :$month, :$day,
                 :$hour, :$minute, :$second, :$nanosecond,
                 :$timezone, :$formatter);
    }

    multi method now() {
        self.from_epoch(time());
    }

    multi method ymd($sep = '-') {
        $!year ~ $sep ~ ($!month, $!day).fmt('%02d', $sep);
    }

    multi method hms($sep = ':') {
        ($!hour, $!minute, $!second).fmt('%02d', $sep);
    }

    multi method datetime() {
        self.ymd ~ 'T' ~ self.hms;
    }

    method iso8601() {
        self.datetime() ~ $!time_zone;
    }

    method Str() {
        $!formatter.format_datetime(self);
    }

    multi method truncate($unit) {
        die 'Unknown truncation unit'
            if $unit eq none(<nanosecond second minute hour day month>);
        given $unit {
            when 'nanosecond' {}
            $!nanosecond = 0;
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

    method set(:$year, :$month, :$day,
               :$hour, :$minute, :$second, :$nanosecond,
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
        $!nanosecond = $nanosecond // $!nanosecond;
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
    method set_nanosecond($nanosecond) { self.set(:$nanosecond) }
    method set_time_zone($time_zone)   { self.set(:$time_zone) }
    method set_formatter($formatter)   { self.set(:$formatter) }
}
