use v6;

subset DateTime::Formatter where { .can( all<fmt-datetime fmt-ymd fmt-hms> )};
subset DateTime::Parser    where { .can( all<parse-datetime parse-ymd parse-hms> )};

# RAKUDO: When we have anonymous classes, we don't need to do it like this
class DefaultFormatter {
    has $.date-sep is rw = '-';
    has $.time-sep is rw = ':';

    method fmt-datetime($dt) { # should be typed 'DateTime'
        $dt.iso8601();
    }

    method fmt-ymd($dt) {
        $dt.year.fmt('%04d') ~ $.date-sep ~
        $dt.month.fmt('%02d') ~ $.date-sep ~
        $dt.day.fmt('%02d');
    }

    method fmt-hms($dt) {
        $dt.hour.fmt('%02d') ~ $.time-sep ~
        $dt.minute.fmt('%02d') ~ $.time-sep ~
        $dt.second.fmt('%02d');
    }
}

class DateTime {

    has Int $.year;
    has Int $.month     = 1;
    has Int $.day       = 1;
    has Int $.hour      = 0;
    has Int $.minute    = 0;
    has Num $.second    = 0.0;
    has     $.timezone = '+0000';

    has DateTime::Formatter $!formatter; # = DefaultFormatter.new;
                                         # does not seem to work

    method assert-valid-time($hour, $minute, $second) {
        die 'Invalid time: hour < 0'     if $hour < 0;
        die 'Invalid time: hour > 23'    if $hour > 23;
        die 'Invalid time: minute < 0'   if $minute < 0;
        die 'Invalid time: minute > 59'  if $minute > 59;
        die 'Invalid time: second < 0'   if $second < 0;
        die 'Invalid time: second > 59'  if $second > 59;
    }

    multi method new(:$year!, Bool :$noassert=Bool::False, :$formatter=DefaultFormatter.new, *%_) {
        if !$noassert {
            ::Date.assert-valid-date($year, %_<month> // 1, %_<day> // 1);
            self.assert-valid-time(%_<hour> // 0, %_<minute> // 0, %_<second> // 0);
        }
        self.bless(*, :$year, :$formatter, |%_);
    }

    multi method new(Str $format, :$formatter=DefaultFormatter.new) {
        if $format ~~ /^(\d**4)'-'(\d\d)'-'(\d\d)T(\d\d)':'(\d\d)':'(\d\d)(<[\-\+]>\d**4)$/ {
            my $year      = +$0;
            my $month     = +$1;
            my $day       = +$2;
            my $hour      = +$3;
            my $minute    = +$4;
            my $second    = +$5;
            my $timezone = ~$6;
            self.new(
                :year($year.Int), :month($month.Int), :day($day.Int), 
                :hour($hour.Int), :minute($minute.Int), :second($second.Int), 
                :$timezone, :$formatter, :noassert(Bool::False)
            );
        }
        else {
            die "DateTime.new(Str) expects an ISO8601 string\n";
        }
    }

    multi method from-epoch($epoch, :$timezone='+0000', :$formatter=DefaultFormatter.new) {
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
                 :$timezone, :$formatter, :noassert);
    }

    multi method to-epoch {
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
        self.from-epoch(
            time(),
            :timezone('+0000'),
            :formatter(DefaultFormatter.new)
        );
    }

    multi method ymd() {
        $!formatter.fmt-ymd(self);
    }

    multi method hms() {
        $!formatter.fmt-hms(self);
    }

    method iso8601() {
        # This should be the only formatting not done by the formatter
        $.year.fmt(  '%04d') ~ '-' ~ $.month.fmt( '%02d') ~ '-' ~
        $.day.fmt(   '%02d') ~ 'T' ~ $.hour.fmt(  '%02d') ~ ':' ~
        $.minute.fmt('%02d') ~ ':' ~ $.second.fmt('%02d') ~ $.timezone;
    }

    method Str() {
        $!formatter.fmt-datetime(self);
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
               :$timezone, :$formatter) {
        # Do this first so that the other nameds have a chance to
        # override.
        if defined $timezone {
            # First attempt. Probably wrong.
            # Confirmed, this does NOT work. TODO: FIXME: Make it work.
            # Notes: The Timezone is in HHMM format. We must parse that
            # in order to figure out what timezone shift to use.
            #my $difference = $timezone - $!timezone;
            #$!hour += $difference;
            $!timezone = $timezone;
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
    method set-year($year)             { self.set(:$year) }
    method set-month($month)           { self.set(:$month) }
    method set-day($day)               { self.set(:$day) }
    method set-hour($hour)             { self.set(:$hour) }
    method set-minute($minute)         { self.set(:$minute) }
    method set-second($second)         { self.set(:$second) }
    method set-timezone($timezone)   { self.set(:$timezone) }
    method set-formatter($formatter)   { self.set(:$formatter) }

    method Date() {
        return ::Date.new(self);
    }

    multi method perl() {
        "DateTime.new('" ~ self.iso8601 ~ "')";
    }


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

