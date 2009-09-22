# Not Yet Implemented
#enum dayOfWeek <Sunday Monday Tuesday Wednesday Thursday Friday Saturday>;
#enum DayOfWeek <Sunday Monday Tuesday Wednesday Thursday Friday Saturday>;

my subset Month     of Int where { 1 <= $^a <= 12 };
my subset Day       of Int where { 1 <= $^a <= 31 };
my subset DayOfWeek of Int where { 1 <= $^a <=  7 };
my subset Hour      of Int where { 0 <= $^a <= 23 };
my subset Minute    of Int where { 0 <= $^a <= 59 };
my subset Second    of Num where { 0 <= $^a <= 60 };

role Temporal::Date {
    ## XXX Rakudo bug - can not use lexical subset types in a class yet
    has Int    $.year;
    has #`(Month)  $.month = 1;
    has #`(Day)    $.day = 1;

    method day-of-week { # returns DayOfWeek {
        my ( $a, $y, $m, $jd );         # algorithm from Claus Tøndering
        $a = (14 - $.month) div 12;
        $y = $.year + 4800 - $a;
        $m = $.month + 12 * $a - 3;
        $jd = $.day + (153 * $m + 2) div 5 + 365 * $y + $y div 4
              - $y div 100 +$y div 400 - 32045;
        return ($jd + 1) % 7 + 1;
    }

    our Str method month-name {
        return <January February March April May June July August
                September October November December>[$.month-1];
    }

    our Str method day-name {
        return <Sunday Monday Tuesday Wednesday Thursday Friday
                Saturday>[self.day-of-week-1];
    }

    our Str method iso8601 {
        given self {
            return sprintf '%04d-%02d-%02d', .year, .month, .day;
        }
    }

    method Str { self.iso8601 };

    sub infix:{'<=>'}( Temporal::Date $left, Temporal::Date $right )
        is export   # would like to define it with «<=>»
    {
        $left.year  <=> $right.year
        ||
        $left.month <=> $right.month
        ||
        $left.day   <=> $right.day;
    }

}

role Temporal::Time {
    ## XXX Rakudo bug - can not use lexical subset types in a class yet
    has #`(Hour)   $.hour = 0;
    has #`(Minute) $.minute = 0;
    has #`(Second) $.second = 0;

    our Str method iso8601 {
        given self {
            return sprintf '%02d:%02d:%02d', .hour, .minute, .second;
        }
    }

    method Str { self.iso8601; }

    sub infix:{'<=>'}( Temporal::Time $left, Temporal::Time $right )
        is export   # would like to define it with «<=>»
    {
        $left.hour   <=> $right.hour
        ||
        $left.minute <=> $right.minute
        ||
        $left.second <=> $right.second;
    }

}

role Temporal::TimeZone::Observance {
    subset Offset of Int where { -86400 < $^a < 86400 };
    has Offset $.offset;
    has Bool   $.isdst;
    has Str    $.abbreviation; # UTC, CST, AST

    # The ISO8601 standard does not allow for offsets with sub-minute
    # resolutions. In real-world practice, this is not an issue.
    our Str method iso8601 {
        sprintf "%+03d%02d", self.offset div 3600,
                (abs(self.offset) div 60 ) % 60;
    }

    method Str { self.iso8601 }
}


role Temporal::DateTime {
    has Temporal::Date $.date;
    has Temporal::Time $.time;
    has Temporal::TimeZone::Observance $.timezone;
    # TODO: replace the three above with the three below somehow fixed,
    # and then revise the tests accordingly
#   has Temporal::Date $!date handles <year month day day-of-week>;
#   has Temporal::Time $!time handles <hour minute second fractional-second>;
#   has Temporal::TimeZone::Observance $!timezone handles <offset isdst>;

    our Str method iso8601 {
        self.date.iso8601 ~ 'T' ~ self.time.iso8601 ~ self.timezone.iso8601;
    }

    method Str { self.iso8601 }

    # This involves a whole bunch of code - see Perl 5's Time::Local
    our Num method epoch {
        my ( $a, $y, $m, $jd );         # algorithm from Claus Tøndering
        $jd = $.date.day + floor((153 * $m + 2) / 5) + 365 * $y
            + floor( $y / 4 ) - floor( $y / 100 ) + floor( $y / 400 ) - 32045;
        $a = (14 - $.date.month) div 12;
        $y = $.date.year + 4800 - $a;
        $m = $.date.month + 12 * $a - 3;
        $jd = $.date.day + (153 * $m + 2) div 5 + 365 * $y
            + $y div 4 - $y div 100 + $y div 400  - 32045;
        return ($jd - 2440588) * 24 * 60 * 60
               + ($.time.hour*60 + $.time.minute)*60 + $.time.second;
    }

    method Int { self.epoch.truncate }

    method Num { self.epoch }
}

class Time {

    our method gmtime( Num $epoch = time ) {
        my ( $time, $second, $minute, $hour, $day, $month, $year );
        $time = floor( $epoch );
        $second  = $time % 60; $time = $time div 60;
        $minute  = $time % 60; $time = $time div 60;
        $hour    = $time % 24; $time = $time div 24;
        # Day month and leap year arithmetic, based on Gregorian day #.
        # 2000-01-01 noon UTC == 2451558.0 Julian == 2451545.0 Gregorian
        $time += 2440588;   # because 2000-01-01 == Unix epoch day 10957
        my $a = $time + 32044;     # date algorithm from Claus Tøndering
        my $b = (4 * $a + 3) div 146097; # 146097 = days in 400 years
        my $c = $a - ( 146097 * $b ) div 4;
        my $d = (4 * $c + 3) div 1461;       # 1461 = days in 4 years
        my $e = $c - ($d * 1461) div 4;
        my $m = (5 * $e + 2) div 153; # 153 = days in Mar-Jul Aug-Dec
        $day   = $e - (153 * $m + 2) div 5 + 1;
        $month = $m + 3 - 12 * ( $m div 10 );
        $year  = $b * 100 + $d - 4800 + $m div 10;
        Temporal::DateTime.new(
            date => Temporal::Date.new(:$year, :$month, :$day),
            time => Temporal::Time.new(:$hour, :$minute, :$second),
            timezone => Temporal::TimeZone::Observance.new(
                offset=>0, isdst=>Bool::False, abbreviation=>'UTC' )
        );
    }
#   Not clear what spec S32-Temporal really means here...
#   multi sub localtime( :$time = time(), :$tz=<GMT> ) is export { ... } # NYI
#   multi sub localtime( Num $epoch = time() ) returns Temporal::DateTime { ... } # NYI
#   our Num sub time() {  ...  } # NYI
}

=begin pod

=head1 SEE ALSO
Perl 6 spec <S32-Temporal|http://perlcabal.org/syn/S32/Temporal.html>.
Perl 5 perldoc L<doc:Time::Local>.

The best yet seen explanation of calendars, by Claus Tøndering
L<Calendar FAQ|http://www.tondering.dk/claus/calendar.html>.
Similar algorithms at L<http://www.hermetic.ch/cal_stud/jdn.htm>
and L<http://www.merlyn.demon.co.uk/daycount.htm>.

<ISO 8601|http://en.wikipedia.org/wiki/ISO_8601>
<Time zones|http://en.wikipedia.org/wiki/List_of_time_zones>

=end pod
