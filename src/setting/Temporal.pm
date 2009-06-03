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
    has Int    $.year;
    has Month  $.month = 1;
    has Day    $.day = 1;

    method day-of-week( ) { # returns DayOfWeek {
        my ( $a, $y, $m, $jd );         # algorithm from Claus Tøndering
        $a = int((14 - $.month) / 12 );
        $y = $.year + 4800 - $a;
        $m = $.month + 12 * $a - 3;
        $jd = $.day + int((153 * $m + 2) / 5) + 365 * $y + int( $y / 4 )
              - int( $y / 100 ) + int( $y / 400 ) - 32045;
        return ($jd + 1) % 7 + 1;
    }

    our Str method month-name( ) {
        return <January February March April May June July August
            September October November December>[$.month-1];
    }

    our Str method day-name( ) {
        return <Sunday Monday Tuesday Wednesday Thursday Friday
                Saturday>[self.day-of-week-1];
    }

    our Str method iso8601() {
        given self {
            return sprintf '%04d-%02d-%02d', .year, .month, .day;
        }
    }

    method Str { self.iso8601 };

    sub infix:{'<=>'}( Temporal::Date $left, Temporal::Date $right )
        is export   # would like to define it with «<=>»
    {
        $left.year <=> $right.year
        ||
        $left.month <=> $right.month
        ||
        $left.day <=> $right.day;
    }

}

role Temporal::Time {
    has Hour   $.hour = 0;
    has Minute $.minute = 0;
    has Second $.second = 0;

    our Str method iso8601() {
        given self {
            return sprintf '%02d:%02d:%02d', .hour, .minute, .second;
        }
    }

    method Str { self.iso8601; }

    sub infix:{'<=>'}( Temporal::Time $left, Temporal::Time $right )
        is export   # would like to define it with «<=>»
    {
        $left.hour <=> $right.hour
        ||
        $left.minute <=> $right.minute
        ||
        $left.second <=> $right.second;
    }

}

role Temporal::DateTime {
    has Temporal::Date $.date;
    has Temporal::Time $.time;
#   has Temporal::Date $!date handles <year month day day-of-week>;
#   has Temporal::Time $!time handles <hour minute second fractional-second>;
#   has Temporal::TimeZone::Observance $!timezone handles <offset isdst>;

    our Str method iso8601 () {
        self.date.iso8601 ~ 'T' ~ self.time.iso8601 ~ self.timezone.iso8601;
    }

    method Str { self.iso8601 }

    # This involves a whole bunch of code - see Perl 5's
    # Time::Local
    our Num method epoch( )
    {
        my ( $a, $y, $m, $jd );         # algorithm from Claus Tøndering
        $a = int((14 - $.date.month) / 12 );
        $y = $.date.year + 4800 - $a;
        $m = $.date.month + 12 * $a - 3;
        $jd = $.date.day + int((153 * $m + 2) / 5) + 365 * $y
            + int( $y / 4 ) - int( $y / 100 ) + int( $y / 400 ) - 32045;
        return $jd - 2440588 
               + ($.time.hour*60 + $.time.minute)*60 + $.time.second;
    }

#   method Int { self.epoch.truncate }

#   method Num { self.epoch }
}

class Time {

    our method gmtime( Num $epoch = time ) {
        my ( $time, $sec, $min, $hour, $mday, $mon, $year );
        $time = int( $epoch );
        $sec  = $time % 60; $time = int($time/60);
        $min  = $time % 60; $time = int($time/60);
        $hour = $time % 24; $time = int($time/24);
        # Day month and leap year arithmetic, based on Gregorian day #.
        # 2000-01-01 noon UTC == 2451558.0 Julian == 2451545.0 Gregorian
        $time += 2440588;   # because 2000-01-01 == Unix epoch day 10957
        my $a = $time + 32044;     # date algorithm from Claus Tøndering
        my $b = int((4 * $a + 3) / 146097); # 146097 = days in 400 years
        my $c = $a - int(( 146097 * $b ) / 4);
        my $d = int((4 * $c + 3) / 1461);       # 1461 = days in 4 years
        my $e = $c - int(($d * 1461) / 4);
        my $m = int((5 * $e + 2) / 153); # 153 = days in Mar-Jul Aug-Dec
        $mday = $e - int((153 * $m + 2) / 5 ) + 1;
        $mon  = $m + 3 - 12 * int( $m / 10 );
        $year = $b * 100 + $d - 4800 + int( $m / 10 );
        Temporal::DateTime.new(
            date => Temporal::Date.new(
                year => $year, month  => $mon, day    => $mday ),
            time => Temporal::Time.new(
                hour => $hour, minute => $min, second => $sec  )
        );
    }
#   Not clear what spec S32-Temporal really means here...
#   multi sub localtime( :$time = time(), :$tz=<GMT> ) is export { ... } # NYI
#   multi sub localtime( Num $epoch = time() ) returns Temporal::DateTime { ... } # NYI
#   our Num sub time() {  ...  } # NYI
}

=begin pod

# Example:

#$date = Date.new( :year(2008), :month(1), :day(25) ); $date.month(); # 1
#Temporal::Time

role Temporal::TimeZone::Observance {
    my subset Offset of Int where { -86400 < $^a < 86400 };
    has Offset $.offset;
    has Bool   $.isdst;
    has Str    $.abbreviation; # CST, AST

    # The ISO8601 standard does not allow for offsets with
    # sub-minute resolutions. In real-world practice, this is not
    # an issue.
    our Str method iso8601 {
        my $hours = self.offset.abs / 3600;
        my $minutes = self.offset.abs % 3600;

        return self.offset < 0 ?? '-' :: '+'
                ~ $hours.fmt('%02d')
                ~ $minutes.truncate.fmt('%02d');
    }

    method Str { self.iso8601 }
}

=end pod

=begin pod

=head1 SEE ALSO
The best yet seen explanation of calendars, by Claus Tøndering
L<Calendar FAQ|http://www.tondering.dk/claus/calendar.html>.
Similar algorithms at L<http://www.hermetic.ch/cal_stud/jdn.htm>
and L<http://www.merlyn.demon.co.uk/daycount.htm>.
Perl 5 perldoc L<doc:Time::Local>.
L<S32-Temporal|http://perlcabal.org/syn/S32/Temporal.html>

=end pod
