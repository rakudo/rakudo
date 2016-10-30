my class Date does Dateish {

    method !formatter() { sprintf '%s-%02d-%02d',self!year-Str,$!month,$!day }

    my $valid-units := nqp::hash(
      'day',    1,
      'days',   1,
      'week',   7,
      'weeks',  7,
      'month',  0,
      'months', 0,
      'year',   0,
      'years',  0,
    );
    method !VALID-UNIT($unit) {
        nqp::existskey($valid-units,$unit)
          ?? $unit
          !! X::DateTime::InvalidDeltaUnit.new(:$unit).throw
    }

    method !SET-SELF($!year,$!month,$!day,&!formatter,$!daycount = Int) { self }

    proto method new(|) {*}
    multi method new(Date: Int() $year, Int() $month, Int() $day, :&formatter, *%_) {
        (1..12).in-range($month,'Month');
        (1 .. self!DAYS-IN-MONTH($year,$month)).in-range($day,'Day');
        self === Date
          ?? nqp::create(self)!SET-SELF($year,$month,$day,&formatter)
          !! self.bless(:$year,:$month,:$day,:&formatter,|%_)
    }
    multi method new(Date: Int() :$year!, Int() :$month = 1, Int() :$day = 1, :&formatter, *%_) {
        self === Date
          ?? nqp::create(self)!SET-SELF($year,$month,$day,&formatter)
          !! self.bless(:$year,:$month,:$day,:&formatter,|%_)
    }
    multi method new(Date: Str $date, :&formatter, *%_) {
        X::Temporal::InvalidFormat.new(
          invalid-str => $date,
          target      => 'Date',
          format      => 'yyyy-mm-dd',
        ).throw unless $date.codes == $date.chars and $date ~~ /^
          (<[+-]>? \d**4 \d*)                            # year
          '-'
          (\d\d)                                         # month
          '-'
          (\d\d)                                         # day
        $/;
        self.new($0,$1,$2,:&formatter,|%_)
    }
    multi method new(Date: Dateish $d, :&formatter, *%_) {
        self === Date
          ?? nqp::create(self)!SET-SELF($d.year,$d.month,$d.day,&formatter)
          !! self.bless(
               :year($d.year),
               :month($d.month),
               :day($d.day),
               :&formatter,
               |%_
             )
    }
    multi method new(Date: Instant $i, :&formatter, *%_) {
        self.new(DateTime.new($i),:&formatter,|%_)
    }
    method new-from-daycount($daycount,:&formatter) {
        self!ymd-from-daycount($daycount, my $year, my $month, my $day);
        self === Date
          ?? nqp::create(self)!SET-SELF($year,$month,$day,&formatter,$daycount)
          !! self.bless(:$year,:$month,:$day,:&formatter,:$daycount)
    }

    method today(:&formatter) { self.new(DateTime.now, :&formatter) }

    multi method WHICH(Date:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::unbox_i(self.daycount)
            ),
            ObjAt
        );
    }

    method truncated-to(Cool $unit) {
        self!clone-without-validating(
          |self!truncate-ymd(self!VALID-UNIT($unit)));
    }

    method later(:$earlier, *%unit) {
        my @pairs = %unit.pairs;
        die "More than one time unit supplied" if @pairs > 1;
        die "No time unit supplied"        unless @pairs;

        my $unit   = self!VALID-UNIT(@pairs.AT-POS(0).key);
        my $amount = @pairs.AT-POS(0).value.Int;
        $amount = -$amount if $earlier;

        if nqp::atkey($valid-units,$unit) -> $multiplier {
            self.new-from-daycount(self.daycount + $multiplier * $amount )
        }
        elsif $unit.starts-with('month') {
            my int $month = $!month;
            my int $year  = $!year;
            $month += $amount;
            $year += floor(($month - 1) / 12);
            $month = ($month - 1) % 12 + 1;
            # If we overflow on days in the month, rather than throw an
            # exception, we just clip to the last of the month
            self.new($year,$month,$!day > 28
              ?? $!day min self!DAYS-IN-MONTH($year,$month)
              !! $!day);
        }
        else { # year
            my int $year = $!year + $amount;
            self.new($year,$!month,$!day > 28
              ?? $!day min self!DAYS-IN-MONTH($year,$!month)
              !! $!day);
        }
    }

    method clone(*%_) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        self.new(
          nqp::existskey($h,'year')  ?? nqp::atkey($h,'year')  !! $!year,
          nqp::existskey($h,'month') ?? nqp::atkey($h,'month') !! $!month,
          nqp::existskey($h,'day')   ?? nqp::atkey($h,'day')   !! $!day,
          :&!formatter,
        )
    }
    method !clone-without-validating(*%_) { # A premature optimization.
        my $h := nqp::getattr(%_,Map,'$!storage');
        nqp::create(self)!SET-SELF(
          nqp::existskey($h,'year')  ?? nqp::atkey($h,'year')  !! $!year,
          nqp::existskey($h,'month') ?? nqp::atkey($h,'month') !! $!month,
          nqp::existskey($h,'day')   ?? nqp::atkey($h,'day')   !! $!day,
          &!formatter,
        )
    }

    method succ(Date:D:) {
        self.new-from-daycount(self.daycount + 1);
    }
    method pred(Date:D:) {
        self.new-from-daycount(self.daycount - 1);
    }

    multi method perl(Date:D:) {
        self.^name ~ ".new($!year,$!month,$!day)"
    }
    multi method ACCEPTS(Date:D: DateTime:D $dt) {
        $dt.day == $!day && $dt.month == $!month && $dt.year == $!year
    }

    proto method DateTime()  { * }
    multi method DateTime(Date:D:) { DateTime.new(:$!year, :$!month, :$!day) }
    multi method DateTime(Date:U:) { DateTime }
    method Date() { self }
}

multi sub infix:<+>(Date:D $d, Int:D $x) {
    Date.new-from-daycount($d.daycount + $x)
}
multi sub infix:<+>(Int:D $x, Date:D $d) {
    Date.new-from-daycount($d.daycount + $x)
}
multi sub infix:<->(Date:D $d, Int:D $x) {
    Date.new-from-daycount($d.daycount - $x)
}
multi sub infix:<->(Date:D $a, Date:D $b) {
    $a.daycount - $b.daycount;
}
multi sub infix:<cmp>(Date:D $a, Date:D $b) {
    $a.daycount cmp $b.daycount
}
multi sub infix:«<=>»(Date:D $a, Date:D $b) {
    $a.daycount <=> $b.daycount
}
multi sub infix:<==>(Date:D $a, Date:D $b) {
    $a.daycount == $b.daycount
}
multi sub infix:«<=»(Date:D $a, Date:D $b) {
    $a.daycount <= $b.daycount
}
multi sub infix:«<»(Date:D $a, Date:D $b) {
    $a.daycount < $b.daycount
}
multi sub infix:«>=»(Date:D $a, Date:D $b) {
    $a.daycount >= $b.daycount
}
multi sub infix:«>»(Date:D $a, Date:D $b) {
    $a.daycount > $b.daycount
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*TZ', {
    PROCESS::<$TZ> = Rakudo::Internals.get-local-timezone-offset();
}

sub sleep($seconds = Inf --> Nil) {
    if $seconds == Inf || nqp::istype($seconds,Whatever) {
        nqp::sleep(1e16) while True;
    }
    elsif $seconds > 0 {
        nqp::sleep($seconds.Num);
    }
}

sub sleep-timer(Real() $seconds = Inf --> Duration) {
    if $seconds <= 0 {
        Duration.new(0);
    }
    else {
        my $time1 = now;
        nqp::sleep($seconds.Num);
        Duration.new( ( $seconds - (now - $time1) ) max 0 );
    }
}

sub sleep-until(Instant() $until --> Bool) {
    my $seconds = $until - now;
    return False if $seconds < 0;

    Nil while $seconds = sleep-timer($seconds);
    True;
}

# vim: ft=perl6 expandtab sw=4
