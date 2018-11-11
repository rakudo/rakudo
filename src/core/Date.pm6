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
    multi method new(Date: Int:D() $year, Int:D() $month, Int:D() $day, :&formatter, *%_) {
        1 <= $month <= 12
          || X::OutOfRange.new(:what<Month>,:got($month),:range<1..12>).throw;
        1 <= $day <= self!DAYS-IN-MONTH($year,$month)
          || X::OutOfRange.new(
               :what<Day>,
               :got($day),
               :range("1..{self!DAYS-IN-MONTH($year,$month)}")
             ).throw;
        nqp::eqaddr(self.WHAT,Date)
          ?? nqp::create(self)!SET-SELF($year,$month,$day,&formatter)
          !! self.bless(:$year,:$month,:$day,:&formatter,|%_)
    }
    multi method new(Date: Int:D() :$year!, Int:D() :$month = 1, Int:D() :$day = 1, :&formatter, *%_) {
        1 <= $month <= 12
          || X::OutOfRange.new(:what<Month>,:got($month),:range<1..12>).throw;
        1 <= $day <= self!DAYS-IN-MONTH($year,$month)
          || X::OutOfRange.new(
               :what<Day>,
               :got($day),
               :range("1..{self!DAYS-IN-MONTH($year,$month)}")
             ).throw;
        nqp::eqaddr(self.WHAT,Date)
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
        nqp::eqaddr(self.WHAT,Date)
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
        nqp::eqaddr(self.WHAT,Date)
          ?? nqp::create(self)!SET-SELF($year,$month,$day,&formatter,$daycount)
          !! self.bless(:$year,:$month,:$day,:&formatter,:$daycount)
    }

    method today(:&formatter) { self.new(DateTime.now, :&formatter) }

    multi method WHICH(Date:D:) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Date),
              'Date|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::unbox_i(self.daycount)
          ),
          ValueObjAt
        )
    }

    method truncated-to(Cool $unit) {
        self!clone-without-validating(
          |self!truncate-ymd(self!VALID-UNIT($unit)));
    }

    method later(:$earlier, *%unit) {

        # basic sanity check
        nqp::if(
          nqp::eqaddr(
            (my \later := (my \iterator := %unit.iterator).pull-one),
            IterationEnd
          ),
          (die "No time unit supplied"),
          nqp::unless(
            nqp::eqaddr(iterator.pull-one,IterationEnd),
            (die "More than one time unit supplied")
          )
        );
        my $unit  := later.key;
        my $amount = later.value;
        $amount = -$amount if $earlier;

        if nqp::atkey($valid-units,$unit) -> $multiplier {
            self.new-from-daycount(self.daycount + $multiplier * $amount )
        }
        elsif $unit.starts-with('month') {
            my Int $month = $!month;
            my Int $year  = $!year;
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
            my Int $year = $!year + $amount;
            self.new($year,$!month,$!day > 28
              ?? $!day min self!DAYS-IN-MONTH($year,$!month)
              !! $!day);
        }
    }

    method clone(*%_) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        self.new(
          nqp::ifnull(nqp::atkey($h,'year'), $!year),
          nqp::ifnull(nqp::atkey($h,'month'),$!month),
          nqp::ifnull(nqp::atkey($h,'day'),  $!day),
          formatter => nqp::ifnull(nqp::atkey($h,'formatter'),&!formatter),
        )
    }
    method !clone-without-validating(*%_) { # A premature optimization.
        my $h := nqp::getattr(%_,Map,'$!storage');
        nqp::create(self)!SET-SELF(
          nqp::ifnull(nqp::atkey($h,'year'), $!year),
          nqp::ifnull(nqp::atkey($h,'month'),$!month),
          nqp::ifnull(nqp::atkey($h,'day'),  $!day),
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

    proto method DateTime()  {*}
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

proto sub sleep($?, *%) {*}
multi sub sleep(--> Nil) { sleep(*) }
multi sub sleep($seconds --> Nil) {
    # 1e9 seconds is a large enough value that still makes VMs sleep
    # larger values cause nqp::sleep() to exit immediately (esp. on 32-bit)
    if nqp::istype($seconds,Whatever) || $seconds == Inf {
        nqp::sleep(1e9) while True;
    }
    elsif $seconds > 1e9 {
        nqp::sleep($_) for gather {
            1e9.take xx ($seconds / 1e9);
            take $seconds - 1e9 * ($seconds / 1e9).Int;
        }
    }
    elsif $seconds > 0e0 {
        nqp::sleep($seconds.Num);
    }
}

proto sub sleep-timer($?, *%) {*}
multi sub sleep-timer(--> Duration:D) { sleep-timer(*) }
multi sub sleep-timer($seconds --> Duration:D) {
    my $time1 = now;
    sleep($seconds);
    Duration.new( ( $seconds - (now - $time1) ) max 0 )
}

proto sub sleep-until($, *%) {*}
multi sub sleep-until(Instant() $until --> Bool:D) {
    my $seconds = $until - now;
    return False if $seconds < 0;

    Nil while $seconds = sleep-timer($seconds);
    True;
}

# vim: ft=perl6 expandtab sw=4
