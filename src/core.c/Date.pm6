my class Date does Dateish {

    method !formatter(--> Str:D) {
        sprintf '%s-%02d-%02d',self!year-Str,$!month,$!day
    }

#?if moar
    my constant $valid-units = nqp::hash(
#?endif
#?if !moar
    my $valid-units := nqp::hash(
#?endif
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

    # fast object creation with sanity check on month/day
    method !SET-SELF(\year,\month,\day,\formatter --> Date:D) {
        self!oor("Month",month,"1..12")
          unless 1 <= month <= 12;
        self!oor("Day",day,"1..{self!DAYS-IN-MONTH(year,month)}")
          unless 1 <= day <= self!DAYS-IN-MONTH(year,month);

        nqp::bindattr(self,Date,'$!year',year);
        nqp::bindattr(self,Date,'$!month',month);
        nqp::bindattr(self,Date,'$!day',day);
        nqp::bindattr(self,Date,'&!formatter',formatter);
        self
    }

    # object creation for subclasses, wit sanity check on month/day
    method !bless($year, $month, $day, &formatter, %nameds) {
        self!oor("Month",$month,"1..12")
          unless 1 <= $month <= 12;
        self!oor("Day",$day,"1..{self!DAYS-IN-MONTH($year,$month)}")
          unless 1 <= $day <= self!DAYS-IN-MONTH($year,$month);

        self.bless(:$year,:$month,:$day,:&formatter,|%nameds)!SET-DAYCOUNT
    }

    proto method new(|) {*}
    multi method new(Date:
      Int:D() $year, Int:D() $month, Int:D() $day, :&formatter
    --> Date:D) {
        nqp::eqaddr(self.WHAT,Date)
          ?? nqp::create(self)!SET-SELF($year, $month, $day, &formatter)
          !! self!bless($year, $month, $day, &formatter, %_)
    }
    multi method new(Date:
      Int:D() :$year!, Int:D() :$month = 1, Int:D() :$day = 1, :&formatter
    --> Date:D) {
        nqp::eqaddr(self.WHAT,Date)
          ?? nqp::create(self)!SET-SELF($year, $month, $day, &formatter)
          !! self!bless($year, $month, $day, &formatter, %_)
    }
    multi method new(Date: Str:D $date, :&formatter --> Date:D) {

        # do we have non-ascii chars in there?
        if nqp::chars($date) == nqp::codes($date) {

            # no, can we fastpath?
            if nqp::chars($date) == 10
              && nqp::eqat($date,'-',4)
              && nqp::eqat($date,'-',7) {
                nqp::eqaddr(self.WHAT,Date)
                 ?? nqp::create(self)!SET-SELF(
                      nqp::substr($date,0,4).Int,
                      nqp::substr($date,5,2).Int,
                      nqp::substr($date,8,2).Int,
                      &formatter
                    )
                 !! self!bless(
                      nqp::substr($date,0,4).Int,
                      nqp::substr($date,5,2).Int,
                      nqp::substr($date,8,2).Int,
                      &formatter,
                      %_
                    )
            }

            # no, can we use regex?
            elsif $date.match(/^
                  (<[+-]>? \d**4 \d*)  # year
                  '-'
                  (\d\d)               # month
                  '-'
                  (\d\d)               # day
                $/) {
                nqp::eqaddr(self.WHAT,Date)
                  ?? nqp::create(self)!SET-SELF($0.Int,$1.Int,$2.Int,&formatter)
                  !! self!bless($0.Int, $1.Int, $2.Int, &formatter, %_)
            }

            # no, too bad
            else {
                self!tif($date,'Date','yyyy-mm-dd');
            }
        }

        # has non-ascii chars
        else {
            self!tif($date,'Date','yyyy-mm-dd');
        }
    }
    multi method new(Date: Dateish $d, :&formatter, *%_ --> Date:D) {
        nqp::eqaddr(self.WHAT,Date)
          ?? nqp::create(self)!SET-SELF($d.year,$d.month,$d.day,&formatter)
          !! self.bless(
               :year($d.year),:month($d.month),:day($d.day),:&formatter, |%_
             )!SET-DAYCOUNT
    }
    multi method new(Date: Instant $i, :&formatter, *%_ --> Date:D) {
        self.new(DateTime.new($i),:&formatter,|%_)   # XXX could be faster
    }
    proto method new-from-daycount($) {*}
    multi method new-from-daycount(Date:U:
      $daycount, :&formatter
    --> Date:D) {
        self!new-from-daycount($daycount, &formatter, %_)
    }
    multi method new-from-daycount(Date:D:
      $daycount, :&formatter = &!formatter
    --> Date:D) {
        self!new-from-daycount($daycount, &formatter, %_)
    }

    method !new-from-daycount($daycount, &formatter, %nameds --> Date:D) {
        self!ymd-from-daycount($daycount, my $year, my $month, my $day);
        if nqp::eqaddr(self.WHAT,Date) {
            my $new := nqp::create(self);
            nqp::bindattr($new,Date,'$!year',nqp::decont($year));
            nqp::bindattr($new,Date,'$!month',nqp::decont($month));
            nqp::bindattr($new,Date,'$!day',nqp::decont($day));
            nqp::bindattr($new,Date,'&!formatter',nqp::decont(&formatter));
            nqp::bindattr($new,Date,'$!daycount',nqp::decont($daycount));
            $new
        }
        else {
           self.bless(
             :$year,:$month,:$day,:&formatter,:$daycount,|%nameds
           )!SET-DAYCOUNT
        }
    }

    method today(:&formatter --> Date:D) { self.new(DateTime.now, :&formatter) }

    multi method WHICH(Date:D: --> ValueObjAt:D) {
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

    method truncated-to(Cool $unit --> Date:D) {
        self!clone-without-validating(
          |self!truncate-ymd(self!VALID-UNIT($unit)));
    }

    method later(:$earlier, *%unit --> Date:D) {

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
              !! $!day,
              :&!formatter)
        }
        else { # year
            my Int $year = $!year + $amount;
            self.new($year,$!month,$!day > 28
              ?? $!day min self!DAYS-IN-MONTH($year,$!month)
              !! $!day,
              :&!formatter)
        }
    }

    method clone(Date:D: *%_ --> Date:D) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        self.new(
          nqp::ifnull(nqp::atkey($h,'year'), $!year),
          nqp::ifnull(nqp::atkey($h,'month'),$!month),
          nqp::ifnull(nqp::atkey($h,'day'),  $!day),
          formatter => nqp::ifnull(nqp::atkey($h,'formatter'),&!formatter),
        )
    }

    # A premature optimization.
    method !clone-without-validating(Date:D: *%_ --> Date:D) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        nqp::create(self)!SET-SELF(
          nqp::ifnull(nqp::atkey($h,'year'), $!year),
          nqp::ifnull(nqp::atkey($h,'month'),$!month),
          nqp::ifnull(nqp::atkey($h,'day'),  $!day),
          &!formatter,
        )
    }

    method new-from-diff(Date:D: Int:D $diff --> Date:D) {
        nqp::isconcrete($!daycount)
          ?? nqp::stmts(
               (my \new := nqp::clone(self)),
               nqp::bindattr(new,Date,'$!day', $!day + $diff),
               nqp::bindattr(new,Date,'$!daycount',$!daycount + $diff),
               new
             )
          !! nqp::p6bindattrinvres(nqp::clone(self),Date,'$!day',$!day + $diff)
    }

    method succ(Date:D: --> Date:D) {
        $!day < 28 && nqp::eqaddr(self.WHAT,Date)
          ?? self.new-from-diff(1)
          !! self.new-from-daycount(self.daycount + 1)
    }
    method pred(Date:D: --> Date:D) {
        $!day > 1 && nqp::eqaddr(self.WHAT,Date)
          ?? self.new-from-diff(-1)
          !! self.new-from-daycount(self.daycount - 1)
    }

    multi method raku(Date:D: --> Str:D) {
        self.^name ~ ".new($!year,$!month,$!day)"
    }
    multi method ACCEPTS(Date:D: DateTime:D $dt --> Bool:D) {
        $dt.day == $!day && $dt.month == $!month && $dt.year == $!year
    }

    proto method DateTime()  {*}
    multi method DateTime(Date:D: --> DateTime:D) {
        DateTime.new(:$!year, :$!month, :$!day)
    }
    multi method DateTime(Date:U: --> DateTime:U) { DateTime }
    method Date(--> Date) { self }
}

multi sub infix:<+>(Date:D $d, Int:D $x --> Date:D) {
    nqp::eqaddr($d.WHAT,Date) && 0 < $d.day + $x <= 28
      ?? $d.new-from-diff($x)
      !! Date.new-from-daycount($d.daycount + $x, formatter => $d.formatter)
}
multi sub infix:<+>(Int:D $x, Date:D $d --> Date:D) {
    nqp::eqaddr($d.WHAT,Date) && 0 < $d.day + $x <= 28
      ?? $d.new-from-diff($x)
      !! Date.new-from-daycount($d.daycount + $x, formatter => $d.formatter)
}
multi sub infix:<->(Date:D $d, Int:D $x --> Date:D) {
    nqp::eqaddr($d.WHAT,Date) && 0 < $d.day - $x <= 28
      ?? $d.new-from-diff(-$x)
      !! Date.new-from-daycount($d.daycount - $x, formatter => $d.formatter)
}
multi sub infix:<->(Date:D $a, Date:D $b --> Int:D) {
    $a.daycount - $b.daycount;
}
multi sub infix:<cmp>(Date:D $a, Date:D $b --> Order:D) {
    $a.daycount cmp $b.daycount
}
multi sub infix:«<=>»(Date:D $a, Date:D $b --> Order:D) {
    $a.daycount <=> $b.daycount
}
multi sub infix:<==>(Date:D $a, Date:D $b --> Bool:D) {
    $a.daycount == $b.daycount
}
multi sub infix:«<=»(Date:D $a, Date:D $b --> Bool:D) {
    $a.daycount <= $b.daycount
}
multi sub infix:«<»(Date:D $a, Date:D $b --> Bool:D) {
    $a.daycount < $b.daycount
}
multi sub infix:«>=»(Date:D $a, Date:D $b --> Bool:D) {
    $a.daycount >= $b.daycount
}
multi sub infix:«>»(Date:D $a, Date:D $b --> Bool:D) {
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
