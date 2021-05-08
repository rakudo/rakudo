my class Date does Dateish {

    method !formatter(--> Str:D) { self.yyyy-mm-dd }

#?if !js
    my constant $valid-units = nqp::hash(
#?endif
#?if js
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

    # fast object creation with sanity check on month/day
    method !SET-SELF(\year,\month,\day,\formatter --> Date:D) {
        self!oor("Month",month,"1..12")
          unless 1 <= month <= 12;
        self!oor("Day",day,"1..{self!DAYS-IN-MONTH(year,month)}")
          unless 1 <= day <= self!DAYS-IN-MONTH(year,month);

        nqp::bindattr_i(self,Date,'$!year',year);
        nqp::bindattr_i(self,Date,'$!month',month);
        nqp::bindattr_i(self,Date,'$!day',day);
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
        self!new-from-daycount(
          nqp::add_i(
            nqp::div_i(Rakudo::Internals.epoch-from-tai($i),86400),
            40587
          ),
          &formatter, %_)
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

    method !new-from-daycount(int $daycount, &formatter, %nameds --> Date:D) {
        self!ymd-from-daycount($daycount,
          my int $year, my int $month, my int $day);
        if nqp::eqaddr(self.WHAT,Date) {
            my $new := nqp::create(self);
            nqp::bindattr_i($new,Date,'$!year',$year);
            nqp::bindattr_i($new,Date,'$!month',$month);
            nqp::bindattr_i($new,Date,'$!day',$day);
            nqp::bindattr($new,Date,'&!formatter',nqp::decont(&formatter));
            nqp::bindattr_i($new,Date,'$!daycount',$daycount);
            $new
        }
        else {
           self.bless(
             :$year,:$month,:$day,:&formatter,:$daycount,|%nameds
           )!SET-DAYCOUNT
        }
    }

    method today(:&formatter --> Date:D) {
        my $lt := nqp::decodelocaltime(time);
        nqp::eqaddr(self.WHAT,Date)
          ?? nqp::create(self)!SET-SELF(
               nqp::atpos_i($lt,5),  # year
               nqp::atpos_i($lt,4),  # month
               nqp::atpos_i($lt,3),  # day
               &formatter)
          !! self!bless(
               nqp::atpos_i($lt,5),  # year
               nqp::atpos_i($lt,4),  # month
               nqp::atpos_i($lt,3),  # day
               &formatter, %_)
    }

    method first-date-in-month(Date:D: --> Date:D) {
        if $!day == 1 {
            self
        }
        else {
            my $date := nqp::clone(self);
            nqp::bindattr_i($date,self.WHAT,'$!day',1);
            nqp::bindattr_i(
              $date,self.WHAT,'$!daycount',$!daycount + 1 - $!day
            ) if $!daycount;
            $date
        }
    }

    method last-date-in-month(Date:D: --> Date:D) {
        my int $last-day = self.days-in-month;

        if $!day == $last-day {
            self
        }
        else {
            my $date := nqp::clone(self);
            nqp::bindattr_i($date,self.WHAT,'$!day',$last-day);
            nqp::bindattr_i(
              $date,self.WHAT,'$!daycount',$!daycount + $last-day - $!day
            ) if $!daycount;
            $date
        }
    }

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

    method truncated-to(Date:D: str $unit --> Date:D) {
        my $truncated := nqp::clone(self);
        my $what      := self.WHAT;
        nqp::bindattr_i($truncated,$what,'$!daycount',0);
        nqp::if(
          nqp::eqat($unit,'week',0),
          ($truncated := $truncated.move-by-unit(
            'day',
            nqp::sub_i(1,$truncated.day-of-week)
          )),
          nqp::stmts(
            nqp::bindattr_i($truncated,$what,'$!day',1),
            nqp::unless(
              nqp::eqat($unit,'month',0),
              nqp::stmts(
                nqp::bindattr_i($truncated,$what,'$!month',1),
                nqp::unless(
                  nqp::eqat($unit,'year',0),
                  die "Cannot truncate {self.^name} object to '$unit'"
                )
              )
            )
          )
        );

        $truncated
    }

    # workhorse method for moving a Date
    method move-by-unit(str $unit, int $amount) is implementation-detail {
        if nqp::atkey($valid-units,$unit) -> int $multiplier {
            self!move-days(nqp::mul_i($multiplier,$amount));
        }
        elsif nqp::eqat($unit,'month',0) {
            my int $month = nqp::add_i($!month,$amount);
            my int $year;
            if nqp::bitor_i(nqp::islt_i($month,1),nqp::isgt_i($month,12)) {
                $year  = nqp::add_i($!year,nqp::div_i(nqp::sub_i($month,1),12));
                $month = nqp::add_i(nqp::mod_i(nqp::sub_i($month,1),12),1);
                $month = nqp::add_i($month,12) if nqp::islt_i($month,1);
            }
            else {
                $year = $!year;
            }

            my $new := nqp::clone(self);
            nqp::bindattr_i($new,Date,'$!year',$year);
            nqp::bindattr_i($new,Date,'$!month',$month);
            nqp::bindattr_i($new,Date,'$!day',
              self!clip-day($year,$month,$!day))
              if $!day > 28;
            nqp::bindattr_i($new,Date,'$!daycount',0);
            $new
        }
        else { # year
            my int $year = nqp::add_i($!year,$amount);

            my $new := nqp::clone(self);
            nqp::bindattr_i($new,Date,'$!year',$year);
            nqp::bindattr_i($new,Date,'$!day',
              self!clip-day($year,$!month,$!day)
            ) if nqp::isgt_i($!day,28);
            nqp::bindattr_i($new,Date,'$!daycount',0);
            $new
        }
    }

    # Helper method to move a number of days within a month
    method !move-days-within-month(int $days --> Date:D) {
        my $new := nqp::clone(self);
        nqp::bindattr_i($new,Date,'$!day', $!day + $days);
        nqp::bindattr_i($new,Date,'$!daycount',$!daycount + $days)
          if $!daycount;
        $new
    }

    # Helper method to move a number of days
    method !move-days(int $days --> Date:D) {
        if $days > 0 && $!day + $days <= self.days-in-month {
            self!move-days-within-month($days)
        }
        else {
            my int $daycount = self.daycount + $days;
            self!ymd-from-daycount(
              $daycount, my int $year, my int $month, my int $day);

            my $new := nqp::clone(self);
            nqp::bindattr_i($new,Date,'$!year',$year);
            nqp::bindattr_i($new,Date,'$!month',$month);
            nqp::bindattr_i($new,Date,'$!day',
              $day < 28 ?? $day !! self!clip-day($year,$month,$day));
            nqp::bindattr_i($new,Date,'$!daycount',$daycount);
            $new
        }
    }

    # If we overflow on days in the month, rather than throw an
    # exception, we just clip to the last of the month
    method !clip-day(int $year, int $month, int $day) {
        (my int $max = self!DAYS-IN-MONTH($year, $month)) < $day
          ?? $max
          !! $day
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

    # internal method that needs to be public for operators
    method MOVE-DAYS(Date:D: int $diff --> Date:D) is implementation-detail {
        my int $day = $!day + $diff;
        $day > 0 && $day < 28
          ?? self!move-days-within-month($diff)
          !! self!move-days($diff)
    }

    method succ(Date:D: --> Date:D) {
        $!day < 28
          ?? self!move-days-within-month(1)
          !! self!move-days(1)
    }
    method pred(Date:D: --> Date:D) {
        $!day > 1
          ?? self!move-days-within-month(-1)
          !! self!move-days(-1)
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
    method Date() { self }
}

multi sub infix:<+>(Date:D \date, Int:D $x --> Date:D) {
    date.MOVE-DAYS($x)
}
multi sub infix:<+>(Int:D $x, Date:D \date --> Date:D) {
    date.MOVE-DAYS($x)
}
multi sub infix:<->(Date:D \date, Int:D $x --> Date:D) {
    date.MOVE-DAYS(nqp::neg_i($x))
}
multi sub infix:<->(Date:D $a, Date:D $b --> Int:D) {
    $a.daycount - $b.daycount;
}
multi sub infix:<cmp>(Date:D $a, Date:D $b) {
    $a.daycount cmp $b.daycount
}
multi sub infix:«<=>»(Date:D $a, Date:D $b) {
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

# vim: expandtab shiftwidth=4
