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

    # Handle error if an error was found
    method !wrong-oor(int $year, int $month, int $day) {
        1 <= $month <= 12
          ?? self!oor("Day", $day, "1.." ~ self!DAYS-IN-MONTH($year, $month))
          !! self!oor("Month", $month, "1..12")
    }

    # Just set the attributes on an instance
    method !SET-SELF(int $year, int $month, int $day, \formatter --> Date:D) {
        nqp::bindattr_i(self,Date,'$!year',$year);
        nqp::bindattr_i(self,Date,'$!month',$month);
        nqp::bindattr_i(self,Date,'$!day',$day);
        nqp::bindattr(self,Date,'&!formatter',formatter);
        self
    }

    # Set attributes in instantiated invocant with given args if ok,
    # handling any subclasses of Date correctly
    method !populate(int $year, int $month, int $day, %nameds) {
        if nqp::islt_i($month,1)
          || nqp::isgt_i($month,12)
          || nqp::islt_i($day,1)
          || nqp::isgt_i($day, self!DAYS-IN-MONTH($year, $month)) {
            self!wrong-oor($year, $month, $day)
        }
        else {
            %nameds<day>   := $day;
            %nameds<month> := $month;
            %nameds<year>  := $year;
            self.BUILDALL(Empty, %nameds)
        }
    }

    # Handling cases where a day is not an Int, e.g. a * to indicate
    # last day of month, or a Callable indicating days from end of
    # month
    method !day-not-Int($year, $month, $day --> Int:D) {
        my $DIM := self!DAYS-IN-MONTH($year,$month);
        nqp::istype($day,Whatever)
          ?? $DIM
          !! nqp::istype($day,Callable)
            ?? $day($DIM)
            !! $day.Int
    }

    proto method new(|) {*}

    # Positional year, month, day
    multi method new(Date:
      Int() $year,
      Int() $month,
            $day is copy,
    --> Date:D) {
        $day = self!day-not-Int($year, $month, $day)
          unless nqp::istype($day,Int);

        nqp::create(self)!populate($year, $month, $day, %_)
    }

    # Named :year, :month, :day
    multi method new(Date:
      Int() :$year!,
      Int() :$month       = 1,
            :$day is copy = 1,
    --> Date:D) {
        $day = self!day-not-Int($year, $month, $day)
          unless nqp::istype($day,Int);

        nqp::create(self)!populate($year, $month, $day, %_)
    }

    # From string
    multi method new(Date: Str:D $Date --> Date:D) {
        my str $date = $Date;

        # do we have non-ascii chars in there?
        if nqp::chars($date) == nqp::codes($date) {

            # no, can we fastpath?
            if nqp::chars($date) == 10
              && nqp::eqat($date,'-',4)
              && nqp::eqat($date,'-',7) {
                nqp::create(self)!populate(
                  nqp::substr($date,0,4).Int,
                  nqp::substr($date,5,2).Int,
                  nqp::substr($date,8,2).Int,
                  %_
                )
            }

            # no, can we use regex?
            elsif $Date.match(/^
                  (<[+-]>? \d**4 \d*)  # year
                  '-'
                  (\d\d)               # month
                  '-'
                  (\d\d)               # day
                $/) {
                nqp::create(self)!populate($0.Int, $1.Int, $2.Int, %_)
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

    # From Date/DateTime or any other client of Dateish
    multi method new(Date: Dateish:D $d --> Date:D) {

        # Use any known formatter if none given
        unless %_<formatter> {
            %_<formatter> = $_ with $d.formatter;
        }
        nqp::create(self)!populate($d.year, $d.month, $d.day, %_)
    }

    multi method new(Date: Instant $i --> Date:D) {
        self!new-from-daycount(
          nqp::add_i(
            nqp::div_i(Rakudo::Internals.epoch-from-tai($i),86400),
            40587
          ),
          %_
        )
    }

    proto method new-from-daycount($) {*}
    multi method new-from-daycount(Date:U: $daycount --> Date:D) {
        self!new-from-daycount($daycount, %_)
    }
    multi method new-from-daycount(Date:D: $daycount --> Date:D) {
        # Use any known formatter if none given
        unless %_<formatter> {
            %_<formatter> = $_ with &!formatter;
        }
        self!new-from-daycount($daycount, %_)
    }

    method !new-from-daycount(int $daycount, %nameds --> Date:D) {
        self!ymd-from-daycount($daycount,
          my int $year, my int $month, my int $day
        );

        my $self := nqp::create(self);
        nqp::bindattr_i($self,Date,'$!daycount',$daycount);
        $self!populate($year, $month, $day, %nameds)
    }

    method today(--> Date:D) {
        my $lt := nqp::decodelocaltime(time);
        nqp::eqaddr(self.WHAT,Date)
          ?? nqp::create(self)!SET-SELF(
               nqp::atpos_i($lt,5),  # year
               nqp::atpos_i($lt,4),  # month
               nqp::atpos_i($lt,3),  # day
               %_<formatter> // (my &)
             )
          !! nqp::create(self)!populate(
               nqp::atpos_i($lt,5),  # year
               nqp::atpos_i($lt,4),  # month
               nqp::atpos_i($lt,3),  # day
               %_
             )
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
        elsif nqp::eqat($unit,'year',0) {
            my int $year = nqp::add_i($!year,$amount);

            my $new := nqp::clone(self);
            nqp::bindattr_i($new,Date,'$!year',$year);
            nqp::bindattr_i($new,Date,'$!day',
              self!clip-day($year,$!month,$!day)
            ) if nqp::isgt_i($!day,28);
            nqp::bindattr_i($new,Date,'$!daycount',0);
            $new
        }
        elsif nqp::eqat($unit,'hour',0)
          || nqp::eqat($unit,'minute',0)
          || nqp::eqat($unit,'second',0) {
            die "Cannot use '$amount $unit' as a unit on a {self.^name}"
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

    method clone(Date:D: --> Date:D) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        if nqp::elems($h) {
            if nqp::atkey($h,'day') -> $day is copy {
                unless nqp::istype($day,Int) {
                    my int $year  = nqp::ifnull(nqp::atkey($h,'year'), $!year);
                    my int $month = nqp::ifnull(nqp::atkey($h,'month'),$!month);
                    $day = self!day-not-Int($year, $month, $day);
                    return nqp::clone(self)!populate(
                      $year, $month, $day, %_
                    );
                }
            }
            nqp::clone(self)!populate(
              nqp::ifnull(nqp::atkey($h,'year'), $!year),
              nqp::ifnull(nqp::atkey($h,'month'),$!month),
              nqp::ifnull(nqp::atkey($h,'day'),  $!day),
              %_
            )
        }
        else {
            nqp::clone(self)
        }
    }

    # Clone an object with any twiddles without doing any checks
    method !clone-without-validating(Date:D: --> Date:D) {
        my $h := nqp::getattr(%_,Map,'$!storage');
        nqp::clone(self)!SET-SELF(
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
    method Date() {
        nqp::eqaddr(self.WHAT,Date)
          ?? self
          !! nqp::create(Date)!SET-SELF($!year, $!month, $!day, &!formatter)
    }

    multi method Int(Date:D: --> Int:D) {
        self.daycount
    }

    multi method Numeric(Date:D: --> Int:D) {
        self.daycount
    }

    multi method Real(Date:D: --> Int:D) {
        self.daycount
    }
}

multi sub infix:<+>(Date:D $date, Int:D $x --> Date:D) {
    $date.MOVE-DAYS($x)
}
multi sub infix:<+>(Int:D $x, Date:D $date --> Date:D) {
    $date.MOVE-DAYS($x)
}
multi sub infix:<->(Date:D $date, Int:D $x --> Date:D) {
    $date.MOVE-DAYS(nqp::neg_i($x))
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
