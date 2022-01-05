my class Duration is Cool does Real {
    has Real $.tai is default(0);
      # A linear count of seconds.

    multi method new(Duration: Rat:D \tai --> Duration:D) {
        nqp::p6bindattrinvres(
          nqp::create(Duration),Duration,'$!tai',nqp::decont(tai) * 1000000000
        )
    }
    multi method new(Duration: \value --> Duration:D) {
        nqp::istype((my \tai := value.Rat),Failure)
          ?? tai.throw
          !! nqp::p6bindattrinvres(nqp::create(Duration),Duration,'$!tai',tai * 1000000000)
    }

    method tai(Duration:D: --> Rat:D) {
        $!tai / 1000000000
    }

    method from-posix-nanos(Duration:U: Int:D $nanos --> Duration:D) {
        nqp::p6bindattrinvres(nqp::create(Duration),Duration,'$!tai',$nanos)
    }

    method to-nanos(--> Int:D) {
        $!tai.Int
    }

    method Bridge(Duration:   --> Num:D) { self.defined ?? self.tai.Num !! self.Real::Bridge }
    method Num   (Duration:D: --> Num:D) { self.tai.Num    }
    method Rat   (Duration:D: --> Rat:D) { self.tai        }
    method narrow(Duration:D:          ) { self.tai.narrow }

    multi method Str(Duration:D: --> Str:D) { ~self.tai }

    multi method raku(Duration:D: --> Str:D) { "Duration.new({($!tai / 1000000000).raku})" }
}

multi sub prefix:<->(Duration:D $a --> Duration:D) {
    Duration.from-posix-nanos: -$a.to-nanos;
}

multi sub infix:<+>(Duration:D $a, Real $b --> Duration:D) {
    Duration.new: $a.tai + $b;
}
multi sub infix:<+>(Real $a, Duration:D $b --> Duration:D) {
    Duration.new: $a + $b.tai;
}
multi sub infix:<+>(Duration:D $a, Duration:D $b --> Duration:D) {
    Duration.new: $a.tai + $b.tai;
}

multi sub infix:<->(Duration:D $a, Real $b --> Duration:D) {
    Duration.new: $a.tai - $b;
}
multi sub infix:<->(Duration:D $a, Duration:D $b --> Duration:D) {
    Duration.new: $a.tai - $b.tai;
}

multi sub infix:<%>(Duration:D $a, Real $b --> Duration:D) {
    Duration.new: $a.tai % $b
}

# vim: expandtab shiftwidth=4
