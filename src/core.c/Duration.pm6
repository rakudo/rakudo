my class Duration is Cool does Real {
    has Rat $.tai is default(0.0);
      # A linear count of seconds.

    multi method new(Duration: Rat:D \tai --> Duration:D) {
        nqp::p6bindattrinvres(nqp::create(Duration),Duration,'$!tai',tai)
    }
    multi method new(Duration: \value --> Duration:D) {
        nqp::if(
          nqp::istype((my \tai := value.Rat),Failure),
          tai.throw,
          nqp::p6bindattrinvres(nqp::create(Duration),Duration,'$!tai',tai)
        )
    }

    method Bridge(Duration:D: --> Num:D) { $!tai.Num    }
    method Num   (Duration:D: --> Num:D) { $!tai.Num    }
    method Rat   (Duration:D: --> Rat:D) { $!tai        }
    method narrow(Duration:D:          ) { $!tai.narrow }

    multi method Str(Duration:D: --> Str:D) { ~$.tai }

    multi method perl(Duration:D: --> Str:D) { "Duration.new({$.tai.perl})" }
}

multi sub prefix:<->(Duration:D $a --> Duration:D) {
    Duration.new: -$a.tai;
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

# vim: ft=perl6 expandtab sw=4
