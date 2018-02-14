my class Duration is Cool does Real {
    has Rat $.tai is default(0.0);
      # A linear count of seconds.

    method new($tai) {
        nqp::p6bindattrinvres(nqp::create(Duration),Duration,'$!tai',$tai.Rat.self) # .self blows up Failures
    }

    method Num   (Duration:D:) { $!tai.Num    }
    method Rat   (Duration:D:) { $!tai        }
    method narrow(Duration:D:) { $!tai.narrow }

    multi method Str(Duration:D:) { ~$.tai }

    multi method perl(Duration:D:) { "Duration.new({$.tai.perl})" }
}

multi sub prefix:<->(Duration:D $a) {
    Duration.new: -$a.tai;
}

multi sub infix:<+>(Duration:D $a, Real $b) {
    Duration.new: $a.tai + $b;
}
multi sub infix:<+>(Real $a, Duration:D $b) {
    Duration.new: $a + $b.tai;
}
multi sub infix:<+>(Duration:D $a, Duration:D $b) {
    Duration.new: $a.tai + $b.tai;
}

multi sub infix:<->(Duration:D $a, Real $b) {
    Duration.new: $a.tai - $b;
}
multi sub infix:<->(Duration:D $a, Duration:D $b) {
    Duration.new: $a.tai - $b.tai;
}

multi sub infix:<%>(Duration:D $a, Real $b) {
    Duration.new: $a.tai % $b
}

# vim: ft=perl6 expandtab sw=4
