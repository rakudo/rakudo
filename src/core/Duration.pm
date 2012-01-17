my class Duration is Real {
    has Rat $.x = 0;
      # A linear count of seconds.

    method new($x) { self.bless: *, x => $x.Rat }

    method Bridge(Duration:D:) { $!x.Num }
    method Rat(Duration:D:)    { $!x     }
    method Num(Duration:D:)    { $!x     }

    multi method Str(Duration:D:) { ~$.x }

    multi method perl(Duration:D:) { "Duration.new({$.x.perl})" }
}

multi sub prefix:<->(Duration:D $a) {
    Duration.new: -$a.x;
}

multi sub infix:<+>(Duration:D $a, Real $b) {
    Duration.new: $a.x + $b;
}
multi sub infix:<+>(Real $a, Duration:D $b) {
    Duration.new: $a + $b.x;
}
multi sub infix:<+>(Duration:D $a, Duration:D $b) {
    Duration.new: $a.x + $b.x;
}

multi sub infix:<->(Duration:D $a, Real $b) {
    Duration.new: $a.x - $b;
}
multi sub infix:<->(Duration:D $a, Duration:D $b) {
    Duration.new: $a.x - $b.x;
}

multi sub infix:<%>(Duration:D $a, Real $b) {
    Duration.new: $a.x % $b
}
