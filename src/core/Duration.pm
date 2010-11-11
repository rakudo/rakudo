use v6;

class Duration does Real {
    has Rat $.x = 0;
      # A linear count of seconds.

    method new($x) { self.bless: *, x => $x.Rat }

    method Bridge() { $.x }

    method Str() { ~$.x }

    method perl() { "Duration.new({$.x.perl})" }
}

our multi sub prefix:<->(Duration $a) {
    Duration.new: -$a.x;
}

our multi sub infix:<+>(Duration $a, Real $b) {
    Duration.new: $a.x + $b;
}
our multi sub infix:<+>(Real $a, Duration $b) {
    Duration.new: $a + $b.x;
}
our multi sub infix:<+>(Duration $a, Duration $b) {
    Duration.new: $a.x + $b.x;
}

our multi sub infix:<->(Duration $a, Real $b) {
    Duration.new: $a.x - $b;
}
our multi sub infix:<->(Duration $a, Duration $b) {
    Duration.new: $a.x - $b.x;
}

our multi sub infix:<*>(Duration $a, Real $b) {
    Duration.new: $a.x * $b
}
our multi sub infix:<*>(Real $a, Duration $b) {
    Duration.new: $a * $b.x
}
our multi sub infix:<*>(Duration $a, Duration $b) {
    die "Can't multiply Durations together"
}

our multi sub infix:</>(Duration $a, Real $b) {
    Duration.new: $a.x / $b
}
our multi sub infix:</>(Real $a, Duration $b) {
    Duration.new: $b / $a.x
}
our multi sub infix:</>(Duration $a, Duration $b) {
    die "Can't divide a Duration by a Duration"
}

our multi sub infix:<%>(Duration $a, Real $b) {
    Duration.new: $a.x % $b
}
our multi sub infix:<%>(Real $a, Duration $b) {
    Duration.new: $b % $a.x
}
our multi sub infix:<%>(Duration $a, Duration $b) {
    die "Can't take remainder after division of a Duration by a Duration"
}

our multi sub infix:<**>(Duration $a, Real $b) {
    Duration.new: $a.x ** $b
}
our multi sub infix:<**>(Real $a, Duration $b) {
    die "Can't use a Duration as an exponent"
}
