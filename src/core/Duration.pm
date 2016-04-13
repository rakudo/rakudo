my class Duration is Cool does Real {
    has Rat $.tai = 0;
      # A linear count of seconds.

    method new($tai) { self.bless: tai => $tai.Rat }

    method Bridge(Duration:D:) { $!tai.Bridge }
    method Rat(Duration:D:)    { $!tai }
    method Num(Duration:D:)    { $!tai.Num }
    method narrow(Duration:D:) { $!tai.narrow }

    multi method Str(Duration:D:) { ~$.tai }
    multi method Numeric() { $!tai } # (Unary plus)

    multi method WHICH(Duration:D:) { self.^name ~ '|' ~ $!tai.WHICH }

    multi method perl(Duration:D:) { "Duration.new({$.tai.perl})" }
}

# For Addition and Subtraction operations, dimensionless number operands are
# assumed to represent durations in units of seconds; once this promotion is
# done, all operations are dimensionally consistent and return Duration objects.
#
# For the rest of the operations, I define three categories:
#   - Perfect:  Dimensionally consistent operations that naturally return
#               Duration objects.
#   - Dimensional:  Dimensionally consistent operations that do *not* return
#                   Duration objects.
#   - Non-Dimensional:  Operations that are non-sensical or would naturally return
#                       values in units we don't have types for.  For these, we
#                       auto-promote dimensionless operands to duration, and
#                       we *always* return values of type Duration.
#
# (Rationale for 'Non-Dimensional' behavior: anyone performing such operations
#  most likely wishes to simply treat Durations and numbers both as durations in
#  units of seconds.)


# Unary minus
multi sub prefix:<->(Duration:D $a) {
    Duration.new: -$a.tai;
}

# Addition
multi sub infix:<+>(Duration:D $a, Real $b) {
    Duration.new: $a.tai + $b;
}
multi sub infix:<+>(Real $a, Duration:D $b) {
    Duration.new: $a + $b.tai;
}
multi sub infix:<+>(Duration:D $a, Duration:D $b) {
    Duration.new: $a.tai + $b.tai;
}

# Subtraction
multi sub infix:<->(Duration:D $a, Real $b) {
    Duration.new: $a.tai - $b;
}
multi sub infix:<->(Real $a, Duration:D $b) {
    Duration.new: $a - $b.tai;
}
multi sub infix:<->(Duration:D $a, Duration:D $b) {
    Duration.new: $a.tai - $b.tai;
}

# Perfect:  Dur * Real --> Dur,  Real * Dur --> Dur
# Non-Dimensional:  Dur * Dur --> Dur
multi sub infix:<*>(Duration:D $a, Real $b) {
    Duration.new: $a.tai * $b
}
multi sub infix:<*>(Real $a, Duration:D $b) {
    Duration.new: $a * $b.tai
}
multi sub infix:<*>(Duration:D $a, Duration:D $b) {
    Duration.new: $a.tai * $b.tai
}

# Dimension examples for division and modulo operations:
#
# 119 seconds / 2  =  59.5 seconds  =  59 seconds, remainder 1 second
# 1 minute / 28 seconds  =  apx. 2.143  ==  2, remainder 4 seconds
# 451 / 42 seconds is non-dimensional.. redefine it as:
#     451 seconds / 42 seconds  =  apx. 10.738  ==  10, remainder 31 seconds
#
# (Remember: in non-dimensional cases, we *always* return a Duration.)

# Perfect:  Dur / Real --> Dur
# Dimensional:  Dur / Dur --> Real
# Non-Dimensional:  Real / Dur --> Dur
multi sub infix:</>(Duration:D $a, Real $b) {
    Duration.new: $a.tai / $b
}
multi sub infix:</>(Real $a, Duration:D $b) {
    Duration.new: $a / $b.tai
}
multi sub infix:</>(Duration:D $a, Duration:D $b) {
    $a.tai / $b.tai
}

# Perfect:  Dur % Real --> Dur,  Dur % Dur --> Dur
# Non-Dimensional:  Real % Dur --> Dur
multi sub infix:<%>(Duration:D $a, Real $b) {
    Duration.new: $a.tai % $b
}
multi sub infix:<%>(Real $a, Duration:D $b) {
    Duration.new: $a % $b.tai
}
multi sub infix:<%>(Duration:D $a, Duration:D $b) {
    Duration.new: $a.tai % $b.tai
}

# vim: ft=perl6 expandtab sw=4
