use Test;

plan 3;

# The quote parser runs a code block after every escape it matches, and a
# code block builds $/ by walking the capture stack of the cursor it runs
# in.  Keep that stack short and an escape costs a small constant.  Let it
# grow with the escapes already matched and the parse turns quadratic.  The
# ratio below catches that well before the parse is slow enough to notice.

use MONKEY-SEE-NO-EVAL;

# A quadratic parse gets worse as $atoms grows, measuring 5x at 500 and 83x
# at 8000, while a linear one stays near 2x whatever $atoms is.  Lowering
# $atoms without lowering $budget stops this catching a quadratic parse.
my int $atoms  = 8000;
my int $budget = 10;

# Both sources spell a string of the same length.  'ab\"' is four source
# characters for three result ones, so 'abc' is its unescaped twin.
sub escaped-source(--> Str:D) { 'my $s = "' ~ ('ab\"' x $atoms) ~ '"; $s.chars' }
sub plain-source(  --> Str:D) { 'my $s = "' ~ ('abc'  x $atoms) ~ '"; $s.chars' }

sub fastest-of-three(Str:D $source --> Duration:D) {
    (^3).map({ my $started = now; EVAL $source; now - $started }).min
}

# These two also warm the compiler up on both shapes, so the timing below
# does not pay for machinery on its first measurement alone.
is EVAL(escaped-source()), $atoms * 3,
  "a string of $atoms escapes compiles to the {$atoms * 3} characters it spells";

is EVAL(plain-source()), $atoms * 3,
  "a string of {$atoms * 3} plain characters compiles to that many characters";

# Escaped first, so anything left to warm up lands on the numerator and
# reads as a worse ratio rather than a better one.
my $escaped = fastest-of-three(escaped-source());
my $plain   = fastest-of-three(plain-source());

cmp-ok $escaped, '<', $plain * $budget,
  "an escaped string parses within $budget times the cost of the same"
    ~ " string unescaped ({($escaped / $plain).round(0.1)}x)";

# vim: expandtab shiftwidth=4
