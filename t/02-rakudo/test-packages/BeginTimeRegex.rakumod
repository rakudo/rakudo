unit module BeginTimeRegex;

# A regex created at BEGIN time reaches the precompilation as a
# serialized value, so everything it invokes at match time must live in
# its own frame; a compile-time frame instance does not survive.

my $capture-rx = BEGIN / (\w+) '-' (\d+) /;
our sub parse-capture($s) {
    $s ~~ $capture-rx ?? "$0|$1" !! "no-match"
}

my $block-rx = BEGIN / (\w) { make "made:" ~ $/ } /;
our sub parse-make($s) {
    $block-rx.ACCEPTS($s).made
}

my $nested-rx = BEGIN / ( a (b) ) /;
our sub parse-nested($s) {
    $s ~~ $nested-rx ?? "$0|$0[0]" !! "no-match"
}
