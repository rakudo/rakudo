use v6-alpha;


say "1..2";

my $var;
if defined $var { say "not ok 1" } else { say "ok 1" }

$var = "Pugs";
if defined $var { say "ok 2" } else { say "not ok 2" }
