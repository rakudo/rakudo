use v6;


say "1..3";

my $var;
if defined $var { say "not ok 1" } else { say "ok 1" }

$var = "Pugs";
if defined $var { say "ok 2" } else { say "not ok 2" }

undefine( $var);

if defined $var { say "not ok 3 - $var" } else { say "ok 3" }
