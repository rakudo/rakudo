use v6;


say "1..3";

my $a  = 42;
my $b := $a;

if $b == $a { say "ok 1" } else { say "not ok 1" }

$b = 23;
if $a == 23 { say "ok 2" } else { say "not ok 2" }

$a = 17;
if $b == 17 { say "ok 3" } else { say "not ok 3" }
