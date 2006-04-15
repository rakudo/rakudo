#!/usr/bin/pugs

use v6;

say "1..3";

{
    my $string = "Pugs";
    if $string.ref eq "Str" { say "ok 1" } else { say "not ok 1" }
}

{
    my $bool = ?1;
    if $bool.ref eq "Bool" { say "ok 2" } else { say "not ok 2" }
}

{
    my $bool = bool::true;
    if $bool.ref eq "Bool" { say "ok 3" } else { say "not ok 3" }
}
