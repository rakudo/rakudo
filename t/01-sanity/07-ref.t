use v6-alpha;


say "1..3";

{
    my $string = "Pugs";
    if $string.WHAT eq Str { say "ok 1" } else { say "not ok 1" }
}

{
    my $bool = ?1;
    if $bool.WHAT eq Bool { say "ok 2" } else { say "not ok 2 # TODO" }
}

{
    my $bool = Bool::True;
    if $bool.WHAT eq Bool { say "ok 3" } else { say "not ok 3 # TODO" }
}
