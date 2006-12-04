use v6-alpha;


say "1..3";

{
    my $string = "Pugs";
    if $string.isa("Str") { say "ok 1" } else { say "not ok 1" }
}

{
    my $num = 3.141;
    if $num.isa("Num")    { say "ok 2" } else { say "not ok 2" }
}

{
    my $code = { 42 };
    if $code.isa("Code")  { say "ok 3" } else { say "not ok 3" }
}
