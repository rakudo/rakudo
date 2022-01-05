use v6;

say "1..3";

{
    my $string = "Pugs";
    if $string.isa("Str") { say "ok 1" } else { say "not ok 1" }
}

{
    my $int = 3;
    if $int.isa("Int")    { say "ok 2" } else { say "not ok 2 # TODO" }
}

{
    my $code = { 42 };
    if $code.isa("Code")  { say "ok 3" } else { say "not ok 3" }
}

# vim: expandtab shiftwidth=4
