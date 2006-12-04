use v6-alpha;


say "1..4";

my $string = "foo!bar!baz";
my @array  = split "!", $string;

if @array[0] eq "foo" { say "ok 1" } else { say "not ok 1" }
if @array[1] eq "bar" { say "ok 2" } else { say "not ok 2" }
if @array[2] eq "baz" { say "ok 3" } else { say "not ok 3" }
if +@array   ==     3 { say "ok 4" } else { say "not ok 4" }
