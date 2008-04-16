use v6;


say "1..3";

my $string = "Pugs -- Perl6 User's Golfing System";
my $substr = substr $string, 8, 5;

if $substr eq "Perl6" { say "ok 1" } else { say "not ok 1" }

my $str = 'not ok ';
say substr( $str, 4 ) ~ "2";
say substr( $str, 4 ), "3";
