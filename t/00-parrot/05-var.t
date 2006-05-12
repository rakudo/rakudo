#!./parrot

# check variables

use v6;

say '1..12';

my $o1 = 'ok 1'; say $o1;

my $o2; $o2 = 'ok 2'; say $o2;

my $a = 3; print 'ok '; say $a;

my $b; $b = 4; print 'ok '; say $b;

our $x = 5;  say 'ok ', $x;

{ my $x = 6; say 'ok ', $x; };

if ($x + 2 == 7)  { say 'ok ', $x + 2; }

{ my $x = 999; { our $x; say 'ok ', $x + 3; } }


##   variable interpolation in strings

$b = 9;  "ok $b" eq 'ok 9' and say 'ok 9';

'ok $b' ne 'ok 9' and say 'ok 10';

$b = "0x0b";  "ok $b" eq 'ok 0x0b' and say 'ok 11';


##   nested 'our' declarations

our $x = 'not ok 12';  { our $x = 'ok 12'; };  say $x;

