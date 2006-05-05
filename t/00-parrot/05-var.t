#!./parrot

# check variables

use v6;

say '1..8';

my $o1 = 'ok 1'; say $o1;

my $o2; $o2 = 'ok 2'; say $o2;

my $a = 3; print 'ok '; say $a;

my $b; $b = 4; print 'ok '; say $b;

our $x = 5;  say 'ok ', $x;

{ my $x = 6; say 'ok ', $x; };

if ($x + 2 == 7)  { say 'ok ', $x + 2; }

{ my $x = 999; { our $x; say 'ok ', $x + 3; } }


