#!./parrot perl6.pbc

# check inplace math ops

use v6;

say '1..11';

my $a = 0;
$a += 1;
print 'ok ';
say $a;
++$a;
print 'ok ';
say $a;
$a = 4;
$a -= 1;
print 'ok ';
say $a;
my $b = 1;
$a += $b;
print 'ok ';
say $a;
# 5

$a +|= $b;
print 'ok ';
say $a;
$a +&= +^$b;
$a +^= 2;
print 'ok ';
say $a;
$a++;
print 'ok ';
say $a;
$a = 1;
$a +<= 3;
print 'ok ';
say $a;

#9
$a +>= 1;
$a -= 1;
$a **= 2;
print 'ok ';
say $a;

$a /= 3;
$a += 7;
print 'ok ';
say $a;

$a %= 3;
print 'ok ';
say $a + 10;
