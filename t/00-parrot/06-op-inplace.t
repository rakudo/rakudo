#!./parrot perl6.pbc

# check inplace math ops

use v6;

say '1..11';

my $test_num = 1;
my $a = 0;
$a += 1;
print 'not ' if $a != 1;
say "ok $test_num";
$test_num = $test_num + 1;

++$a;
print 'not ' if $a != 2;
say "ok $test_num";
$test_num = $test_num + 1;

$a = 4;
$a -= 1;

print 'not ' if $a != 3;
say "ok $test_num";
$test_num = $test_num + 1;


my $b = 1;
$a += $b;
print 'not ' if $a != 4;
say "ok $test_num";
$test_num = $test_num + 1;

$a +|= $b;
print 'not ' if $a != 5;
say "ok $test_num";
$test_num = $test_num + 1;

$a +&= +^$b;
$a +^= 2;
print 'not ' if $a != 6;
say "ok $test_num";
$test_num = $test_num + 1;

$a++;
print 'not ' if $a != 7;
say "ok $test_num";
$test_num = $test_num + 1;

$a = 1;
$a +<= 3;
print 'not ' if $a != 8;
say "ok $test_num";
$test_num = $test_num + 1;

$a +>= 1;
$a -= 1;
$a **= 2;
print 'not ' if $a != 9;
say "ok $test_num";
$test_num = $test_num + 1;

$a /= 3;
$a += 7;
print 'not ' if $a != 10;
say "ok $test_num";
$test_num = $test_num + 1;

$a %= 3;
print 'not ' if ( $a +10 ) != 11;
say "ok $test_num";
$test_num = $test_num + 1;
