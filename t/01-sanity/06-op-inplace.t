use v6;

# check inplace math ops
say '1..11';

my $test_num = 1;
my $a = 0;
$a += 1;
$a != 1 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

++$a;
$a != 2 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a = 4;
$a -= 1;
$a != 3 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;


my $b = 1;
$a += $b;
$a != 4 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a +|= $b;
$a != 5 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a +&= +^$b;
$a +^= 2;
$a != 6 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a++;
$a != 7 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a = 1;
$a +<= 3;
$a != 8 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a +>= 1;
$a -= 1;
$a **= 2;
$a != 9 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a /= 3;
$a += 7;
$a != 10 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

$a %= 3;
$a != 1 and print 'not ';
say "ok $test_num";
$test_num = $test_num + 1;

# vim: expandtab shiftwidth=4
