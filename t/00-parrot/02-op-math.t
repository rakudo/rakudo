#!./parrot perl6.pbc

# check basic math ops

use v6;

say '1..14';

print 'ok ';
say 0 + 1;
print 'ok ';
say 1 + 1;
print 'ok ';
say 4 - 1;
print 'ok ';
say 2 * 2;

# 5
print 'ok ';
say 4 +| 1;
print 'ok ';
say 7 +& +^1;

print 'ok ';
say 15 +^ 8;
print 'ok ';
say 2 ** 3;

# 9
print 'ok ';
say 3 ** 2;
print 'ok ';
say 20 +> 1;

print 'ok ';
say 5 +< 1 + 1;
print 'ok ';
say 25 % 13;

# 13
print 'ok ';
say -(-13);
print 'ok ';
say abs -14;
