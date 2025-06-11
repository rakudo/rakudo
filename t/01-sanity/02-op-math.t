# check basic math ops
say '1..14';

# testing basic operators
print 'ok ';
say 0 + 1;
print 'ok ';
say 1 + 1;
print 'ok ';
say 4 - 1;
print 'ok ';
say 2 * 2;

# testing different assignment operators
# Result should be 5
print 'ok ';
say 4 +| 1;
print 'ok ';
say 7 +& +^1;

print 'ok ';
say 15 +^ 8;
print 'ok ';
say 2 ** 3;

# testing square operator and +> operator
# Result should be 9
print 'ok ';
say 3 ** 2;
print 'ok ';
say 20 +> 1;

# testing modulo and +< operators
print 'ok ';
say 5 +< 1 + 1;
print 'ok ';
say 25 % 13;

# testing double negative and abs operator
# Result should be 13
print 'ok ';
say -(-13);
print 'ok ';
say abs -14;

# vim: expandtab shiftwidth=4
