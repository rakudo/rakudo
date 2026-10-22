use Test;

plan 1;

my @a = [5,6],[7,8];
is @a[1; ||(lazy 0,)], 7,
    'before 6.e a double slip of a lazy list in a subscript is not refused';

# vim: expandtab shiftwidth=4
