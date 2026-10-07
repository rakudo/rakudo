use v6.e.PREVIEW;
use Test;

plan 7;

my @a[2;3] = (1,2,3),(4,5,6);
is-deeply @a[*;0], (1, 4),
    'a whatever star in a dimension of a shaped array takes each of its indices';
is-deeply (@a[1;**], @a[0..*;0]), ((4, 5, 6), (1, 4)),
    'a trailing hyper whatever star and a lazy index take the indices within the shape';
is-deeply (@a[0..2;0]:exists, @a[0,2;0]:k), ((True, True, False), ((0, 0),)),
    'an index past a dimension is one the array does not have';
my @c[2;2;2] = ((1,2),(3,4)),((5,6),(7,8));
is-deeply @c[*;1], (3, 4, 7, 8),
    'a dimension left out of a multidimensional slice takes each of its indices';

my @b[2;2] = (1,2),(3,4);
is-deeply @b[0;*]:delete:k, ((0, 0), (0, 1)),
    'a slice of a shaped array takes :delete with :k';
is-deeply (@b[0;0]:exists, @b[1;0]:exists), (False, True),
    'deleting a slice of a shaped array deletes its elements';

my int @n[2;2] = (1,2),(3,4);
is-deeply @n[*;0], (1, 3),
    'a whatever star in a dimension of a native array takes each of its indices';

# vim: expandtab shiftwidth=4
