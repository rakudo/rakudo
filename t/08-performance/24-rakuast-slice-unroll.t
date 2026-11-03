use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 12;

# A slice of a plain variable by literal integer indexes becomes the list
# of the AT-POS calls those indexes dispatch to. An assignment target, a
# runtime index, and an adverbed slice keep the postcircumfix call.

# These observe the emitted QAST.
qast-is 'my @a = 1..9; my ($x, $y) = @a[1, 3]', -> \v {
    qast-contains-callmethod(v, 'AT-POS')
}, 'a literal integer slice unrolls to AT-POS calls';

qast-is 'my @a = 1..9; @a[1, 3] = 90, 91', -> \v {
    not qast-contains-callmethod(v, 'AT-POS')
}, 'a slice assignment target keeps the postcircumfix call';

qast-is 'my @a = 1..9; my $i = 1; my ($x, $y) = @a[$i, 3]', -> \v {
    not qast-contains-callmethod(v, 'AT-POS')
}, 'a runtime index keeps the postcircumfix call';

qast-is 'my @a = 1..9; my @s = @a[1, 3]:v', -> \v {
    not qast-contains-callmethod(v, 'AT-POS')
}, 'an adverbed slice keeps the postcircumfix call';

# These observe that slices still behave.
{
    my @a = 10, 20, 30, 40;
    is @a[1, 3].join(','), '20,40', 'an unrolled slice gives its values';
    is @a[0, 2, 3].join(','), '10,30,40', 'a three-index slice gives its values';
    my ($p, $q) = @a[1, 2];
    is "$p $q", '20 30', 'an unrolled slice list-binds';
}
{
    my @a = 1, 2;
    is @a[0, 5].elems, 2, 'an out-of-range index still yields a slot';
    nok @a[0, 5][1].defined, 'the out-of-range value is undefined';
}
{
    my @a = 10, 20, 30, 40;
    @a[1, 3] = 91, 92;
    is @a.join(','), '10,91,30,92', 'slice assignment still stores';
    @a[1, 5] = 81, 82;
    is @a[5], 82, 'slice assignment past the end still extends';
}
{
    my @a = 10, 20, 30;
    my $r = @a[1, 2];
    is $r.elems, 2, 'a slice result has slice shape';
}

# vim: expandtab shiftwidth=4
