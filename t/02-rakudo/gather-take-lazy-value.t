use Test;

plan 5;

# `take` hands back the same value it stashes into the gather. Sinking that
# result would drain a lazy value out from under the gather, so a lazy value
# taken in a sunk position must survive to be pulled from the gather.

{
    my @vlp = gather for (60, 64, 67) -> $v {
        take (61, 65, 68).map: * - $v;
    }
    is @vlp.map(*.join(',')).join('; '), '1,5,8; -3,1,4; -6,-2,1',
        'a lazy Seq taken as a sunk for-loop body survives';
}

{
    my @taken = gather for 1, 2 -> $v {
        take (10, 20).map: * + $v;
    }
    is @taken.join('; '), '11 21; 12 22',
        'each taken lazy Seq keeps its own values';
}

{
    my @r = gather {
        my $first = take 5;
        take $first + 1;
    }
    is-deeply @r.List, (5, 6), 'the value returned by take is still usable';
}

{
    my @a = 1, 2, 3;
    .=succ for gather { take-rw @a[1] }
    is @a.join(','), '1,3,3', 'take-rw still yields a writable container';
}

{
    my $iterated = 0;
    my sub take($x) { (^2).map: { ++$iterated } }
    take 1;
    is $iterated, 2,
        'a user routine named take is sunk like any other call';
}
