use Test;

plan 3;

# `A xx *` produces a lazy Seq, so when its value is sunk (here, the final
# statement of a gather block) it must be iterated for the thunk to run each
# time. Bounded with head so the infinite repetition terminates.
is (gather do { take 1 } xx *).head(3).elems, 3,
    'takes from the thunk of a sunk `xx *` reach the enclosing gather';

# The same lazy repetition run through an explicit sink also fires its thunk.
{
    my $count = 0;
    (do { $count++ } xx *).head(3).sink;
    is $count, 3, 'a bounded lazy `xx *` runs its thunk when sunk';
}

# A finite count reifies eagerly, so a sunk `A xx N` runs its thunk N times
# with no extra sink needed. Guards against over-correcting the lazy case.
{
    my $count = 0;
    do { $count++ } xx 4;
    is $count, 4, 'a finite `xx N` in sink context runs its thunk N times';
}
