use Test;

plan 6;

# A supply whose whole body is a single `emit EXPR` is lowered to
# SUPPLY-ONE-EMIT, whose tappable emits the block's return value and quits
# when evaluating the block throws or fails.

# The lowered form still emits its value.
is do {
    my @got;
    react {
        whenever (supply emit 42) -> $v { @got.push($v) }
    }
    @got.join(',')
}, '42', 'a single-emit supply statement emits its value';

# The block form is lowered the same way.
is do {
    my @got;
    react {
        whenever (supply { emit 42 }) -> $v { @got.push($v) }
    }
    @got.join(',')
}, '42', 'a single-emit supply block emits its value';

# A Failure produced while evaluating the emitted expression quits the
# supply instead of being emitted as a value.
{
    sub failing() { fail "boom" }
    my @got;
    dies-ok {
        react {
            whenever (supply emit failing()) -> $v { @got.push($v.WHAT.^name) }
        }
    }, 'a Failure from the emitted expression quits the supply';
    is @got.join(','), '', 'the Failure was not emitted as a value';
}

# A supply with more than one emit is not lowered and still works.
is do {
    my @got;
    react {
        whenever (supply { emit 1; emit 2 }) -> $v { @got.push($v) }
    }
    @got.join(',')
}, '1,2', 'a two-emit supply block emits both values';

# A statement modifier keeps the generic supply path and still works.
is do {
    my @got;
    react {
        whenever (supply emit 42 if True) -> $v { @got.push($v) }
    }
    @got.join(',')
}, '42', 'a single emit with a statement modifier emits its value';

# vim: expandtab shiftwidth=4
