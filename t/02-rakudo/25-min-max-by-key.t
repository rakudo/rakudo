use Test;

plan 15;

# A 1-arg &by passed to .min / .max is a key function, not a comparator.
# It must be called once per value rather than twice per comparison,
# which matters when the key function is expensive or has side effects.

{
    my int $calls;
    my sub chars-of($value) { ++$calls; $value.chars }

    $calls = 0;
    is-deeply (^100).Array.min(&chars-of), 0,
        '.min with a key function finds the first minimal value';
    is $calls, 100,
        '.min called the key function once per value';

    $calls = 0;
    is-deeply (^100).Array.max(&chars-of), 10,
        '.max with a key function finds the first maximal value';
    is $calls, 100,
        '.max called the key function once per value';

    $calls = 0;
    is-deeply (42,).min(&chars-of), 42,
        '.min of a single value returns that value';
    is $calls, 1,
        '.min of a single value called the key function once';
}

{
    my int $calls;
    my sub abs-of($value) { ++$calls; $value.abs }

    $calls = 0;
    is-deeply (Int, 3, 1, -5).min(&abs-of), 1,
        '.min with a key function skips type objects';
    is $calls, 3,
        '.min never called the key function on a type object';
}

{
    my int $calls;

    $calls = 0;
    is-deeply (^100).Array.max({ ++$calls; .chars }), 10,
        '.max with a block using the topic finds the maximum';
    is $calls, 100,
        '.max called the topic block once per value';
}

{
    my int $calls;
    my sub compare($a, $b) { ++$calls; $a <=> $b }

    $calls = 0;
    is-deeply (3, 1, 4, 1, 5).min(&compare), 1,
        '.min with a 2-arg comparator finds the minimum';
    is $calls, 4,
        '.min called the comparator once per value after the first';

    $calls = 0;
    is-deeply (3, 1, 4, 1, 5).max(&compare), 5,
        '.max with a 2-arg comparator finds the maximum';
    is $calls, 4,
        '.max called the comparator once per value after the first';
}

is-deeply <ab cd e>.max(*.chars), 'ab',
    '.max with a key function keeps the first of tied values';

# vim: expandtab shiftwidth=4
