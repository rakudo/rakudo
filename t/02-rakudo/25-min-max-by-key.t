use Test;

plan 32;

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

{
    my int $calls;
    my sub chars-of($value) { ++$calls; $value.chars }

    $calls = 0;
    is-deeply (^10).Array.minpairs(&chars-of), ((^10).map({ $_ => $_ })).List,
        '.minpairs with a key function keeps all tied values';
    is $calls, 10,
        '.minpairs called the key function once per value';

    $calls = 0;
    is-deeply (9, 10, 11, 8).maxpairs(&chars-of), (1 => 10, 2 => 11),
        '.maxpairs with a key function keeps all maximal values';
    is $calls, 4,
        '.maxpairs called the key function once per value';

    $calls = 0;
    is-deeply (^100).Array.min(&chars-of, :k), (^10).List,
        '.min(:k) with a key function returns the keys of the minimal values';
    is $calls, 100,
        '.min(:k) called the key function once per value';

    $calls = 0;
    is-deeply (^100).Array.max(&chars-of, :v), (10..99).List,
        '.max(:v) with a key function returns the maximal values';
    is $calls, 100,
        '.max(:v) called the key function once per value';
}

{
    my int $calls;
    my sub compare($a, $b) { ++$calls; $a <=> $b }

    $calls = 0;
    is-deeply (3, 1, 4, 1).minpairs(&compare), (1 => 1, 3 => 1),
        '.minpairs with a 2-arg comparator keeps all minimal values';
    is $calls, 3,
        '.minpairs called the comparator once per value after the first';
}

{
    my int $calls;
    my sub chars-of($value) { ++$calls; $value.chars }

    $calls = 0;
    is-deeply (^100).Array.minmax(&chars-of), 0..10,
        '.minmax with a key function finds the minimal and maximal values';
    is $calls, 100,
        '.minmax called the key function once per value';
}

{
    my int $calls;
    my sub abs-of($value) { ++$calls; $value.abs }

    $calls = 0;
    is-deeply (1..3, 9, -7).minmax(&abs-of), 1..9,
        '.minmax with a key function compares Range values by their endpoints';
    is $calls, 4,
        '.minmax called the key function once per endpoint and value';
}

is-deeply (1^..5, 7).minmax(*.self), 1^..7,
    '.minmax with a key function preserves endpoint exclusions';

is-deeply ((3, 9), 1).minmax(*.abs), 1..9,
    '.minmax with a key function recurses into Positional values';

is-deeply (3, 1, 4, 5).minmax(-> $a, $b { $a <=> $b }), 1..5,
    '.minmax with a 2-arg comparator finds the minimal and maximal values';

# vim: expandtab shiftwidth=4
