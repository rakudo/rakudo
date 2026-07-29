use lib <t/02-rakudo/test-packages>;
use Test;
use BeginTimeRegex;

plan 3;

# Loading the module precompiles it, so these BEGIN-created regexes come
# back through serialization and must still reach their capture-group
# and code-block closures at match time.

is BeginTimeRegex::parse-capture('abc-12'), 'abc|12',
    'the capture groups of a precompiled BEGIN regex match';
is BeginTimeRegex::parse-make('x'), 'made:x',
    'a code block of a precompiled BEGIN regex runs and makes';
is BeginTimeRegex::parse-nested('ab'), 'ab|b',
    'a nested capture group of a precompiled BEGIN regex matches';
