use v6.e.PREVIEW;
use Test;

# Passing Mu as the actions means: no actions, also for subparse

plan 3;

grammar G {
    token TOP { \w+ }
}

is G.subparse("word", actions => Mu).Str, 'word',
    'subparse accepts Mu as the actions';
is G.parse("word", actions => Mu).Str, 'word',
    'parse accepts Mu as the actions';

my class Actions {
    method TOP($/) { make ~$/ ~ '!' }
}

is G.subparse("word", actions => Actions).made, 'word!',
    'subparse still runs a real actions class';

# vim: expandtab shiftwidth=4
