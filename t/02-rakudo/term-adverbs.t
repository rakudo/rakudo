use Test;

plan 3;

proto term:<FOO>(*%) {*}
multi term:<FOO>(Int :$of!) { "of=$of" }
multi term:<FOO>(Str :$to!) { "to=$to" }

is FOO:of(5),         'of=5',    'colonpair adverb on a custom term becomes a named arg';
is FOO:to("x"),       'to=x',    'a different adverb selects a different term multi';
is FOO:to(FOO:of(5)), 'to=of=5', 'adverbed terms nest as arguments';

# vim: expandtab shiftwidth=4
