#!/usr/bin/pugs

# Checking that testing is sane: TAP output

use v6;

say '1..10';

say 'ok 1';
say "ok 2";

say 'ok';
say '# comment';
say 'ok ', '4';
say "ok", " " ~ "5";

say 'ok 6 foo';
say 'ok 7 # skip';
say 'ok 8 # skip bar';
say 'not ok 9 # TODO';
say 'not ok 10 # TODO baz';
