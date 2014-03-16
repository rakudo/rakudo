
# L<S01/"Random Thoughts"/specifically tell it you're running Perl 6>
use v6;


# Checking that testing is sane: TAP output

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
