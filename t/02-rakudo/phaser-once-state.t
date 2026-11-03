use Test;

plan 4;

# A `once` directly under a run-once phaser (INIT, BEGIN) keeps its state in
# the phaser's own thunk. RakuAST left that state in the enclosing scope, whose
# state init never fired for the thunk call, so the body was skipped.

my $init;
INIT once { $init = 'ran' }
is $init, 'ran', 'INIT once runs its body';

my $begin;
BEGIN once { $begin = 'ran' }
is $begin, 'ran', 'BEGIN once runs its body';

my $val = INIT once 42;
is $val, 42, 'INIT once yields its value';

my @log;
INIT once { @log.push: 'a'; @log.push: 'b' }
is @log.join(','), 'a,b', 'INIT once runs a multi-statement body';

# vim: expandtab shiftwidth=4
