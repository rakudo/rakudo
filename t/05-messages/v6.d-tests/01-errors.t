use Test;

plan 1;

# https://github.com/rakudo/rakudo/issues/1323
throws-like { await 42 }, Exception, 'giving await non-Awaitable things throws';

# vim: expandtab shiftwidth=4
