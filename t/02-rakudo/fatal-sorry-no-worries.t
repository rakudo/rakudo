use Test;

plan 1;

# Under `use fatal`, a check-time problem that is a sorry with no accompanying
# worry must still report the sorry. Promoting worries to sorries should not
# iterate the worry list when none was ever recorded.

throws-like 'use fatal; sub f($a?, $b) { }', X::Comp,
    message => /'required parameter'/,
    'a sorry under fatal with no accompanying worry reports the sorry';

# vim: expandtab shiftwidth=4
