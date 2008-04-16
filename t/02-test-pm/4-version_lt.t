use v6;

use Test;

plan 6;

ok Test::version_lt('6.2.12', '6.2.13');
ok Test::version_lt('6.2.13', '6.28.0');
ok Test::version_lt('6.2.13', '6.28');
ok Test::version_lt('0.42', '0.50');
ok Test::version_lt('0.001', '0.002');
ok Test::version_lt('-10.001', '0.000');
