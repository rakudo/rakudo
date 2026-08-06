use v6;
use Test;

# C99 hexadecimal floating point literals, e.g. 0x1.8p+1

plan 20;

is-deeply 0x1.8p+1, 3e0,     'basic hexfloat with fraction';
is-deeply 0x1p+0,   1e0,     'hexfloat without fraction';
is-deeply 0x1p-2,   0.25e0,  'negative exponent';
is-deeply 0x.8p+1,  1e0,     'hexfloat without integer part';
is-deeply 0x1.8P4,  24e0,    'capital P exponent, no sign';
is-deeply 0x1.999999999999ap-4, 0.1e0, 'full-precision mantissa';
is-deeply 0xdead.beefp0, (0xdead + 0xbeef / 65536).Num, 'multi-digit parts';
is-deeply -0x1.8p+1, -3e0,   'negated hexfloat';
is-deeply 0x1.fffffffffffffp+1023, 1.7976931348623157e308, 'largest double';
is-deeply 0x1p-1022,  2.2250738585072014e-308, 'smallest normal';
is-deeply 0x1p-1074,  5e-324, 'smallest subnormal';
is-deeply 0x1.8p-1074, 1e-323, 'subnormal tie rounds to even';
is-deeply 0x1p+9999,  Inf,   'exponent overflow gives Inf';
is-deeply 0x1p-9999,  0e0,   'exponent underflow gives 0';
is-deeply 0xde_ad.be_efp+0, 0xdead.beefp0, 'underscores in mantissa';
is-deeply 0x1p+1_0, 1024e0,  'underscore in exponent';
ok (-0x0p+0) === -0e0,       'negative zero survives';

# unchanged behaviors
is-deeply 0x10, 16,     'plain hex integer literal is still an Int';
is-deeply 0x1.abs, 1,   'method call on hex integer literal still works';
throws-like { EVAL '0x1.8' }, Exception, 'hexfloat without p exponent is still an error';

# vim: expandtab shiftwidth=4
