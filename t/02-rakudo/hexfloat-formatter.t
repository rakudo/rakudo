use v6.e.PREVIEW;
use Test;

# %a / %A (C99 hexadecimal floating point) directives in the 6.e Formatter

plan 31;

is sprintf('%a', 0.1e0),  '0x1.999999999999ap-4', '%a';
is sprintf('%A', 0.1e0),  '0X1.999999999999AP-4', '%A';
is sprintf('%a', 1),      '0x1p+0',   '%a of Int drops radix point';
is sprintf('%a', 3e0),    '0x1.8p+1', '%a of 3e0';
is sprintf('%a', -3e0),   '-0x1.8p+1', '%a of negative value';
is sprintf('%a', 0e0),    '0x0p+0',  '%a of +0e0';
is sprintf('%a', -0e0),   '-0x0p+0', '%a of -0e0';
is sprintf('%a', 1.7976931348623157e308), '0x1.fffffffffffffp+1023', '%a of max double';
is sprintf('%a', 5e-324), '0x1p-1074', '%a of min subnormal is normalized';
is sprintf('%.0a', 0.1e0), '0x2p-4',     '%.0a rounds the mantissa';
is sprintf('%.3a', 0.1e0), '0x1.99ap-4', '%.3a rounds the mantissa';
is sprintf('%.13a', 0.1e0), '0x1.999999999999ap-4', '%.13a is the exact mantissa';
is sprintf('%.16a', 0.1e0), '0x1.999999999999a000p-4', '%.16a pads with zeroes';
is sprintf('%.3a', 0e0), '0x0.000p+0', '%.3a of zero fills up to precision';
is sprintf('%.0a', 1.9375e0), '0x2p+0',   'rounding carries into the leading digit';
is sprintf('%.1a', 255.5e0),  '0x2.0p+7', 'rounding carries through all digits';
is sprintf('%.0a', 1.5e0),     '0x2p+0',   'tie rounds to even (up)';
is sprintf('%.1a', 1.09375e0), '0x1.2p+0', 'tie 0x1.18 rounds to even (up)';
is sprintf('%.1a', 1.15625e0), '0x1.2p+0', 'tie 0x1.28 rounds to even (down)';
is sprintf('%+a', 3e0), '+0x1.8p+1', '%a with plus flag';
is sprintf('% a', 3e0), ' 0x1.8p+1', '%a with space flag';
is sprintf('%#a', 1e0), '0x1.p+0',   '%a with hash flag keeps radix point';
is sprintf('<%20a>', 1.5e0),  '<            0x1.8p+0>', 'right-justified %a';
is sprintf('<%-20a>', 1.5e0), '<0x1.8p+0            >', 'left-justified %a';
is sprintf('<%020a>', 1.5e0), '<0x0000000000001.8p+0>', '0-padding goes after the 0x prefix';
is sprintf('<%020a>', -1.5e0), '<-0x000000000001.8p+0>', '0-padding of negative value';
is sprintf('%*.*a', 12, 3, 1.5e0), '  0x1.800p+0', 'star width and precision';
is sprintf(Q[%2$a %1$a], 1e0, 2e0), '0x1p+1 0x1p+0', 'positional parameters';
is sprintf('%a', Inf),  'Inf',  '%a of Inf';
is sprintf('%a', -Inf), '-Inf', '%a of -Inf';
is sprintf('<%010a>', NaN), '<       NaN>', '%a of NaN is padded with spaces';

# vim: expandtab shiftwidth=4
