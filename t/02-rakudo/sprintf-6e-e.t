use v6.e.PREVIEW;
use Test;

# The %e directive of the 6.e sprintf.  Every expected string matches
# the output of a C printf for the same format and double value,
# except that Inf and NaN render in their Raku spellings.

plan 19;

is sprintf('%e', 999999.99e0), '1.000000e+06', '%e carry into next magnitude';
is sprintf('%e', 1000000),     '1.000000e+06', '%e of an exact power of ten';
is sprintf('%.0e', 999999),    '1e+06',        '%.0e carry with zero precision';
is sprintf('%e', 0.99999999e0), '1.000000e+00',
    '%e carry across the exponent sign';
is sprintf('%e', 9.9999999e-5), '1.000000e-04',
    '%e carry at a negative exponent';

is sprintf('%e', 6.02214076e23), '6.022141e+23',  '%e of a large double';
is sprintf('%e', 1.6e-19),       '1.600000e-19',  '%e of a small double';
is sprintf('%e', 5e-324),        '4.940656e-324', '%e of the smallest subnormal';
is sprintf('%e', 1e19),          '1.000000e+19',  '%e at the native integer boundary';

# The digits are the digits of the double, not of a shorter value
# that would round-trip to it
is sprintf('%.20e', 0.1e0), '1.00000000000000005551e-01',
    '%.20e of one tenth shows the digits of the double';
is sprintf('%.3e', 0.00012345e0), '1.234e-04',
    '%.3e rounds the digits of the double, not of the literal';
is sprintf('%.16e', 1e-298), '9.9999999999999991e-299',
    '%.16e below an exact power of ten keeps its exponent';
is sprintf('%.15e', 1e-310), '9.999999999999969e-311',
    '%.15e of a subnormal keeps its exponent';

# Exact types render exactly
is sprintf('%e', 1/3),    '3.333333e-01', '%e of a Rat';
is sprintf('%e', 10**24), '1.000000e+24', '%e of a large Int';

is sprintf('%E', 1.6e-19),   '1.600000E-19', '%E uses an uppercase exponent letter';
is sprintf(Q[%#.0e], 9.99e0), '1.e+01',      '%#.0e keeps the radix point';
is sprintf('%.*e', -1, 2.5e0), '2.500000e+00',
    'a negative dynamic precision acts as an omitted one';

is sprintf('%e', NaN), 'NaN', '%e of NaN';

# vim: expandtab shiftwidth=4
