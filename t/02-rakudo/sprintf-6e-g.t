use v6.e.PREVIEW;
use Test;

# The %g directive of the 6.e sprintf, and the zero and negative zero
# handling shared by the float directives.  Every expected string
# matches the output of a C printf for the same format and double
# value, except that Inf and NaN render in their Raku spellings.

plan 34;

# %g treats the precision as significant digits and picks the notation
# from the decimal exponent of the rounded value
is sprintf('%g', 123456789),  '1.23457e+08', '%g of a large integer';
is sprintf('%g', 0.0001234),  '0.0001234',   '%g of a small fraction';
is sprintf('%g', 999999.5e0), '1e+06',       '%g rounding across the notation threshold';
is sprintf('%g', 1000000),    '1e+06',       '%g at the notation threshold';
is sprintf('%g', 100),        '100',         '%g of a round number strips zeroes';
is sprintf('%g', 0.5),        '0.5',         '%g of one half';
is sprintf('%g', 1e-5),       '1e-05',       '%g below the fraction threshold';
is sprintf('%g', 9.9999999e-5), '0.0001',
    '%g carry across the fraction threshold';
is sprintf('%g', 123456.789), '123457',      '%g rounding to a whole number';
is sprintf('%.3g', 1234),     '1.23e+03',    '%.3g of an integer';
is sprintf('%.0g', 7),        '7',           '%.0g means one significant digit';
is sprintf('%.*g', 3, 1234),  '1.23e+03',    'dynamic precision works as a literal one';
is sprintf('%g', 6.02214076e23), '6.02214e+23', '%g of a large double';
is sprintf('%.17g', 1e-298), '9.9999999999999991e-299',
    '%.17g below an exact power of ten keeps its exponent';
is sprintf('%G', 123456789),  '1.23457E+08', '%G uses an uppercase exponent letter';
is sprintf('%G', 1e-5),       '1E-05',       '%G of a small value';

# Negative values keep their digits and their sign
is sprintf('%g', -0.5),       '-0.5',         '%g of a negative fraction';
is sprintf('%g', -123456789), '-1.23457e+08', '%g of a large negative integer';
is sprintf('%g', -100),       '-100',         '%g of a negative round number';

# The alternate form keeps insignificant zeroes and the radix point
is sprintf(Q[%#.0g], 7),       '7.',      '%#.0g keeps the radix point';
is sprintf(Q[%#.0g], 0),       '0.',      '%#.0g of zero keeps the radix point';
is sprintf(Q[%#g], 1.5),       '1.50000', '%#g keeps insignificant zeroes';
is sprintf(Q[%#g], 0),         '0.00000', '%#g of zero keeps insignificant zeroes';
is sprintf(Q[%#g], 123456789), '1.23457e+08', '%#g with all digits significant';
is sprintf(Q[%#.3g], 100),     '100.',    '%#.3g keeps only the radix point';

# A zero value renders through the same path as any other value, so
# the signer, width and the sign of a negative zero reach it
is sprintf('%f', -0e0),    '-0.000000',       '%f of -0e0';
is sprintf('%e', -0e0),    '-0.000000e+00',   '%e of -0e0';
is sprintf('%g', -0e0),    '-0',              '%g of -0e0';
is sprintf('%15e', -0e0),  '  -0.000000e+00', 'right-justified %e of -0e0';
is sprintf('%-12f', -0e0), '-0.000000   ',    'left-justified %f of -0e0';
is sprintf('%+g', 0),      '+0',              '%g of zero with plus flag';
is sprintf('%15g', 0),     '              0', '%g of zero with width';

# Inf and NaN pass through untouched
is sprintf('%g', Inf), 'Inf', '%g of Inf';
is sprintf('%G', NaN), 'NAN', '%G uppercases NaN';

# vim: expandtab shiftwidth=4
