use v6.e.PREVIEW;
use Test;

# The %f directive of the 6.e sprintf.  Every expected string matches
# the output of a C printf for the same format and double value,
# except that Inf and NaN render in their Raku spellings.

plan 22;

# Small fractions stay right-aligned in the fraction digits
is sprintf('%f', 1e-5),     '0.000010',  '%f of 1e-5';
is sprintf('%f', 1e-6),     '0.000001',  '%f of 1e-6';
is sprintf('%f', -1e-5),    '-0.000010', '%f of negative small value';
is sprintf('%f', 1/100000), '0.000010',  '%f of a small Rat';

# The leading zero and the sign survive
is sprintf('%f', -0.25),  '-0.250000', '%f of -0.25';
is sprintf('%.2f', -1/3), '-0.33',     '%.2f of negative Rat';
is sprintf('%#.0f', -0.4), '-0.',      '%#.0f rounding to negative zero';
is sprintf('%.0f', -0.4),  '-0',       '%.0f rounding to negative zero';
is sprintf('%08.2f', -1.5), '-0001.50', 'the sign leads the zero fill';

# The digits are the exact decimal expansion of the double
is sprintf('%f', 6.02214076e23), '602214075999999987023872.000000',
    '%f renders the exact decimal expansion of a large double';
is sprintf('%.20f', 0.1e0), '0.10000000000000000555',
    '%.20f of one tenth shows the digits of the double';
is sprintf('%.16f', 0.9999999999999999e0), '0.9999999999999999',
    '%.16f just below one does not round up';

# Ties round to even, as C rounds the exact value of the double
is sprintf('%.0f', 2.5e0),  '2',   'tie 2.5 rounds to even';
is sprintf('%.0f', -2.5e0), '-2',  'tie -2.5 rounds to even';
is sprintf('%.0f', -0.5e0), '-0',  'tie -0.5 rounds to even zero';
is sprintf('%.2f', 0.125e0), '0.12', 'tie 0.125 rounds to even';
is sprintf('%.1f', 0.25e0), '0.2', 'tie 0.25 rounds down to even';
is sprintf('%.1f', 0.75e0), '0.8', 'tie 0.75 rounds up to even';

# Inf and NaN pass through untouched
is sprintf('%f', Inf),    'Inf',        '%f of Inf';
is sprintf('%F', -Inf),   '-INF',       '%F uppercases -Inf';
is sprintf('%010f', Inf), '       Inf', 'zero fill falls back to spaces for Inf';
is sprintf('%+f', NaN),   'NaN',        'the plus flag is not applied to NaN';

# vim: expandtab shiftwidth=4
