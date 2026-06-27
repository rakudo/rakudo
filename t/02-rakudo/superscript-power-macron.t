use Test;

plan 5;

# A negative superscript power exponent may be written with either the high
# minus `⁻` (U+207B) or the macron `¯` (U+00AF); the grammar accepts both as a
# super-sign, so both must yield the same negative exponent.
is 10¯²⁴, 1e-24, '10 with a macron-signed negative exponent';
is 10⁻²⁴, 1e-24, '10 with a high-minus negative exponent';
is-deeply (10¯²⁴), (10⁻²⁴), 'macron and high-minus exponents agree';
is 2¯³, 0.125, 'macron negative exponent on a small base';

# Positive exponents are unaffected.
is 10²⁴, 1000000000000000000000000, 'positive superscript exponent';

# vim: expandtab shiftwidth=4
