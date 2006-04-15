#!/usr/bin/pugs

# Checking that testing is sane: use

use v6;

# We've to output the TAP header at begin time to ensure it is outputted,
# as the use() below might not work, causing this program to not even compile,
# causing the TAP header to not be printed.
#   BEGIN { say '1..1' }
# --iblech, 2005-06-14

# Ok. PIL2JS uses pugs -CPIL ... to get the PIL of a program.
# But the "1..1" is outputted at compile-time, too, in addition of the PIL
# tree. So now, I removed the BEGIN and put the say "1..1" back into normal
# runtime. "But then, when the compilation fails, the TAP header is not
# printed?" -- yes. *But*: The test will still not succeed, as a. the header is
# not printed, and b. there's no "ok 1" line. Therefore it's ok to output the
# plan at runtime.  --iblech, 2005-09-19.
say '1..1';

# We try to load an arbitrary module.
use lib ".";

say 'ok 1';
