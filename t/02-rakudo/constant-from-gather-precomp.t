use lib <t/02-rakudo/test-packages>;
use Test;
use ConstantFromGather;

plan 3;

# Loading the module precompiles it, so this constant comes back through
# serialization. A `%` gather constant materializes to a Map, so the value is
# correct and reusable across repeated access; a regression that left the
# gather block or a single-use Seq in the constant would fail here.

is ConstantFromGather::<%RULES><a>, 1,
    'a precompiled %-constant from gather holds the gathered pairs';
is ConstantFromGather::<%RULES>.elems, 2,
    'a precompiled %-constant from gather has all the pairs';
is ConstantFromGather::<%RULES><b>, 2,
    'a precompiled %-constant from gather is reusable across access';

# vim: expandtab shiftwidth=4
