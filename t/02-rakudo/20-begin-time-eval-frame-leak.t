use lib <t/packages/02-rakudo/lib>;
use Test;

use BeginFrameLeakEval;
is x(), 42,
    'BEGIN block with non-serializable lexicals before EVAL does not leak the BEGIN lexpad through the EVAL Sub';

done-testing;

# vim: expandtab shiftwidth=4
