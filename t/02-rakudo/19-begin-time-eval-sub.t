use lib <t/packages/02-rakudo/lib>;
use Test;

use StringEvalSubAtBegin;
is foo(), 'from-precomped-string-eval',
    'precompiling a module returning a string-EVAL Sub from BEGIN works';

done-testing;

# vim: expandtab shiftwidth=4
