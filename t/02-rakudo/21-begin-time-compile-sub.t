use lib <t/packages/02-rakudo/lib>;
use Test;

plan 2;

use CompilerEvalSubAtBegin;
is foo(), 'from-precomped-compiler-eval',
    'precompiling a module returning a Sub compiled through the compiler object from BEGIN works';
is bar(), 'from-precomped-compiler-unit',
    'the same with the compilation unit kept and its mainline taken by the caller';

# vim: expandtab shiftwidth=4
