use lib <t/packages/02-rakudo/lib>;
use Test;

BEGIN if $*VM.name eq 'jvm' {
    plan :skip-all<java.lang.ArrayIndexOutOfBoundsException: Index 7 out of bounds for length 7>;
}

use BeginTimeEvalAndNativeCall;
pass 'Module loaded successfully';
done-testing;

# vim: expandtab shiftwidth=4
