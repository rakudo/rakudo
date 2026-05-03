use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 4;

# `$?FILE` resolves at parse time to the absolute path of the source file
# being compiled, matching legacy's current_file in src/Perl6/World.nqp.
# Non-path source names such as `-e` pass through as-is. The value is a
# string literal at each use site rather than a lexical declared per
# compilation unit: a per-compunit lexical leaked into `SETTING::` when
# the CORE setting itself was the compunit being compiled, and embedded
# the source path in the string heap of every compilation unit whether
# or not it mentioned `$?FILE`.

is-run 'say $?FILE', :out("-e\n"),
  '`$?FILE` of a -e one-liner is -e';

ok $?FILE.IO.is-absolute,
  '`$?FILE` in a source file is an absolute path';

is $?FILE.IO.basename, 'file-variable.t',
  '`$?FILE` names the file being compiled';

nok SETTING::{'$?FILE'}:exists,
  '`$?FILE` does not leak into SETTING:: from the CORE build';

# vim: expandtab shiftwidth=4
