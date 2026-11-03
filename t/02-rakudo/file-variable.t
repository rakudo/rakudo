use lib <t/packages/Test-Helpers>;
use MONKEY-SEE-NO-EVAL;
use Test;
use Test::Helpers;
use nqp;

my $rakuast = nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 8;

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

# Code handed to EVAL with a file name, as EVALFILE does, has that name
# absolutized the same way. Only an EVAL's own pseudo-file such as EVAL_0,
# which names no on-disk source, passes through as-is. The cwd is joined
# on with a `/`, so the absolutized paths are compared cleaned up rather
# than as strings. `.cleanup` leaves a relative path relative, where
# `.absolute` would resolve one against the cwd and hide it.

todo 'the legacy frontend puts the cwd in front of the pseudo-file'
  unless $rakuast;
like EVAL(Q[$?FILE]), /^ 'EVAL_' \d+ $/,
  '`$?FILE` of an EVAL given no file name is its pseudo-file as-is';

is EVAL(Q[$?FILE], :filename<from-eval.raku>).IO.cleanup.Str,
  $*CWD.add('from-eval.raku').cleanup.Str,
  '`$?FILE` of an EVAL given a relative file name is that name under the cwd';

is EVAL(Q[$?FILE], :filename($*CWD.add('from-eval.raku').Str)),
  $*CWD.add('from-eval.raku').Str,
  '`$?FILE` of an EVAL given an absolute file name is that name';

my $relative = "file-variable-$*PID.raku";
$relative.IO.spurt('$?FILE');
LEAVE $relative.IO.unlink;
is EVALFILE($relative).IO.cleanup.Str, $*CWD.add($relative).cleanup.Str,
  '`$?FILE` of an EVALFILEd relative path is absolute';

# vim: expandtab shiftwidth=4
