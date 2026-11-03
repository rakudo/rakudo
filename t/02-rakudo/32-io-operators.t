use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

use MONKEY-SEE-NO-EVAL;

plan 4;

# NOTE: This test file currently lives in t/02-rakudo in order to more directly track adjustments to the
#       revision-gated trait.

my $non-existent = $*TMPDIR.add("non-existent-" ~ ((^9).roll xx 2).join).Str;
my $non-existent-also =  $*TMPDIR.add("non-existent-" ~ ((^9).roll xx 3).join).Str;

subtest "chmod across the revisions", {
    is-run 'use v6.c;' ~ Q:c| print chmod(0o777, '{$non-existent}') ~~ [] |,
            :out("True"), q|[v6.c] chmod with nonexistent file produces empty array |;

    is-run 'use v6.d;' ~ Q:c| print chmod(0o777, '{$non-existent}') ~~ [] |,
            :out("True"), q|[v6.d] chmod with nonexistent file produces empty array |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print chmod(0o777, '{$non-existent}', '{$non-existent-also}') ~~ [] |,
            :out("True"), :err(/ .* 'Please use @paths.grep(*.IO.chmod) instead.' .* /),
            q|[v6.e] chmod with multiple nonexistent files and old signature produces empty array and expected deprecation message |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print chmod(0o777, '{$non-existent}') ~~ [] |,
            :exitcode(0), :out("True"), :err(/.* 'Please use chmod(Str $path, Int() :$mode) instead.' .*/),
            q|[v6.e] chmod with single nonexistent file (Str) and old signature produces empty array and expected deprecation message '|;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print chmod(0o777, '{$non-existent}'.IO) ~~ [] |,
            :exitcode(0), :out("True"), :err(/.* 'Please use chmod(IO $path, Int() :$mode) instead.' .*/),
            q|[v6.e] chmod with single nonexistent file (IO) and old signature produces empty array and expected deprecation message '|;

    is-run "use v6.e.PREVIEW;" ~ Q:c| print (my $res = chmod('{$non-existent}', :mode(0o777))) ~~ Failure; print $res |,
            :exitcode(1), :out("True"), :err(/ "Failed to set the mode of" .* "to '0o777'" .* /),
            q|chmod with nonexistent file (Str) produces Failure |;

    is-run "use v6.e.PREVIEW;" ~ Q:c| print (my $res = chmod('{$non-existent}', :mode(0o777))) ~~ Failure; print $res |,
            :exitcode(1), :out("True"), :err(/ "Failed to set the mode of" .* "to '0o777'" .* /),
            q|chmod with nonexistent file (IO) produces Failure |;

};

subtest "chown across the revisions", {
    is-run 'use v6.c;' ~ Q:c| print chown('{$non-existent}', :uid(111), :gid(222)) ~~ [] |,
            :out("True"), q|[v6.c] chown with nonexistent file produces empty array|;

    is-run 'use v6.d;' ~ Q:c| print chown('{$non-existent}', :uid(111), :gid(222)) ~~ [] |,
            :out("True"), q|[v6.d] chown with nonexistent file produces empty array|;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print chown('{$non-existent}', '{$non-existent-also}', :uid(111), :gid(222)) ~~ [] |,
            :out("True"), :err(/ .* '@paths.grep(*.IO.chown(:uid(111), :gid(222)))' .* /),
            q|[v6.e] chown with multiple nonexistent files (UID + GID) produces empty array and deprecation |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print chown('{$non-existent}', '{$non-existent-also}', :uid(111)) ~~ [] |,
            :out("True"), :err(/ .* '@paths.grep(*.IO.chown(:uid(111)))' .* /),
            q|[v6.e] chown with multiple nonexistent files (UID) produces empty array and deprecation |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print chown('{$non-existent}', '{$non-existent-also}', :gid(222)) ~~ [] |,
            :out("True"), :err(/ .* '@paths.grep(*.IO.chown(:gid(222)))' .* /),
            q|[v6.e] chown with multiple nonexistent files (GID) produces empty array and deprecation |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print (my $res = chown('{$non-existent}', :uid(111), :gid(222))) ~~ Failure; print $res |,
            :exitcode(1), :out("True"), :err(/ .* "Failed to change owner of" .* "to 111/222" .* /),
            q|[v6.e] chown with nonexistent file (Str) produces Failure |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print (my $res = chown('{$non-existent}'.IO, :uid(111), :gid(222))) ~~ Failure; print $res |,
            :exitcode(1), :out("True"), :err(/ .* "Failed to change owner of" .*  "to 111/222" .* /),
            q|[v6.e] chown with nonexistent file (IO) produces Failure |;
};


subtest "unlink across the revisions", {
    is-run 'use v6.c;' ~ Q:c| print unlink('{$non-existent}') ~~ ['{$non-existent}'] |,
            :out("True"), q|[v6.c] unlink with nonexistent file produces array containing file name |;

    is-run 'use v6.d;' ~ Q:c| print unlink('{$non-existent}') ~~ ['{$non-existent}'] |,
            :out("True"), q|[v6.d] unlink with nonexistent file produces array containing file name |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print unlink('{$non-existent}', '{$non-existent-also}') ~~ ['{$non-existent}', '{$non-existent-also}'] |,
            :out("True"), :err(/ .* '@paths.grep(*.IO.unlink)' .* /),
            q|[v6.e] unlink with multiple nonexistent files produces empty array and deprecation |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print unlink('{$non-existent}') |,
            :out("True"),
            q|[v6.e] unlink with nonexistent file (Str) produces True |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print unlink('{$non-existent}'.IO) |,
            :out("True"),
            q|[v6.e] chown with nonexistent file (IO) produces True |;
};

subtest "rmdir across the revisions", {
    is-run 'use v6.c;' ~ Q:c| print rmdir('{$non-existent}') ~~ [] |,
            :out("True"), q|[v6.c] rmdir with nonexistent directory produces empty array |;

    is-run 'use v6.d;' ~ Q:c| print rmdir('{$non-existent}') ~~ [] |,
            :out("True"), q|[v6.d] unlink with nonexistent directory produces empty array |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print rmdir('{$non-existent}', '{$non-existent-also}') ~~ [] |,
            :out("True"), :err(/ .* '@paths.grep(*.IO.rmdir)' .* /),
            q|[v6.e] rmdir with multiple nonexistent directory produces empty array and deprecation |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print (my $res = rmdir('{$non-existent}')) ~~ Failure; print $res |,
            :exitcode(1), :out("True"), :err(/ .* "Failed to remove the directory" .* /),
            q|[v6.e] rmdir with nonexistent file (Str) produces True |;

    is-run 'use v6.e.PREVIEW;' ~ Q:c| print (my $res = rmdir('{$non-existent}'.IO)) ~~ Failure; print $res |,
            :exitcode(1), :out("True"), :err(/ .* "Failed to remove the directory" .* /),
            q|[v6.e] rmdir with nonexistent file (IO) produces True |;
};