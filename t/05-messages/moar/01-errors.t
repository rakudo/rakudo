use lib <t/packages/>;
use Test;
use Test::Helpers;
$*VM.name eq 'moar' or plan :skip-all<These tests are only for MoarVM backend>;

plan 1;

# https://github.com/rakudo/rakudo/issues/1451
subtest 'no SEGV with failed opens of MVM debug files' => {
    plan +my @vars := <
        MVM_SPESH_LOG  MVM_DYNVAR_LOG  MVM_COVERAGE_LOG
    >;
    my $dir := (make-temp-dir).absolute;
    for @vars {
        (temp %*ENV){$_} = $dir;
        is-run ｢42.say｣, :out(*), :err{.contains: 'Failed to open file'},
            :exitcode(1), $_;
    }
}

# vim: expandtab shiftwidth=4
