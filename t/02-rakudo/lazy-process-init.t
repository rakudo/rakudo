use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 9;

# %*ENV, $*CWD and $*SCHEDULER initialize on first access rather than at
# startup. These run in a fresh process because the test harness itself
# touches all three.

is-run 'print PROCESS::<%ENV>:exists',
    :out<False>,
    'a fresh process has not built %*ENV';

is-run 'print PROCESS::<$SCHEDULER>:exists',
    :out<False>,
    'a fresh process has not built $*SCHEDULER';

is-run 'print PROCESS::<$CWD>:exists',
    :out<False>,
    'a fresh process has not built $*CWD';

is-run q:to/CODE/,
    my $before = (PROCESS::<%ENV>:exists) || (PROCESS::<$SCHEDULER>:exists) || (PROCESS::<$CWD>:exists);
    my $env-works = %*ENV.elems > 0;
    my $cwd-works = $*CWD.d;
    my $sched-works = await(start { 42 }) == 42;
    my $after = (PROCESS::<%ENV>:exists) && (PROCESS::<$SCHEDULER>:exists) && (PROCESS::<$CWD>:exists);
    print (!$before, $env-works, $cwd-works, $sched-works, $after).join(",");
    CODE
    :out<True,True,True,True,True>,
    'first access builds each of them and they work';

is-run 'Rakudo::Deprecations.DEPRECATED("meow","","")',
    :err(/meow/),
    'a deprecation report prints when the END phaser is the first %*ENV access';

is-run '%*ENV<RAKUDO_NO_DEPRECATIONS> = 1; Rakudo::Deprecations.DEPRECATED("meow","","")',
    :err(''),
    'RAKUDO_NO_DEPRECATIONS suppresses the deprecation report';

# A spawned process runs on the process scheduler even under a lexical
# $*SCHEDULER, and the spawn works when nothing built that scheduler yet.
is-run q:to/CODE/,
    my $*SCHEDULER = CurrentThreadScheduler.new;
    my $proc = run $*EXECUTABLE, '-e', 'print 7', :out;
    print $proc.out.slurp(:close);
    CODE
    :out<7>,
    'run prefers the process scheduler over a lexical one';

# The scheduler debug flags read the raw environment, and an explicit 0
# must keep meaning off like it does through a val()'d %*ENV value.
{
    temp %*ENV<RAKUDO_SCHEDULER_DEBUG> = '0';
    my $proc = run $*EXECUTABLE, '-e', 'await start { 1 }', :err;
    is $proc.err.slurp(:close), '',
        'RAKUDO_SCHEDULER_DEBUG set to 0 keeps scheduler debug off';
}
{
    temp %*ENV<RAKUDO_SCHEDULER_DEBUG> = '1';
    my $proc = run $*EXECUTABLE, '-e', 'await start { 1 }', :err;
    ok $proc.err.slurp(:close).contains('[SCHEDULER'),
        'RAKUDO_SCHEDULER_DEBUG set to 1 turns scheduler debug on';
}

# vim: expandtab shiftwidth=4
