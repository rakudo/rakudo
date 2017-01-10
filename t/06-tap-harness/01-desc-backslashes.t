use v6;
use lib <lib>;
use Test;
use TAP:auth<perl>;

plan 8;

constant $TEST_FILE
= 't/06-tap-harness/test-files/01-desc-backslashes--1.TAP-TEST';

# stupid hack to avoid TAP::Harness's output from interfering with our tests
# I can't find docs for it to know how to do this properly; so run it as
# as a separate script, make it do tests, then test that output with like()
given run :out, :err, $*EXECUTABLE, '-e', ｢
    use lib <lib>;
    use Test;
    use TAP:auth<perl>;

    plan 8;

    my $h = TAP::Harness.new;
    my $w = $h.run('｣ ~ $TEST_FILE ~ ｢').waiter;
    await $w;
    given $w.result {
        is .tests-planned,  12,    'HTEST1 planned';
        is .passed,         12,    'HTEST2 passed';
        is .failed,         0,     'HTEST3 failed';
        is .errors,         0,     'HTEST4 errors';
        is .skipped,        1,     'HTEST5 skipped';
        is .todo,           0,     'HTEST6 todo';
        is .todo-passed,    0,     'HTEST7 todo passed';
        is .exit-failed,    False, 'HTEST8 exit failed';
    }
｣ {
    given .err.slurp-rest { when :so { diag "STDERR output was ```$_```" } }

    my $output = .out.slurp-rest;
    like $output, /^^ 'ok' \N+ 'HTEST1'/, 'planned';
    like $output, /^^ 'ok' \N+ 'HTEST2'/, 'passed';
    like $output, /^^ 'ok' \N+ 'HTEST3'/, 'failed';
    like $output, /^^ 'ok' \N+ 'HTEST4'/, 'errors';
    like $output, /^^ 'ok' \N+ 'HTEST5'/, 'skipped';
    like $output, /^^ 'ok' \N+ 'HTEST6'/, 'todo';
    like $output, /^^ 'ok' \N+ 'HTEST7'/, 'todo passed';
    like $output, /^^ 'ok' \N+ 'HTEST8'/, '.exit-failed';
}
