use Test;

# An exception that escapes a raw Thread or a thread pool worker must
# be reported through the same renderer an unhandled mainline exception
# uses, including the handler named in RAKU_EXCEPTIONS_HANDLER and the
# low level variant under --ll-exception, and must exit the process
# with 1. The pool report keeps its usual header when no handler takes
# over.

plan 33;

sub run-child($code, $handler?, *@flags) {
    my %env = %*ENV;
    with $handler {
        %env<RAKU_EXCEPTIONS_HANDLER> = $_;
    }
    else {
        %env<RAKU_EXCEPTIONS_HANDLER>:delete;
    }
    my $proc = Proc::Async.new($*EXECUTABLE, |@flags, '-e', $code);
    my $err = '';
    $proc.stderr.tap: { $err ~= $_ };
    my $result = await $proc.start(:ENV(%env));
    ($err, $result)
}

my $thread-code = 'Thread.start({ die "from thread" }); sleep 5';

my ($err, $result) = run-child($thread-code);
ok $err.contains('from thread'),
    'The exception message of a Thread that died reaches stderr';
nok $err.contains('No exception handler located'),
    'A Thread that died does not produce the low level VM report';
is $result.exitcode, 1,
    'A process whose Thread died from an exception exits with 1';

($err, $result) = run-child($thread-code, 'JSON');
ok $err.contains('"X::AdHoc"') && $err.contains('from thread'),
    'RAKU_EXCEPTIONS_HANDLER renders the exception of a Thread that died';
nok $err.contains('  in block'),
    'The handler output of a Thread that died has no default report';
is $result.exitcode, 1,
    'A process whose Thread died with the handler set exits with 1';

($err, $result) = run-child($thread-code, Nil, '--ll-exception');
ok $err.contains('from thread') && $err.contains('SETTING::src/core.c/'),
    'A Thread that died keeps the full backtrace under --ll-exception';
is $result.exitcode, 1,
    'A process whose Thread died under --ll-exception exits with 1';

($err, $result) = run-child('END note "END RAN"; ' ~ $thread-code);
ok $err.contains('END RAN'),
    'END phasers run for a process whose Thread died';
is $result.exitcode, 1,
    'A process whose Thread died runs END phasers and exits with 1';

my $pool-code = 'start { die "from pool" }; sleep 5';
my $header    = 'Unhandled exception in code scheduled on thread';

($err, $result) = run-child($pool-code);
ok $err.contains($header) && $err.contains('from pool'),
    'A pool worker that died keeps its usual report without the handler set';
is $result.exitcode, 1,
    'A process whose pool worker died exits with 1';

($err, $result) = run-child($pool-code, 'JSON');
ok $err.contains('"X::AdHoc"') && $err.contains('from pool'),
    'RAKU_EXCEPTIONS_HANDLER renders the exception of a pool worker that died';
nok $err.contains($header),
    'The handler output of a pool worker that died has no default report';
is $result.exitcode, 1,
    'A process whose pool worker died with the handler set exits with 1';

($err, $result) = run-child($pool-code, 'DoesNotExistCabbage');
ok $err.contains($header) && $err.contains('from pool'),
    'A handler that cannot be loaded falls back to the usual pool report';
is $result.exitcode, 1,
    'A process whose pool worker died with an unloadable handler exits with 1';

($err, $result) = run-child(
    'class Exceptions::Decline { method process($e) { $*ERR.say("DECLINE-SAW"); True } }; '
        ~ $pool-code,
    'Decline');
ok $err.contains('DECLINE-SAW') && $err.contains($header) && $err.contains('from pool'),
    'A handler that declines is followed by the usual pool report';
is $result.exitcode, 1,
    'A process whose pool worker died with a declining handler exits with 1';

($err, $result) = run-child(
    'class Exceptions::Boom { method process($e) { die "handler broke" } }; '
        ~ $pool-code,
    'Boom');
ok $err.contains($header) && $err.contains('from pool'),
    'A handler that died is followed by the usual pool report';
ok $err.contains('Exceptions::Boom died'),
    'A handler that died is reported itself';
ok $err.contains('handler broke'),
    'The exception a handler died with is reported';
is $result.exitcode, 1,
    'A process whose pool worker died with a dying handler exits with 1';

($err, $result) = run-child($pool-code, Nil, '--ll-exception');
ok $err.contains($header) && $err.contains('from pool')
    && $err.contains('SETTING::src/core.c/'),
    'The pool report keeps the full backtrace under --ll-exception';
is $result.exitcode, 1,
    'A process whose pool worker died under --ll-exception exits with 1';

($err, $result) = run-child(
    'my $p = Promise.new(:report-broken-if-sunk); $p.break("boom"); $p.sink; sleep 5');
ok $err.contains($header) && $err.contains('boom'),
    'A sunk broken Promise with an exception that was never thrown is reported';
is $result.exitcode, 1,
    'A process with a sunk broken Promise exits with 1';

($err, $result) = run-child('END note "END RAN"; ' ~ $pool-code);
ok $err.contains('END RAN'),
    'END phasers run for a process whose pool worker died';
is $result.exitcode, 1,
    'A process whose pool worker died runs END phasers and exits with 1';

($err, $result) = run-child(
    'my $p = Promise.new(:report-broken-if-sunk); $p.break(X::AdHoc); $p.sink; sleep 5');
ok $err.contains($header) && $err.contains('(AdHoc)'),
    'A sunk Promise broken with an exception type object is reported';
is $result.exitcode, 1,
    'A process with a sunk Promise broken with a type object exits with 1';

($err, $result) = run-child(
    'start { $*SCHEDULER.handle_uncaught("plain string uncaught") }; sleep 5');
ok $err.contains($header) && $err.contains('plain string uncaught'),
    'A value that is not an exception passed to handle_uncaught is reported';
is $result.exitcode, 1,
    'A process that passed a plain value to handle_uncaught exits with 1';

# vim: expandtab shiftwidth=4
