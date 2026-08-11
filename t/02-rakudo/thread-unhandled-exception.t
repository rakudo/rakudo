use Test;

# An exception that escapes a raw Thread must be reported through the
# same renderer an unhandled mainline exception uses, including the
# handler named in RAKU_EXCEPTIONS_HANDLER and the low level variant
# under --ll-exception, and must exit the process with 1.

plan 10;

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

# vim: expandtab shiftwidth=4
