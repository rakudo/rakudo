use Test;

plan 3;

# https://github.com/rakudo/rakudo/issues/2207

my $merged = Proc::Async.new('does-not-exist-cabbage-mooncake-unicycle');
my $merge-supply = $merged.Supply;
my $merged-promise = $merged.start;
throws-like { react { whenever $merge-supply { } } }, X::AdHoc,
    'The merged Supply of an async process that does not exist quits with the spawn error',
    message => /'does-not-exist-cabbage-mooncake-unicycle'/;
dies-ok { await $merged-promise },
    'Promise for an async process that does not exist is broken when its merged Supply is tapped';

if $*DISTRO.is-win {
    skip 'no pty support on Windows', 1;
}
else {
    my $pty = Proc::Async.new('does-not-exist-cabbage-mooncake-unicycle',
        :pty((:cols(80), :rows(24))));
    dies-ok { await $pty.start },
        'Promise for a pty process that does not exist is broken';
}

# The crashes guarded here kill a thread pool worker asynchronously. Give
# such a crash time to take the process down before the plan completes.
await Promise.in(0.5);

# vim: expandtab shiftwidth=4
