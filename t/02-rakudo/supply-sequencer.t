use Test;

# Drives Rakudo::Internals::SupplySequencer through the calling contract
# Proc::Async uses. The reordered and post-completion shapes cannot be
# produced by a real process.

plan 5;

sub driven(:$dones-expected = 1, **@tuples) {
    my @events;
    my $ss = Rakudo::Internals::SupplySequencer.new:
        :$dones-expected,
        on-data-ready => -> \data { @events.push(data.decode) },
        on-completed  => -> { @events.push('done') },
        on-error      => -> \err { @events.push("error " ~ err) };
    $ss.process(|$_) for @tuples;
    @events
}

is-deeply driven((0, 'a'.encode, Str), (1, 'b'.encode, Str), (2, Blob, Str)),
    ['a', 'b', 'done'],
    'A single stream delivers its output and completes on its EOF report';

is-deeply driven(:2dones-expected,
        (0, 'a'.encode, Str), (1, Blob, Str),
        (2, 'b'.encode, Str), (3, Blob, Str)),
    ['a', 'b', 'done'],
    'A merged stream delivers output arriving between its EOF reports';

is-deeply driven(:2dones-expected,
        (0, 'a'.encode, Str), (3, Blob, Str),
        (1, Blob, Str), (2, 'b'.encode, Str)),
    ['a', 'b', 'done'],
    'A merged stream completes exactly once on reordered EOF reports';

is-deeply driven(:2dones-expected,
        (0, 'a'.encode, Str), (1, Blob, Str), (2, Blob, Str),
        (Int, Str, 'late failure')),
    ['a', 'done'],
    'A failure report arriving after completion is not delivered';

is-deeply driven(:2dones-expected,
        (Int, Str, 'first failure'), (Int, Str, 'second failure')),
    ['error first failure'],
    'Only the first of two failure reports is delivered';

# vim: expandtab shiftwidth=4
