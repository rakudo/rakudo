use Test;

plan 10;

# A reader that shifts the close or fail marker off a channel's queue pushes
# it straight back, which leaves the queue empty for a moment. A reader that
# takes the last value during that moment sees nothing behind it and must
# still settle the closed promise, or an await on the channel never returns.

# Each channel holds one value and is already closed or failed when several
# threads race to drain it, so the last value and the marker are shifted
# concurrently often enough to hit that moment.
sub drain-raced(&end, &take) {
    my @channels = (^10000).map: {
        my $c = Channel.new;
        $c.send(1);
        end($c);
        $c
    }
    my @threads = (^4).map: {
        Thread.start({ for @channels { take($_); take($_) } })
    }
    .finish for @threads;
    @channels
}

sub unsettled(@channels) {
    +@channels.grep({ .closed.status == Planned })
}

sub not-failed(@channels) {
    +@channels.grep({ .closed.status != Broken || .closed.cause.message ne 'nope' })
}

{
    my @channels = drain-raced({ .close }, { .poll });
    is unsettled(@channels), 0,
      'closed promise is kept for every closed channel drained by poll';

    my $awaited = start { for @channels { try await $_ } }
    await Promise.anyof($awaited, Promise.in(30));
    is $awaited.status, Kept,
      'await on every drained closed channel returns';
}

{
    my @channels = drain-raced({ .close }, { try .receive });
    is unsettled(@channels), 0,
      'closed promise is kept for every closed channel drained by receive';
}

{
    my @channels = drain-raced({ .fail('nope') }, { .poll });
    is not-failed(@channels), 0,
      'closed promise is broken with the failure for every failed channel drained by poll';
}

{
    my @channels = drain-raced({ .fail('nope') }, { try .receive });
    is not-failed(@channels), 0,
      'closed promise is broken with the failure for every failed channel drained by receive';
}

{
    my $c = Channel.new;
    is $c.poll, Nil, 'poll on an open empty channel returns Nil';
    is $c.closed.status, Planned,
      'poll on an open empty channel leaves the closed promise planned';
}

# The first marker to reach the queue decides how the channel ends.
{
    my $c = Channel.new;
    $c.send(1);
    $c.fail('first');
    $c.close;
    $c.poll;
    is $c.closed.status, Broken,
      'closed promise is broken when a fail comes before a close';
    is $c.closed.cause.message, 'first',
      'closed promise carries the failure when a fail comes before a close';
}

{
    my $c = Channel.new;
    $c.send(1);
    $c.close;
    $c.fail('late');
    $c.poll;
    is $c.closed.status, Kept,
      'closed promise is kept when a fail comes after a close';
}

# vim: expandtab shiftwidth=4
