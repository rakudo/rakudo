#!/usr/bin/env perl6

sub MAIN(
    #| old measurement
    \old-time,
    #| new measurement (typically less than the old measurement)
    \new-time,
    #| old overhead of the bench script (e.g. rakudo startup time)
    \old-overhead = 0,
    #| new overhead of the bench script (by default same as old-overhead)
    \new-overhead = old-overhead,
) {
    my \old = old-time - old-overhead;
    my \new = new-time - new-overhead;

    put (old ÷ new).round(0.01), ‘x as fast’;

    note ‘’;
    note ‘Given:’;
    note ‘old-time     = ’, old-time;
    note ‘new-time     = ’, new-time;
    note ‘old-overhead = ’, old-overhead;
    note ‘new-overhead = ’, new-overhead;
    note ‘’;
    note ‘old          = ’, old-time - old-overhead,
                         ‘ (old-time - old-overhead)’;
    note ‘new          = ’, new-time - new-overhead,
                         ‘ (new-time - new-overhead)’;

    note ‘’;
    note ‘All forms:’;
    note  old ÷ new,            ‘x as fast (old ÷ new)’;
    note  old ÷ new - 1,        ‘x faster (old ÷ new - 1)’;
    note (old ÷ new - 1) × 100, ‘% faster ( (old ÷ new - 1) × 100 )’;
    note ‘runs in ’,
          (new ÷ old × 100).round(0.01), ‘% of the time it used to (new ÷ old × 100)’;
}
