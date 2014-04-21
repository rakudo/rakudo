my enum Signal ( :SIGINT(1), :SIGBREAK(2), :SIGHUP(3), :SIGWINCH(4) );

sub signal(*@signals, :$scheduler = $*SCHEDULER) {
    my class SignalCancellation is repr('AsyncTask') { }
    state %sigmap =
        SIGINT,   nqp::const::SIG_INT,
        SIGBREAK, nqp::const::SIG_BREAK,
        SIGHUP,   nqp::const::SIG_HUP,
        SIGWINCH, nqp::const::SIG_WINCH;
    my @supplies = @signals.map(-> $sig {
        die "Invalid signal" unless $sig ~~ Signal;
        my $s = Supply.new;
        nqp::signal($scheduler.queue,
            -> $signum { $s.more($signum) },
            nqp::unbox_i(%sigmap{$sig}),
            SignalCancellation);
        $s
    });
    @supplies == 1 ?? @supplies[0] !! Supply.merge(@supplies)
}
