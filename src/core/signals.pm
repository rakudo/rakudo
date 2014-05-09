my enum Signal ( :SIGINT(1), :SIGBREAK(2), :SIGHUP(3), :SIGWINCH(4) );

sub signal(Signal $signal, *@signals, :$scheduler = $*SCHEDULER) {

    if @signals.grep( { $_ !~~ Signal } ) -> @invalid {
        die "Found invalid signals: {@invalid}";
    }
    @signals.unshift: $signal;

    state %sigmap =
        SIGINT,   nqp::const::SIG_INT,
        SIGBREAK, nqp::const::SIG_BREAK,
        SIGHUP,   nqp::const::SIG_HUP,
        SIGWINCH, nqp::const::SIG_WINCH;

    my class SignalCancellation is repr('AsyncTask') { }
    Supply.merge( @signals.map(-> $sig {
        my $s = Supply.new;
        nqp::signal($scheduler.queue,
            -> $signum { $s.more($signum) },
            nqp::unbox_i(%sigmap{$sig}),
            SignalCancellation);
        $s
    }) );
}
