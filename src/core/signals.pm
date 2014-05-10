my enum Signal ( :SIGINT(1), :SIGBREAK(2), :SIGHUP(3), :SIGWINCH(4) );

sub signal(Signal $signal, *@signals, :$scheduler = $*SCHEDULER) {

    if @signals.grep( { $_ !~~ Signal } ) -> @invalid {
        die "Found invalid signals: {@invalid}";
    }
    @signals.unshift: $signal;
    @signals .= uniq;

    state %sigmap =
        SIGINT,   nqp::const::SIG_INT,
        SIGBREAK, nqp::const::SIG_BREAK,
        SIGHUP,   nqp::const::SIG_HUP,
        SIGWINCH, nqp::const::SIG_WINCH;

    state @known_signals;
    once {
        my @names = "",qx/kill -l/.words;
        @names.splice(1,1) if @names[1] eq "0";  # Ubuntu fudge

        for Signal.^enum_value_list -> $signal {
            my $name = $signal.key.substr(3);
            if @names.first-index( * eq $name ) -> $index {
                @known_signals[$index] = $signal;
            }
        }
    }

    my class SignalCancellation is repr('AsyncTask') { }
    Supply.merge( @signals.map(-> $sig {
        my $s = Supply.new;
        nqp::signal($scheduler.queue,
            -> $signum { $s.more(@known_signals[$signum] // $signum) },
            nqp::unbox_i(%sigmap{$sig}),
            SignalCancellation);
        $s
    }) );
}
