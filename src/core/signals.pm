my enum Signal ( :SIGINT(1), :SIGBREAK(2), :SIGHUP(3), :SIGWINCH(4) );

sub signal(Signal $signal, *@signals, :$scheduler = $*SCHEDULER) {

    if @signals.grep( { $_ !~~ Signal } ) -> @invalid {
        die "Found invalid signals: {@invalid}";
    }
    @signals.unshift: $signal;
    @signals .= uniq;

    state %sigmap =
        SIGHUP,   nqp::const::SIG_HUP,
        SIGINT,   nqp::const::SIG_INT,
        SIGQUIT,  nqp::const::SIG_QUIT,
        SIGILL,   nqp::const::SIG_ILL,
        SIGTRAP,  nqp::const::SIG_TRAP,
        SIGABRT,  nqp::const::SIG_ABRT,
        SIGEMT,   nqp::const::SIG_EMT,
        SIGFPE,   nqp::const::SIG_FPE,
        SIGKILL,  nqp::const::SIG_KILL,
        SIGBUS,   nqp::const::SIG_BUS,
        SIGSEGV,  nqp::const::SIG_SEGV,
        SIGSYS,   nqp::const::SIG_SYS,
        SIGPIPE,  nqp::const::SIG_PIPE,
        SIGALRM,  nqp::const::SIG_ALRM,
        SIGTERM,  nqp::const::SIG_TERM,
        SIGURG,   nqp::const::SIG_URG,
        SIGSTOP,  nqp::const::SIG_STOP, # hammer time
        SIGTSTP,  nqp::const::SIG_TSTP,
        SIGCONT,  nqp::const::SIG_CONT,
        SIGCHLD,  nqp::const::SIG_CHLD,
        SIGTTIN,  nqp::const::SIG_TTIN,
        SIGTTOU,  nqp::const::SIG_TTOU,
        SIGIO,    nqp::const::SIG_IO,
        SIGXCPU,  nqp::const::SIG_XCPU,
        SIGXFSZ,  nqp::const::SIG_XFSZ,
        SIGVTALRM,nqp::const::SIG_VTALRM,
        SIGPROF,  nqp::const::SIG_PROF,
        SIGWINCH, nqp::const::SIG_WINCH,
        SIGINFO,  nqp::const::SIG_INFO,
        SIGUSR1,  nqp::const::SIG_USR1,
        SIGUSR2,  nqp::const::SIG_USR2,
        SIGTHR,   nqp::const::SIG_THR,
        SIGSTKFLT,nqp::const::SIG_STKFLT,
        SIGPWR,   nqp::const::SIG_PWR,
        SIGBREAK, nqp::const::SIG_BREAK;

    state @known_signals := $*KERNEL.signals;

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

# vim: ft=perl6 expandtab sw=4
