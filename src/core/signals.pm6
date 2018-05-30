my enum Signal ( :SIGHUP(1), :SIGINT(2), :SIGQUIT(3), :SIGILL(4), :SIGTRAP(5),
    :SIGABRT(6), :SIGEMT(7), :SIGFPE(8), :SIGKILL(9), :SIGBUS(10), :SIGSEGV(11),
    :SIGSYS(12), :SIGPIPE(13), :SIGALRM(14), :SIGTERM(15), :SIGURG(16),
    :SIGSTOP(17), :SIGTSTP(18), :SIGCONT(19), :SIGCHLD(20), :SIGTTIN(21),
    :SIGTTOU(22), :SIGIO(23), :SIGXCPU(24), :SIGXFSZ(25), :SIGVTALRM(26),
    :SIGPROF(27), :SIGWINCH(28), :SIGINFO(29), :SIGUSR1(30), :SIGUSR2(31),
    :SIGTHR(32), :SIGSTKFLT(116), :SIGPWR(130), :SIGBREAK(221) );

proto sub signal($, |) {*}
multi sub signal(Signal $signal, *@signals, :$scheduler = $*SCHEDULER) {

    if @signals.grep( { !nqp::istype($_,Signal) } ).list -> @invalid {
        die "Found invalid signals: {@invalid}";
    }
    @signals.unshift: $signal;
    @signals .= unique;

    my constant %sigmap = (
#?if moar
        SIGHUP,   nqp::const::SIG_HUP // -1,
#?endif
        SIGINT,   nqp::const::SIG_INT // -1,
#?if moar
        SIGQUIT,  nqp::const::SIG_QUIT // -1,
        SIGILL,   nqp::const::SIG_ILL // -1,
        SIGTRAP,  nqp::const::SIG_TRAP // -1,
        SIGABRT,  nqp::const::SIG_ABRT // -1,
        SIGEMT,   nqp::const::SIG_EMT // -1,
        SIGFPE,   nqp::const::SIG_FPE // -1,
#?endif
        SIGKILL,  nqp::const::SIG_KILL // -1,
#?if moar
        SIGBUS,   nqp::const::SIG_BUS // -1,
        SIGSEGV,  nqp::const::SIG_SEGV // -1,
        SIGSYS,   nqp::const::SIG_SYS // -1,
        SIGPIPE,  nqp::const::SIG_PIPE // -1,
        SIGALRM,  nqp::const::SIG_ALRM // -1,
        SIGTERM,  nqp::const::SIG_TERM // -1,
        SIGURG,   nqp::const::SIG_URG // -1,
        SIGSTOP,  nqp::const::SIG_STOP // -1, # hammer time
        SIGTSTP,  nqp::const::SIG_TSTP // -1,
        SIGCONT,  nqp::const::SIG_CONT // -1,
        SIGCHLD,  nqp::const::SIG_CHLD // -1,
        SIGTTIN,  nqp::const::SIG_TTIN // -1,
        SIGTTOU,  nqp::const::SIG_TTOU // -1,
        SIGIO,    nqp::const::SIG_IO // -1,
        SIGXCPU,  nqp::const::SIG_XCPU // -1,
        SIGXFSZ,  nqp::const::SIG_XFSZ // -1,
        SIGVTALRM,nqp::const::SIG_VTALRM // -1,
        SIGPROF,  nqp::const::SIG_PROF // -1,
        SIGWINCH, nqp::const::SIG_WINCH // -1,
        SIGINFO,  nqp::const::SIG_INFO // -1,
        SIGUSR1,  nqp::const::SIG_USR1 // -1,
        SIGUSR2,  nqp::const::SIG_USR2 // -1,
        SIGTHR,   nqp::const::SIG_THR // -1,
        SIGSTKFLT,nqp::const::SIG_STKFLT // -1,
        SIGPWR,   nqp::const::SIG_PWR // -1,
        SIGBREAK, nqp::const::SIG_BREAK // -1
#?endif
    ).hash;

    my @known_signals := $*KERNEL.signals;

    my class SignalCancellation is repr('AsyncTask') { }
    Supply.merge( @signals.map(-> $signal {
        class SignalTappable does Tappable {
            has $!scheduler;
            has @!known_signals;
            has %!sigmap;
            has $!signal;

            submethod BUILD(:$!scheduler, :@!known_signals, :%!sigmap, :$!signal) { }

            method tap(&emit, &, &, &tap) {
                my $cancellation := nqp::signal($!scheduler.queue(:hint-time-sensitive),
                    -> $signum { emit(@!known_signals[$signum] // $signum) },
                    nqp::unbox_i(%!sigmap{$!signal}),
                    SignalCancellation);
                my $t = Tap.new({ nqp::cancel($cancellation) });
                tap($t);
                $t;
            }

            method live(--> False) { }
            method sane(--> True) { }
            method serial(--> False) { }
        }
        Supply.new(SignalTappable.new(:$scheduler, :@known_signals, :%sigmap, :$signal));
    }) );
}

# vim: ft=perl6 expandtab sw=4
