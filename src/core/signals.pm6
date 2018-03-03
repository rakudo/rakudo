my enum Signal ( |nqp::getsignals() );

proto sub signal(|) {*}
multi sub signal(Signal $signal, *@signals, :$scheduler = $*SCHEDULER) {

    if @signals.grep( { !nqp::istype($_,Signal) } ).list -> @invalid {
        die "Found invalid signals: {@invalid}";
    }
    @signals.unshift: $signal;
    @signals .= unique;

    my class SignalCancellation is repr('AsyncTask') { }
    Supply.merge( @signals.map(-> $signal {
        class SignalTappable does Tappable {
            has $!scheduler;
            has $!signal;

            submethod BUILD(:$!scheduler, :$!signal) { }

            method tap(&emit, &, &, &tap) {
                my $cancellation := nqp::signal($!scheduler.queue(:hint-time-sensitive),
                    -> $signum { emit($signum) },
                    nqp::unbox_i($!signal),
                    SignalCancellation);
                my $t = Tap.new({ nqp::cancel($cancellation) });
                tap($t);
                $t;
            }

            method live(--> False) { }
            method sane(--> True) { }
            method serial(--> False) { }
        }
        Supply.new(SignalTappable.new(:$scheduler, :$signal));
    }) );
}

# vim: ft=perl6 expandtab sw=4
