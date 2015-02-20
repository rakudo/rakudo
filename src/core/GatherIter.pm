class GatherIter is Iterator {
    has Mu $!coro;             # coroutine to execute for more pairs
    has $!reified;             # Parcel of this iterator's results
    has $!infinite;            # true if iterator is known infinite

    my $GATHER_PROMPT = Mu.new;
    my $SENTINEL := Mu.new;
    method new($block, Mu :$infinite) {
        my Mu $takings;
        my Mu $state;
        my &yield := {
            nqp::continuationcontrol(0, $GATHER_PROMPT, -> Mu \c { $state := c; });
        }
        $state := {
            nqp::handle( $block(),
                'TAKE', SEQ($takings := nqp::getpayload(nqp::exception()); yield(); nqp::resume(nqp::exception())));
            $takings := $SENTINEL; yield();
        };
        my $coro := { nqp::continuationreset($GATHER_PROMPT, $state); $takings };
        my Mu $new := nqp::create(self);
        nqp::bindattr($new, GatherIter, '$!coro', $coro);
        nqp::bindattr($new, GatherIter, '$!infinite', $infinite);
        $new;
    }

    multi method DUMP(GatherIter:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my $flags    := ("\x221e" if self.infinite);
        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!reified' );
        nqp::push($attrs,  $!reified  );
        nqp::push($attrs, '$!coro'    );
        nqp::push($attrs,  $!coro     );
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx, :$flags);
    }

    method reify($n) {
        if !$!reified.defined {
            my Mu $rpa := nqp::list();
            my Mu $parcel;
            my int $end;
            my int $count =
              nqp::unbox_i(nqp::istype($n,Whatever) ?? 1000 !! $n);
            while nqp::not_i($end) && nqp::isgt_i($count,0) {
                $parcel := $!coro();
                $end = nqp::eqaddr($parcel, $SENTINEL);
                nqp::push($rpa, $parcel) if nqp::not_i($end);
                $count = $count - 1;
            }
            nqp::push($rpa,
                nqp::p6bindattrinvres(
                    nqp::p6bindattrinvres(
                        nqp::create(self), GatherIter, '$!coro', $!coro),
                    GatherIter, '$!infinite', $!infinite))
                if nqp::not_i($end);
            $!reified := nqp::p6parcel($rpa, nqp::null());
        }
        $!reified
    }

    multi method infinite(GatherIter:D:) { $!infinite }

}


sub GATHER(\block, Mu :$infinite) {
    GatherIter.new( block, :$infinite ).list;
}

# vim: ft=perl6 expandtab sw=4
