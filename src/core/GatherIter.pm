class GatherIter is Iterator {
    has Mu $!coro;             # coroutine to execute for more pairs
    has $!reified;             # Parcel of this iterator's results
    has $!infinite;            # true if iterator is known infinite

#?if jvm
    my $GATHER_PROMPT = [];
    my $SENTINEL := [];
#?endif
#?if moar
    my $SENTINEL := [];
#?endif
    method new($block, Mu :$infinite) {
#?if parrot
        my Mu $coro := 
            nqp::clone(nqp::getattr(&coro, Code, '$!do'));
        nqp::ifnull($coro($block), Nil);
#?endif
#?if jvm
        my Mu $takings;
        my Mu $state;
        my sub yield() {
            nqp::continuationcontrol(0, $GATHER_PROMPT, -> Mu \c {
                $state := sub () is rw { nqp::continuationinvoke(c, -> | { Nil }); };
            });
        }
        $state := sub () is rw {
            nqp::handle( $block().eager(),
                'TAKE', ($takings := nqp::getpayload(nqp::exception()); yield(); nqp::resume(nqp::exception())));
            $takings := $SENTINEL; yield();
        };
        my $coro := sub () is rw { nqp::continuationreset($GATHER_PROMPT, $state); $takings };
#?endif
#?if moar
        # XXX Cheating, eager implementation for now. Limits to 1000
        # takes.
        my int $done = 0;
        my $coro := -> {
            if $done {
                $SENTINEL
            }
            else {
                my Mu $takings := nqp::list();
                my int $taken = 0;
                nqp::handle($block().eager(),
                    'TAKE', nqp::stmts(
                        nqp::push($takings, nqp::getpayload(nqp::exception())),
                        ($taken = $taken + 1) <= 1000 && nqp::resume(nqp::exception())));
                $done = 1;
                nqp::p6parcel($takings, Mu)
            }
        }
#?endif
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

    method reify($n = 1) { 
        if !$!reified.defined {
            my Mu $rpa := nqp::list();
            my Mu $parcel;
            my $end = Bool::False;
            my $count = nqp::istype($n, Whatever) ?? 1 !! $n;
            while !$end && $count > 0 {
                $parcel := $!coro();
#?if parrot
                $end = nqp::p6bool(nqp::isnull($parcel));
#?endif
#?if jvm
                $end = nqp::p6bool(nqp::eqaddr($parcel, $SENTINEL));
#?endif
#?if moar
                $end = nqp::p6bool(nqp::eqaddr($parcel, $SENTINEL));
#?endif
                nqp::push($rpa, $parcel) unless $end;
                $count = $count - 1;
            }
#?if moar
            # Needed until we have proper thing-at-a-time gather/take.
            if nqp::elems($rpa) {
                $rpa := nqp::getattr(nqp::atpos($rpa, 0), Parcel, '$!storage');
            }
#?endif
            nqp::push($rpa, 
                nqp::p6bindattrinvres(
                    nqp::p6bindattrinvres(
                        nqp::create(self), GatherIter, '$!coro', $!coro),
                    GatherIter, '$!infinite', $!infinite))
                unless $end;
            $!reified := nqp::p6parcel($rpa, nqp::null());
        }
        $!reified
    }

    method infinite() { $!infinite }

#?if parrot
    my sub coro(\block) {
        Q:PIR {
            .local pmc block, handler, taken
            block = find_lex 'block'
            .yield ()
            handler = root_new ['parrot';'ExceptionHandler']
            handler.'handle_types'(.CONTROL_TAKE)
            set_addr handler, take_handler
            push_eh handler
            $P0 = block()
            $P0.'eager'()
            pop_eh
          gather_done:
            null taken
            .yield (taken)
            goto gather_done
          take_handler:
            .local pmc exception, resume
            .get_results (exception)
            taken  = exception['payload']
            resume = exception['resume']
            .yield (taken)
            resume()
            goto gather_done    # should never get here
        };
        True
    }
#?endif
}


sub GATHER(\block, Mu :$infinite) { 
    GatherIter.new( block, :$infinite ).list;  
}

