class GatherIter is Iterator {
    has Mu $!coro;             # coroutine to execute for more pairs
    has $!reified;             # Parcel of this iterator's results
    has $!infinite;            # true if iterator is known infinite

    method new($block, Mu :$infinite) {
        my Mu $coro := 
            nqp::clone(nqp::getattr(&coro, Code, '$!do'));
        nqp::ifnull($coro($block), Nil);
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
                $end = nqp::p6bool(nqp::isnull($parcel));
                nqp::push($rpa, $parcel) unless $end;
                $count = $count - 1;
            }
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

    my sub coro(\block) {
#?if parrot
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
#?endif
#?if !parrot
        die "GatherIter NYI on JVM backend"
#?endif
    }
}


sub GATHER(\block, Mu :$infinite) { 
    GatherIter.new( block, :$infinite ).list;  
}

