class GatherIter is Iterator {
    has Mu $!coro;             # coroutine to execute for more pairs
    has $!reified;             # Parcel of this iterator's results
    has $!infinite;            # true if iterator is known infinite

    method new($block, Mu :$infinite) {
        my Mu $coro := 
            nqp::clone(nqp::getattr(&coro, Code, '$!do'));
        Q:PIR {
            $P0 = find_lex '$block'
            $P1 = find_lex '$coro'
            $P1($P0)
        };
        pir::setattribute__0PPsP(
            pir::setattribute__0PPsP(nqp::create(self), GatherIter, '$!coro', $coro),
            GatherIter, '$!infinite', $infinite);
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
                pir::setattribute__0PPsP(
                    pir::setattribute__0PPsP(
                        nqp::create(self), GatherIter, '$!coro', $!coro),
                    GatherIter, '$!infinite', $!infinite))
                unless $end;
            $!reified := nqp::p6parcel($rpa, nqp::null());
        }
        $!reified
    }

    method infinite() { $!infinite }

    my sub coro(\$block) {
        Q:PIR {
            .local pmc block, handler, taken
            block = find_lex '$block'
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
}


sub GATHER(\$block, Mu :$infinite) { 
    GatherIter.new( $block, :$infinite ).list;  
}

