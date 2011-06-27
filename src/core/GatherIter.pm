class GatherIter is Iterator {
    has Mu $!coro;             # coroutine to execute for more pairs
    has $!reified;             # Parcel of this iterator's results
    has $!infinite;            # true if iterator is known infinite

    method new($block, :$infinite) {
        my Mu $coro := 
            nqp::clone(nqp::getattr(&coro, Code, '$!do'));
        Q:PIR {
            $P0 = find_lex '$block'
            $P1 = find_lex '$coro'
            $P1($P0)
        };
        pir::setattribute__0PPsP(
            pir::setattribute__0PPsP(self.CREATE, GatherIter, '$!coro', $coro),
            GatherIter, '$!infinite', $infinite);
    }

    method reify($n is copy = 1) { 
        if !$!reified.defined {
            my Mu $rpa := nqp::list();
            my Mu $parcel;
            my $end;
            $n = 1 if Whatever.ACCEPTS($n);
            while !$end && $n > 0 {
                $parcel := Q:PIR {
                    $P0 = find_lex 'self'
                    $P1 = find_lex 'GatherIter'
                    $P2 = getattribute $P0, $P1, '$!coro'
                    %r = $P2()
                };
                $end = nqp::p6bool(nqp::isnull($parcel));
                nqp::push($rpa, $parcel) unless $end;
                $n = $n - 1;
            }
            nqp::push($rpa, 
                pir::setattribute__0PPsP(
                    pir::setattribute__0PPsP(
                        self.CREATE, GatherIter, '$!coro', $!coro),
                    GatherIter, '$!infinite', $!infinite))
                unless $end;
            $!reified := pir__perl6_box_rpa__PP($rpa);
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
            block()
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
        }
    }
}


sub GATHER(\$block, :$infinite) { 
    GatherIter.new( $block, :$infinite ).list;  
}

