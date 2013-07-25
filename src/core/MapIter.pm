my class MapIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has Mu $!listiter;         # the list we're consuming
    has Mu $!flattens;         # flag to flatten input list
    has $!block;               # the block we're applying
    has $!first;               # Is this the first iterator in the sequence?
    has Mu $!items;            # reified items we haven't consumed yet

    method new($list, $block, Mu $flattens = Bool::True) { 
        my $new := nqp::create(self);
        $new.BUILD(nqp::p6listiter(nqp::qlist(nqp::decont($list)), $new), 
                   $block, $flattens, True);
        $new;
    }

    method BUILD(Mu \listiter, \block, Mu \flattens, $first = False) { 
        nqp::bindattr(listiter, ListIter, '$!list', self) if nqp::isconcrete(listiter);
        $!listiter := listiter; 
        $!block = block; 
        $!first = $first;
        $!flattens = flattens;
        self 
    }

    method flattens() { $!flattens }

    method reify($n = 1, :$sink) {
        unless nqp::isconcrete($!reified) {
            my $argc   = $!block.count;
            $argc = 1 if $argc < 1 || $argc ~~ Inf;
            my $block  := nqp::decont($!block);
            my Mu $rpa := nqp::list();

            if $!first {
                $!items := nqp::qlist();
                nqp::p6setfirstflag($block)
                  if (nqp::can($block, 'phasers') && $block.phasers('FIRST'));
            }

            my $count = $n;
            if nqp::istype($count, Whatever) {
                $!listiter.reify(*) 
                  if $!listiter && nqp::elems($!items) < $argc;
                $count = (nqp::elems($!items) / $argc).floor;
                $count = 1 if $count < 1;
                $count = 100000 if $count > 100000;
            }

            my int $NEXT = nqp::can($block, 'fire_phasers') 
                             && +$block.phasers('NEXT');
            my int $is_sink = $sink ?? 1 !! 0;

#?if parrot
            Q:PIR {
                .local int argc, count, NEXT, is_sink
                .local pmc handler, self, MapIter, items, args, result, block, rpa
                $P0      = find_lex '$argc'
                argc     = repr_unbox_int $P0
                $P0      = find_lex '$count'
                count    = repr_unbox_int $P0
                self     = find_lex 'self'
                rpa      = find_lex '$rpa'
                MapIter = find_lex 'MapIter'
                items    = getattribute self, MapIter, '$!items'
                args     = new 'QRPA'
                block    = find_lex '$block'
                handler  = root_new ['parrot';'ExceptionHandler']
                NEXT     = find_lex '$NEXT'
                is_sink  = find_lex '$is_sink'

                set_addr handler, catch
                handler.'handle_types'(.CONTROL_LOOP_LAST, .CONTROL_LOOP_NEXT, .CONTROL_LOOP_REDO)
                push_eh handler

              iter_loop:
                $I0 = elements rpa
                unless $I0 < count goto iter_done
                $I0 = elements items
                if $I0 >= argc goto have_items
                $I0 = argc - $I0
                $P0 = getattribute self, MapIter, '$!listiter'
                unless $P0 goto have_items
                $P0.'reify'($I0)
              have_items:
                args = 0
                perl6_shiftpush args, items, argc
                unless args goto iter_done
              redo:
                result = block(args :flat)
                if is_sink goto sink_result
                push rpa, result
                goto next
              sink_result:
                $I0 = repr_defined result
                unless $I0 goto next
                $I0 = can result, 'sink'
                unless $I0 goto next
                $I0 = defined result
                unless $I0 goto next
                result.'sink'()
                goto next
              catch:
                .local pmc exception, type
                .get_results (exception)
                null $P0
                perl6_invoke_catchhandler $P0, exception
                result = getattribute exception, 'payload'
                push rpa, result
                type = getattribute exception, 'type'
                if type == .CONTROL_LOOP_REDO goto redo
                if type == .CONTROL_LOOP_LAST goto last
              next:
                unless NEXT goto iter_loop
                block.'fire_phasers'('NEXT')
                goto iter_loop
              last:
                $P0 = find_lex 'Any'
                setattribute self, MapIter, '$!items', $P0
                setattribute self, MapIter, '$!listiter', $P0
              iter_done:
                pop_eh
            };
#?endif
#?if !parrot
            my int $state = 1;
            my int $itmp;
            my Mu $items := $!items;
            my Mu $args := nqp::list();
            my Mu $arg;
            
            # Pre-size (set to count we want, then back to zero, which leaves
            # the backing array at $count).
            nqp::setelems($rpa, $count);
            nqp::setelems($rpa, 0);
            
            if $argc == 1 && !$NEXT {
                # Fast path case: only 1 argument for each block, no NEXT phaser.
                nqp::while(($state && nqp::islt_i(nqp::elems($rpa), $count)), nqp::handle(
                    nqp::stmts(
                        nqp::if(nqp::iseq_i($state, 1), nqp::stmts(
                            nqp::unless(nqp::elems($items), nqp::stmts(
                                nqp::if($!listiter, $!listiter.reify(1))
                            )),
                            nqp::if($items, 
                                nqp::stmts(($arg := nqp::shift($items)), $state = 2),
                                $state = 0)
                        )),
                        nqp::if(nqp::iseq_i($state, 2), nqp::stmts(
                            ($sink ?? $block($arg) !! nqp::push($rpa, $block($arg))),
                            $state = 1
                        ))
                    ),
                    'LAST', nqp::stmts(
                        ($!items := Any),
                        ($!listiter := Any),
                        ($state = 0)
                    ),
                    'REDO', $state = 2,
                    'NEXT', $state = 1
                ));
            }
            else {
                nqp::while(($state && nqp::islt_i(nqp::elems($rpa), $count)), nqp::handle(
                    nqp::stmts(
                        nqp::if(nqp::iseq_i($state, 1), nqp::stmts(
                            ($itmp = nqp::elems($items)),
                            nqp::unless($itmp >= $argc, nqp::stmts(
                                ($itmp = $argc - $itmp),
                                nqp::if($!listiter, $!listiter.reify($itmp))
                            )),
                            nqp::setelems($args, 0),
                            nqp::p6shiftpush($args, $items, $argc),
                            nqp::if($args, $state = 2, $state = 0)
                        )),
                        nqp::if(nqp::iseq_i($state, 2), nqp::stmts(
                            ($sink
                                ?? nqp::p6invokeflat($block, $args)
                                !! nqp::push($rpa, nqp::p6invokeflat($block, $args))),
                            $state = 3
                        )),
                        nqp::if(nqp::iseq_i($state, 3), nqp::stmts(
                            nqp::if($NEXT, $block.fire_phasers('NEXT')),
                            ($state = 1)
                        ))
                    ),
                    'LAST', nqp::stmts(
                        ($!items := Any),
                        ($!listiter := Any),
                        ($state = 0)
                    ),
                    'REDO', $state = 2,
                    'NEXT', $state = 3
                ));
            }
#?endif

            if $!items || $!listiter {
                my $nextiter := nqp::create(self).BUILD($!listiter, $!block, $!flattens);
                nqp::bindattr($nextiter, MapIter, '$!items', $!items);
                nqp::push($rpa, $nextiter);
            }
            elsif nqp::can($block, 'fire_phasers') {
                $block.fire_phasers('LAST');
            }

            $!reified := nqp::p6parcel($rpa, nqp::null());
            # release references to objects we no longer need/own
            $!items := Any;
            $!listiter := Any;
            $!block := Any;

        }
        $!reified;
    }

    method REIFY(Parcel \parcel, Mu \nextiter) {
        nqp::splice($!items, nqp::getattr(parcel, Parcel, '$!storage'),
                    nqp::elems($!items), 0);
        $!listiter := nextiter;
        parcel
    }

    multi method DUMP(MapIter:D: :$indent-step = 4, :%ctx?) {
        return DUMP(self, :$indent-step) unless %ctx;

        my Mu $attrs := nqp::list();
        nqp::push($attrs, '$!flattens');
        nqp::push($attrs,  $!flattens );
        nqp::push($attrs, '$!first'   );
        nqp::push($attrs,  $!first    );
        nqp::push($attrs, '$!reified' );
        nqp::push($attrs,  $!reified  );
        nqp::push($attrs, '$!items'   );
        nqp::push($attrs,  $!items    );
        nqp::push($attrs, '$!listiter');
        nqp::push($attrs,  $!listiter );
        nqp::push($attrs, '$!block'   );
        nqp::push($attrs,  $!block    );
        self.DUMP-OBJECT-ATTRS($attrs, :$indent-step, :%ctx);
    }
}
