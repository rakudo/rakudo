my class MapIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has Mu $!listiter;         # the list we're consuming
    has Mu $!flattens;         # flag to flatten input list
    has $!block;               # the block we're applying
    has $!first;               # Is this the first iterator in the sequence?
    has Mu $!items;            # reified items we haven't consumed yet
    has Mu $!label;            # The label that might be attached to us

    method new($list, $block, Mu $flattens = Bool::True, :$label) { 
        my $new := nqp::create(self);
        $new.BUILD(nqp::p6listiter(nqp::list(nqp::decont($list)), $new), 
                   $block, $flattens, True, :$label);
        $new;
    }

    method BUILD(Mu \listiter, \block, Mu \flattens, $first = False, :$label) { 
        nqp::bindattr(listiter, ListIter, '$!list', self) if nqp::isconcrete(listiter);
        $!listiter := listiter; 
        $!block = block; 
        $!first = $first;
        $!flattens = flattens;
        $!label := $label;
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
                $!items := nqp::list();
                nqp::p6setfirstflag($block)
                  if (nqp::can($block, 'phasers') && $block.phasers('FIRST'));
            }

            my $count = $n;
            if nqp::istype($count, Whatever) {
                $!listiter.reify($argc)
                if $!listiter && nqp::elems($!items) < $argc;
                $count = (nqp::elems($!items) / $argc).floor;
                $count = 1 if $count < 1;
                $count = 100000 if $count > 100000;
            }

            my int $NEXT        = nqp::can($block, 'fire_phasers')
                                  && +$block.phasers('NEXT');
            my int $is_sink     = $sink ?? 1 !! 0;
            my int $did_iterate = 0;
            my Mu $label       := $!label;

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
            };
            if $!label {
                Q:PIR { handler.'handle_types'(.CONTROL_LOOP_LAST, .CONTROL_LOOP_NEXT, .CONTROL_LOOP_REDO, 512, 513, 514) };
                1
            }
            else {
                Q:PIR { handler.'handle_types'(.CONTROL_LOOP_LAST, .CONTROL_LOOP_NEXT, .CONTROL_LOOP_REDO) };
                1
            }
            Q:PIR {
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
                store_lex '$did_iterate', 1
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
            };
            if $!label {
                Q:PIR {
                    .local int id1_reg, id2_reg
                    .local pmc label
                    label = find_lex '$label'
                    id1_reg = get_id result
                    id2_reg = label
                    if id1_reg != id2_reg goto rethrow
                    if type == 512 goto next
                    if type == 513 goto redo
                    if type == 514 goto last
                  rethrow:
                    rethrow exception # XXX Should that be perl6_based_rethrow?
                };
                1
            }
            Q:PIR {
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
            nqp::setelems($rpa, $count min 1024) unless $sink;
            nqp::setelems($rpa, 0);
            
            if $argc == 1 && !$NEXT {
                # Fast path case: only 1 argument for each block, no NEXT phaser.
                $!label ??
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
                            ($sink ?? ($did_iterate = 1; $block($arg))
                                   !! nqp::push($rpa, $block($arg))),
                            $state = 1
                        ))
                    ),
                    'LABELED', nqp::decont($label),
                    'LAST', nqp::stmts(
                        ($!items := Any),
                        ($!listiter := Any),
                        ($state = 0)
                    ),
                    'REDO', $state = 2,
                    'NEXT', $state = 1
                )) !!
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
                            ($sink ?? ($did_iterate = 1; $block($arg)) 
                                   !! nqp::push($rpa, $block($arg))),
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
                $!label ??
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
                                ?? ($did_iterate = 1; nqp::p6invokeflat($block, $args))
                                !! nqp::push($rpa, nqp::p6invokeflat($block, $args))),
                            $state = 3
                        )),
                        nqp::if(nqp::iseq_i($state, 3), nqp::stmts(
                            nqp::if($NEXT, $block.fire_phasers('NEXT')),
                            ($state = 1)
                        ))
                    ),
                    'LABELED', nqp::decont($label),
                    'LAST', nqp::stmts(
                        ($!items := Any),
                        ($!listiter := Any),
                        ($state = 0)
                    ),
                    'REDO', $state = 2,
                    'NEXT', $state = 3
                )) !!
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
                                ?? ($did_iterate = 1; nqp::p6invokeflat($block, $args))
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
                my $nextiter := nqp::create(self).BUILD($!listiter, $!block, $!flattens, :$!label);
                nqp::bindattr($nextiter, MapIter, '$!items', $!items);
                nqp::push($rpa, $nextiter);
            }
            elsif ($did_iterate || nqp::elems($rpa) || $rpa) && nqp::can($block, 'fire_phasers') {
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

# vim: ft=perl6 expandtab sw=4
