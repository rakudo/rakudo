my class MapIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has Mu $!listiter;         # the list we're consuming
    has Mu $!flattens;         # flag to flatten input list
    has $!block;               # the block we're applying
    has $!first;               # Is this the first iterator in the sequence?
    has Mu $!items;            # reified items we haven't consumed yet
    has Mu $!label;            # The label that might be attached to us

    method new($list, $block, Mu $flattens = Bool::True, :$label) {
        nqp::create(self).BUILD(
          nqp::p6listiter(nqp::list(nqp::decont($list)), self),
          $block, $flattens, True, :$label);
    }

    submethod BUILD(Mu \listiter, $!block, Mu $!flattens, $!first, :$label) {
        nqp::bindattr(listiter, ListIter, '$!list', self)
          if nqp::isconcrete(listiter);
        $!listiter := listiter;
        $!label    := $label;
        self
    }

    method flattens() { $!flattens }

    method reify($n, :$sink) {
        unless nqp::isconcrete($!reified) {
            my $argc   := $!block.count;
            $argc := 1 if $argc < 1 || $argc == Inf;
            my $block  := nqp::decont($!block);
            my Mu $rpa := nqp::list();

            if $!first {
                $!items := nqp::list();
                nqp::p6setfirstflag($block)
                  if (nqp::can($block, 'phasers') && $block.phasers('FIRST'));
            }

            my $count := $n;
            if nqp::istype($count, Whatever) {
                $!listiter.reify($argc)
                if $!listiter && nqp::elems($!items) < $argc;
                $count := (nqp::elems($!items) / $argc).floor;
                $count := 1 if $count < 1;
                $count := 100000 if $count > 100000;
            }

            my int $NEXT        = nqp::can($block, 'fire_phasers')
                                  && +$block.phasers('NEXT');
            my int $is_sink     = $sink ?? 1 !! 0;
            my int $did_iterate = 0;
            my Mu $label       := $!label;

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
                            ($sink ?? SEQ($did_iterate = 1; $block($arg))
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
                            ($sink ?? SEQ($did_iterate = 1; $block($arg))
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
                                ?? SEQ($did_iterate = 1; nqp::p6invokeflat($block, $args))
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
                                ?? SEQ($did_iterate = 1; nqp::p6invokeflat($block, $args))
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

            if $!items || $!listiter {
                my $nextiter := nqp::create(self).BUILD($!listiter, $!block, $!flattens, False, :$!label);
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
