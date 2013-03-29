my class MapIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has Mu $!listiter;         # the list we're consuming
    has Mu $!flattens;         # flag to flatten input list
    has $!block;               # the block we're applying
    has $!first;               # Is this the first iterator in the sequence?
    has Mu $!items;            # reified items we haven't consumed yet

    method new($list, $block, Mu $flattens = Bool::True) { 
        my $new := nqp::create(self);
        $new.BUILD(nqp::p6listiter(nqp::qlist(nqp::p6decont($list)), $new), 
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
            my $block  := nqp::p6decont($!block);
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

    multi method DUMP(MapIter:D: :$indent-step = 4) {
        my $before := self.DUMP-ID() ~ '(';
        my @pieces;
        @pieces.push: ':flattens(' ~ DUMP($!flattens) ~ ')';
        @pieces.push: ':first('    ~ DUMP($!first)    ~ ')';
        @pieces.push: ':reified('  ~ DUMP($!reified)  ~ ')';
        @pieces.push: ':items('    ~ DUMP($!items)    ~ ')';
        @pieces.push: ':listiter(' ~ DUMP($!listiter) ~ ')';
        @pieces.push: ':block('    ~ DUMP($!block)    ~ ')';
        @pieces.DUMP-PIECES($before, :$indent-step);
    }
}
