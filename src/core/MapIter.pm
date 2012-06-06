my class MapIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has Mu $!listiter;         # the list we're consuming
    has $!block;               # the block we're applying
    has $!first;               # Is this the first iterator in the sequence?
    has Mu $!items;            # reified items we haven't consumed yet

    method new(:$list!, :$block!) { 
        my $new := nqp::create(self);
        $new.BUILD(nqp::p6listiter(nqp::list(nqp::p6decont($list)), $new), 
                   $block, True);
        $new;
    }

    method BUILD(Mu \$listiter, \$block, $first = False) { 
        nqp::bindattr($listiter, ListIter, '$!list', self) if nqp::isconcrete($listiter);
        $!listiter := $listiter; 
        $!block = $block; 
        $!first = $first;
        self 
    }

    method flattens() { 1 }

    method reify($n = 1) {
        unless nqp::isconcrete($!reified) {
            my $argc   = $!block.count;
            $argc = 1 if $argc < 1;
            my $block  = nqp::p6decont($!block); ### TODO: Why?
            my Mu $rpa := nqp::list();

            if $!first {
                $!items := nqp::list();
                pir::perl6_set_block_first_flag__vP($block)
                  if (nqp::can($block, 'phasers') && $block.phasers('FIRST'));
            }

            my $count = $n;
            if nqp::istype($count, Whatever) {
                $!listiter.reify(*) if nqp::elems($!items) < $argc;
                $count = (nqp::elems($!items) / $argc).floor;
                $count = 1 if $count < 1;
                $count = 100000 if $count > 100000;
            }

            my int $NEXT = nqp::can($block, 'fire_phasers') 
                             && $block.phasers('NEXT');

            Q:PIR {
                .local int argc, count, NEXT
                .local pmc handler, self, MapIter, items, args, result, block, rpa
                $P0      = find_lex '$argc'
                argc     = repr_unbox_int $P0
                $P0      = find_lex '$count'
                count    = repr_unbox_int $P0
                self     = find_lex 'self'
                rpa      = find_lex '$rpa'
                MapIter = find_lex 'MapIter'
                items    = getattribute self, MapIter, '$!items'
                args     = new 'ResizablePMCArray'
                block    = find_lex '$block'
                handler  = root_new ['parrot';'ExceptionHandler']
                NEXT     = find_lex '$NEXT'

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
                push rpa, result
                goto next
              catch:
                .local pmc exception, type
                .get_results (exception)
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
                #nqp::say('next map iter');
                #nqp::say(nqp::elems($!items));
                #nqp::say(DUMP($!listiter));
                my $nextiter := nqp::create(self).BUILD($!listiter, $!block);
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

    method REIFY(Parcel \$parcel, Mu \$nextiter) {
        #nqp::say("REIFY " ~ $nextiter.^name);
        nqp::splice($!items, nqp::getattr($parcel, Parcel, '$!storage'),
                    nqp::elems($!items), 0);
        $!listiter := $nextiter;
        $parcel
    }

    method DUMP() {
        self.DUMP-ID() ~ '('
          ~ ':reified(' ~ DUMP($!reified) ~ '), '
          ~ ':items(' ~ DUMP($!items) ~'), '
          ~ ':listiter(' ~ DUMP($!listiter) ~ ')'
          ~ ')'
    }
}
