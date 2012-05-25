my class MapIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has $!list;                # the list we're consuming
    has $!block;               # the block we're applying
    has int $!followup;        # is this an iterator that continues an existing iteration?

    method new(:$list!, :$block!) { 
        my $new := nqp::create(self).BUILD($list, $block);
        $new;
    }

    method BUILD(\$list, \$block) { 
        $!list = $list; 
        $!block = $block; 
        self 
    }

    method reify($n = 1, :$sink) {
        if !$!reified.defined {
            ## we don't have good control blocks yet, so we'll 
            ## temporarily implement MapIter with Q:PIR blocks.
            my $argc = $!block.count;
            $argc = 1 if $argc < 1;
            my $count;
            if nqp::istype($n, Whatever) {
                $count = ($!list.gimme(*).Num / $argc).ceiling.Int
            }
            else {
                $count = $n.Int; $!list.gimme($argc * $count);
            }
            $count = 1 if $count < 1;
            my Mu $rpa  := nqp::list();
            my $list    := nqp::p6decont($!list);
            my $block   := nqp::p6decont($!block); ### TODO: Why?
            my $munched := $!list.munch($argc * $count);
            my $NEXT;
            if pir::can__IPs($block, 'phasers') && $block.phasers('NEXT') -> @NEXT {
                if @NEXT.elems == 1 {
                    $NEXT := @NEXT[0];
                }
                else {
                    $NEXT := -> {
                        .() for @NEXT;
                    };
                }
            }
            unless $!followup {
                if pir::can__IPs($block, 'phasers') && $block.phasers('FIRST') {
                    pir::perl6_set_block_first_flag__vP($block);
                }
            }
            my Mu $args := Q:PIR {
                .local int count, argc, munchpos
                .local pmc rpa, args, block, list, munched, result, Parcel, List, NEXT
                rpa      = find_lex '$rpa'
                list     = find_lex '$list'
                block    = find_lex '$block'
                $P0      = find_lex '$argc'
                argc     = repr_unbox_int $P0
                List     = find_lex 'List'
                Parcel   = find_lex 'Parcel'
                $P0      = find_lex '$munched'
                munched  = getattribute $P0, Parcel, '$!storage'
                $P0      = find_lex '$count'
                count    = repr_unbox_int $P0
                NEXT     = find_lex '$NEXT'
                munchpos = 0
                args     = root_new ['parrot';'ResizablePMCArray']
                .local pmc handler
                handler = root_new ['parrot';'ExceptionHandler']
                set_addr handler, catch
                handler.'handle_types'(.CONTROL_LOOP_LAST, .CONTROL_LOOP_NEXT, .CONTROL_LOOP_REDO)
                push_eh handler
              next:
                $I0 = elements rpa
                unless $I0 < count goto done
                args = 0
                $I0 = 0
              arg_list_loop:
                if $I0 == argc goto arg_list_loop_end
                $I1 = exists munched[munchpos]
                unless $I1 goto arg_list_loop_end
                $P0 = munched[munchpos]
                args[$I0] = $P0
                inc munchpos
                inc $I0
                goto arg_list_loop
              arg_list_loop_end:
                unless args goto done
                result = block(args :flat)
                push rpa, result
                unless NEXT goto no_next_phaser
                NEXT()
              no_next_phaser:
                goto next
              catch:
                .local pmc exception, type
                .get_results (exception)
                result = getattribute exception, 'payload'
                push rpa, result
                type = getattribute exception, 'type'
                if type == .CONTROL_LOOP_LAST goto last
                if type == .CONTROL_LOOP_REDO goto redo
                unless NEXT goto next
                NEXT()
                goto next
              redo:
                $I0 = elements args
                munchpos -= $I0
                goto next
              last:
                args = perl6_booleanize 0
              done:
                null $P0
                perl6_shiftpush $P0, munched, munchpos
                unless munched goto uneaten_saved
                $P0 = getattribute list, List, '$!items'
                splice $P0, munched, 0, 0
              uneaten_saved:
                pop_eh
                %r = args
            };
            # create the next MapIter if we haven't reached the end
            if $args {
                my $iter := nqp::create(self).BUILD($!list, $!block);
                nqp::bindattr_i($iter, MapIter, '$!followup', 1);
                nqp::push($rpa, $iter);
            }
            elsif pir::can__IPs($block, 'phasers') && $block.phasers('LAST') -> @LAST {
                .() for @LAST;
            }
            $!reified := nqp::p6parcel($rpa, nqp::null());
            $!list = Any;
            $!block = Any;
        }
        $!reified
    }

    method DUMP() {
        self.DUMP-ID() ~ '('
          ~ ':reified(' ~ DUMP($!reified) ~ '), '
          ~ ':list(' ~ DUMP($!list) ~ '), '
          ~ ':block(' ~ DUMP($!block) ~ ')'
          ~ ')'
    }
}


