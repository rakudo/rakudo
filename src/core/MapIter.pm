my class MapIter is Iterator {
    has $!reified;             # Parcel we return after reifying
    has $!list;                # the list we're consuming
    has $!block;               # the block we're applying

    method new(:$list!, :$block!) { 
        my $new := self.CREATE.BUILD($list, $block);
        $new;
    }

    method BUILD(\$list, \$block) { 
        $!list = $list; 
        $!block = $block; 
        self 
    }

    method reify($n is copy = 1, :$sink) {
        if !$!reified.defined {
            ## we don't have good control blocks yet, so we'll 
            ## temporarily implement MapIter with Q:PIR blocks.
            my $count = $!block.count;
            $n = nqp::istype($n, Whatever) ?? 1000 !! $n.Int;
            $!list.gimme($count * $n);
            my Mu $rpa := nqp::list();
            my $block := pir::perl6_decontainerize__PP($!block); ### TODO: Why?
            my $list := pir::perl6_decontainerize__PP($!list);
            my Mu $args := Q:PIR {
                .local int n
                .local pmc rpa, block, count, list, Parcel, List, result
                rpa    = find_lex '$rpa'
                block  = find_lex '$block'
                count  = find_lex '$count'
                list   = find_lex '$list'
                Parcel = find_lex 'Parcel'
                List   = find_lex 'List'
                $P0    = find_lex '$n'
                n      = repr_unbox_int $P0
                .local pmc handler
                handler = root_new ['parrot';'ExceptionHandler']
                set_addr handler, catch
                handler.'handle_types'(.CONTROL_LOOP_LAST, .CONTROL_LOOP_NEXT, .CONTROL_LOOP_REDO)
                push_eh handler
              next:
                $I0 = elements rpa
                unless $I0 < n goto last
                .local pmc args
                args = list.'munch'(count)
                args = getattribute args, Parcel, '$!storage'
                unless args goto last
                result = block(args :flat)
                push rpa, result
                goto next
              catch:
                .local pmc exception, type
                .get_results (exception)
                result = getattribute exception, 'payload'
                push rpa, result
                type = getattribute exception, 'type'
                if type == .CONTROL_LOOP_LAST goto last
                if type != .CONTROL_LOOP_REDO goto next
              redo:
                $P0 = getattribute list, List, '$!items'
                splice $P0, args, 0, 0
                goto next
              last:
                pop_eh
                %r = args
            };
            # create the next MapIter if we haven't reached the end
            nqp::push($rpa, self.CREATE.BUILD($!list, $!block))
              if $args;
            $!reified := nqp::p6parcel($rpa, nqp::null());
            $!list = Any;
            $!block = Any;
        }
        $!reified
    }

    method infinite() { $!list.infinite }

    method DUMP() {
        self.DUMP-ID() ~ '('
          ~ ':reified(' ~ DUMP($!reified) ~ '), '
          ~ ':list(' ~ DUMP($!list) ~ '), '
          ~ ':block(' ~ DUMP($!block) ~ ')'
          ~ ')'
    }
}


