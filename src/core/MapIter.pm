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
            ## we don't have &prefix:<|> or good control blocks yet,
            ## so we'll temporarily implement MapIter with Q:PIR blocks.
            my $count = $!block.count;
            my $block := pir::perl6_decontainerize__PP($!block); ### TODO: Why?
            $n = nqp::istype($n, Whatever) ?? 1000 !! $n.Int;
            $!list.gimme($count * $n);
            my Mu $rpa := nqp::list();
            my Mu $args;
            repeat {
                $args := nqp::getattr($!list.munch($!block.count), 
                                      Parcel, '$!storage');
                nqp::push($rpa,
                    Q:PIR {
                        $P0 = find_lex '$args'
                        $P1 = find_lex '$block'
                        %r = $P1($P0 :flat)
                    }) if $args;
            } while $args 
                    && nqp::islt_i(nqp::elems($rpa), nqp::unbox_i($n));
            # create the next MapIter if we haven't reached the end
            nqp::push($rpa, self.CREATE.BUILD($!list, $!block))
              if $args;
            $!reified := pir__perl6_box_rpa__PP($rpa);
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


