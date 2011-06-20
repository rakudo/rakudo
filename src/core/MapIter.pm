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

    method reify($n is copy = 1) {
        if !$!reified.defined {
            ## we don't have &prefix:<|> or good control blocks yet,
            ## so we'll temporarily implement MapIter with Q:PIR blocks.

            my $count = $!block.count;
            my $block := pir::perl6_decontainerize__PP($!block);
            $n = 1000 if Whatever.ACCEPTS($n);
            my Mu $rpa := pir::new__Ps('ResizablePMCArray');
            my Mu $args := 
                pir__perl6_unbox_rpa__PP($!list.munch($!block.count));
            while $args && $n > 0 {
                pir::push__vPP(
                    $rpa,
                    Q:PIR {
                        $P0 = find_lex '$args'
                        $P1 = find_lex '$block'
                        %r = $P1($P0 :flat)
                    });
                $n = $n - 1;
                $args := pir__perl6_unbox_rpa__PP($!list.munch($count))
                  if $n > 0;
            }
            # create the next MapIter if we haven't reached the end
            pir::push__vPP($rpa, self.CREATE.BUILD($!list, $!block))
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


