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

            # build an RPA that we can form into our reified parcel
            my Mu $rpa := pir::new__Ps('ResizablePMCArray');

            my Mu $args := 
                pir__perl6_unbox_rpa__PP($!list.munch($!block.count));
            my $block := $!block;   # easier to access from Q:PIR
            my Mu $parcel;
            $n = 1 if Whatever.ACCEPTS($n);
            while $args && $n > 0 {
                $parcel := Q:PIR {
                               $P0 = find_lex '$args'
                               $P1 = find_lex '$block'
                               $P1 = perl6_decontainerize $P1
                               %r = $P1($P0 :flat)
                           };
                pir::push__vPP($rpa, $parcel);
                $n = $n - 1;
                $args := pir__perl6_unbox_rpa__PP($!list.munch($!block.count))
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

    method DUMP() {
        self.DUMP-ID() ~ '('
          ~ ':reified(' ~ DUMP($!reified) ~ '), '
          ~ ':list(' ~ $!list.DUMP-ID() ~ '), '
          ~ ':block(' ~ DUMP($!block) ~ ')'
          ~ ')'
    }
}


