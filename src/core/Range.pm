class Range is Iterable {
    has $.min;
    has $.max;
    has $.excludes_min;
    has $.excludes_max;

    proto method new(|$) { * }
    multi method new($min, $max, :$excludes_min, :$excludes_max) {
        my $new = self.CREATE;
        $new.BUILD($min, $max, $excludes_min, $excludes_max)
    }
    multi method new($min, Whatever $max, :$excludes_min, :$excludes_max) {
        my $new = self.CREATE;
        $new.BUILD($min, $Inf, $excludes_min, $excludes_max)
    }

    method BUILD($min, $max, $excludes_min, $excludes_max) {
        $!min = $min;
        $!max = $max;
        $!excludes_min = $excludes_min;
        $!excludes_max = $excludes_max;
        self;
    }

    method flat()     { nqp::p6list(nqp::list(self), List, 1.Bool) }
    method infinite() { $.max == $Inf }
    method iterator() { self }
    method list()     { self.flat }

    method reify($n is copy = 10) {
        $n = nqp::istype($n, Whatever) ?? $Inf !! $n.Num;
        fail "request for infinite elements from range"
          if $n == $Inf && self.infinite;
        my $value = $!excludes_min ?? $!min.succ !! $!min;
        my $cmpstop = $!excludes_max ?? 0 !! 1;
        my Mu $rpa := pir::new__Ps('ResizablePMCArray');
        if Int.ACCEPTS($value) || Num.ACCEPTS($value) {
            # Q:PIR optimized for int/num ranges
            $value = $value.Num;
            my $max = $!max.Num;
            Q:PIR {
                .local pmc rpa, value_pmc, n_pmc
                .local num value, n, max
                .local int cmpstop
                rpa = find_lex '$rpa'
                value_pmc = find_lex '$value'
                value = repr_unbox_num value_pmc
                n_pmc = find_lex '$n'
                n = repr_unbox_num n_pmc
                $P0 = find_lex '$max'
                max = repr_unbox_num $P0
                $P0 = find_lex '$cmpstop'
                cmpstop = repr_unbox_int $P0
              loop:
                unless n > 0 goto done
                $I0 = cmp value, max
                unless $I0 < cmpstop goto done
                $P0 = perl6_box_num value
                push rpa, $P0
                inc value
                dec n
                goto loop
              done:
                $P0 = perl6_box_num value
                '&infix:<=>'(value_pmc, $P0)
                %r = rpa
            };
        }    
        else {
          (pir::push__vPP($rpa, $value++); $n--)
              while $n > 0 && ($value cmp $!max) < $cmpstop;
        }
        if ($value cmp $!max) < $cmpstop {
            pir::push__vPP($rpa,
                ($value.succ cmp $!max < $cmpstop)
                   ?? self.CREATE.BUILD($value, $!max, 0, $!excludes_max)
                   !! $value);
        }
        pir__perl6_box_rpa__PP($rpa)
    }

    multi method gist(Range:D:) { self.perl }
    multi method perl(Range:D:) { 
        $.min 
          ~ ('^' if $.excludes_min)
          ~ '..'
          ~ ('^' if $.excludes_max)
          ~ $.max
    }

    multi method DUMP(Range:D:) {
        self.DUMP-ID() ~ '('
          ~ ':min(' ~ DUMP($!min) ~ '), '
          ~ ':max(' ~ DUMP($!max) ~ ')'
          ~ ')'
    }
}


###  XXX remove the (1) from :excludes_min and :excludes_max below
sub infix:<..>($min, $max) { 
    Range.new($min, $max) 
}
sub infix:<^..>($min, $max) { 
    Range.new($min, $max, :excludes_min(1)) 
}
sub infix:<..^>($min, $max) { 
    Range.new($min, $max, :excludes_max(1)) 
}
sub infix:<^..^>($min, $max) {
    Range.new($min, $max, :excludes_min(1), :excludes_max(1)) 
}
sub prefix:<^>($max) {
    Range.new(0, $max, :excludes_max(1)) 
}
