class Range is Iterable does Positional {
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
    multi method new(Whatever $min, $max, :$excludes_min, :$excludes_max) {
        my $new = self.CREATE;
        $new.BUILD(-$Inf, $max, $excludes_min, $excludes_max)
    }
    multi method new(Whatever $min, Whatever $max, :$excludes_min, :$excludes_max) {
        fail "*..* is not a valid range";
    }
    

    method BUILD($min, $max, $excludes_min, $excludes_max) {
        $!min = $min;
        $!max = $max;
        $!excludes_min = $excludes_min.Bool;
        $!excludes_max = $excludes_max.Bool;
        self;
    }

    method flat()     { nqp::p6list(nqp::list(self), List, Bool::True) }
    method infinite() { $.max == $Inf }
    method iterator() { self }
    method list()     { self.flat }

    method bounds()   { ($!min, $!max) }

    multi method ACCEPTS(Range:D: Mu \$topic) {
        ($topic cmp $!min) > -(!$!excludes_min)
            and ($topic cmp $!max) < +(!$!excludes_max)
    }

    multi method ACCEPTS(Range:D: Range \$topic) {
        ?( $.min eqv $topic.min
           && $.max eqv $topic.max
           && $.excludes_min === $topic.excludes_min
           && $.excludes_max === $topic.excludes_max)
    }

    method reify($n = 10) {
        my $count;
        if nqp::istype($n, Whatever) {
            $count = self.infinite ?? 10 !! $Inf;
        }
        else {
            $count = $n.Num;
            fail "request for infinite elements from range"
              if $count == $Inf && self.infinite;
        }
        my $value = $!excludes_min ?? $!min.succ !! $!min;
        my $cmpstop = $!excludes_max ?? 0 !! 1;
        my Mu $rpa := nqp::list();
        if Int.ACCEPTS($value) || Num.ACCEPTS($value) {
            # Q:PIR optimized for int/num ranges
            $value = $value.Num;
            my $max = $!max.Num;
            Q:PIR {
                .local pmc rpa, value_pmc, count_pmc
                .local num value, count, max
                .local int cmpstop
                rpa = find_lex '$rpa'
                value_pmc = find_lex '$value'
                value = repr_unbox_num value_pmc
                count_pmc = find_lex '$count'
                count = repr_unbox_num count_pmc
                $P0 = find_lex '$max'
                max = repr_unbox_num $P0
                $P0 = find_lex '$cmpstop'
                cmpstop = repr_unbox_int $P0
              loop:
                unless count > 0 goto done
                $I0 = cmp value, max
                unless $I0 < cmpstop goto done
                $P0 = perl6_box_bigint value
                push rpa, $P0
                inc value
                dec count
                goto loop
              done:
                $P0 = perl6_box_num value
                '&infix:<=>'(value_pmc, $P0)
                %r = rpa
            };
        }    
        else {
          (nqp::push($rpa, $value++); $count--)
              while $count > 0 && ($value cmp $!max) < $cmpstop;
        }
        if ($value cmp $!max) < $cmpstop {
            nqp::push($rpa,
                ($value.succ cmp $!max < $cmpstop)
                   ?? self.CREATE.BUILD($value, $!max, 0, $!excludes_max)
                   !! $value);
        }
        nqp::p6parcel($rpa, nqp::null());
    }

    method at_pos($pos) { self.flat.at_pos($pos) }

    multi method gist(Range:D:) { self.perl }
    multi method perl(Range:D:) { 
        $.min.perl
          ~ ('^' if $.excludes_min)
          ~ '..'
          ~ ('^' if $.excludes_max)
          ~ $.max.perl
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
    Range.new($min, $max, :excludes_min) 
}
sub infix:<..^>($min, $max) { 
    Range.new($min, $max, :excludes_max) 
}
sub infix:<^..^>($min, $max) {
    Range.new($min, $max, :excludes_min, :excludes_max) 
}
sub prefix:<^>($max) {
    Range.new(0, $max, :excludes_max) 
}
