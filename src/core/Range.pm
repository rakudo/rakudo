class Range is Iterable does Positional {
    has $.min;
    has $.max;
    has $.excludes_min;
    has $.excludes_max;

    proto method new(|$) { * }
    multi method new($min, $max, :$excludes_min, :$excludes_max) {
        nqp::create(self).BUILD($min, $max, $excludes_min, $excludes_max)
    }
    multi method new($min, Whatever $max, :$excludes_min, :$excludes_max) {
        nqp::create(self).BUILD($min, $Inf, $excludes_min, $excludes_max)
    }
    multi method new(Whatever $min, $max, :$excludes_min, :$excludes_max) {
        nqp::create(self).BUILD(-$Inf, $max, $excludes_min, $excludes_max)
    }
    multi method new(Whatever $min, Whatever $max, :$excludes_min, :$excludes_max) {
        fail "*..* is not a valid range";
    }
    

    submethod BUILD($min, $max, $excludes_min, $excludes_max) {
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
        my $value = $!excludes_min ?? $!min.succ !! $!min;
        # Iterating a Str range delegates to iterating a sequence.
        if Str.ACCEPTS($value) {
            return $value after $!max
                     ?? ()
                     !! SEQUENCE($value, $!max, :exclude_end($!excludes_max)).iterator.reify($n)
        } 
        my $count;
        if nqp::istype($n, Whatever) {
            $count = self.infinite ?? 10 !! $Inf;
        }
        else {
            $count = $n.Num;
            fail "request for infinite elements from range"
              if $count == $Inf && self.infinite;
        }
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
                $P0 = perl6_box_bigint value
                perl6_container_store value_pmc, $P0
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
                   ?? nqp::create(self).BUILD($value, $!max, 0, $!excludes_max)
                   !! $value);
        }
        nqp::p6parcel($rpa, nqp::null());
    }

    method at_pos($pos) { self.flat.at_pos($pos) }

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

    proto method roll(|$) { * }
    multi method roll(Range:D: Whatever) {
        gather loop { take self.roll }
    }
    multi method roll(Range:D:) {
        return self.list.roll unless $!min.^isa(Int) && $!max.^isa(Int);
        my Int:D $least = $!excludes_min ?? $!min + 1 !! $!min;
        my Int:D $elems = 1 + ($!excludes_max ?? $!max - 1 !! $!max) - $least;
        $elems ?? ($least + $elems.rand.floor) !! Any;
    }
    multi method roll(Cool $num as Int) {
        return self.list.roll($num) unless $!min.^isa(Int) && $!max.^isa(Int);
        return self.roll if $num == 1;
        my int $n = nqp::unbox_i($num);
        gather loop (my int $i = 0; $i < $n; $i = $i + 1) {
            take self.roll;
        }
    }

    proto method pick(|$)        { * }
    multi method pick()          { self.roll };
    multi method pick(Whatever)  { self.list.pick(*) };
    multi method pick(Cool $n as Int) {
        return self.list.pick($n) unless $!min.^isa(Int) && $!max.^isa(Int);
        return self.roll if $n == 1;
        my Int:D $least = $!excludes_min ?? $!min + 1 !! $!min;
        my Int:D $elems = 1 + ($!excludes_max ?? $!max - 1 !! $!max) - $least;
        return self.list.pick($n) unless $elems > 3 * $n;
        my %seen;
        my int $i_n = nqp::unbox_i($n);
        gather while $i_n > 0 {
            my Int $x = $least + $elems.rand.floor;
            unless %seen{$x} {
                %seen{$x} = 1;
                $i_n = $i_n - 1;
                take $x;
            }
        }
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
    Range.new(0, $max.Numeric, :excludes_max) 
}
