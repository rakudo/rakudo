class Rangeint is Range does Positional {
    has int $.min;
    has int $.max;
    has int $.elems;
    has Mu $!list;

    method new(int $min, int $max) { nqp::create(self).BUILD($min,$max) }

    submethod BUILD(int $min, int $max) {
        $!min   = $min;
        $!max   = $max;
        $!elems = $max - $min + 1;
        $!list := nqp::null();

        self;
    }

    method !list() {
        if nqp::isnull($!list) {
            $!list := nqp::list();
            nqp::setelems($!list,$!elems);
            my int $pos;
            my int $val = $!min;
            while $pos < $!elems {
                nqp::bindpos($!list,$pos,nqp::p6box_i($val));
                $val = $val + 1;
                $pos = $pos + 1;
            }
        }
        $!list;
    }

    multi method WHICH (Rangeint:D:)   { self.^name ~ "|$!min..$!max" }
    multi method infinite(Rangeint:D:) { False }
    multi method Numeric(Rangeint:D:)  { $!elems }

    method flat(Rangeint:D:)         {
        nqp::p6parcel(nqp::decont(self!list),nqp::null);
    }
    method excludes-min(Rangeint:D:) { False }
    method excludes-max(Rangeint:D:) { False }
    method iterator(Rangeint:D:)     { self }
    method reify(Rangeint:D: $n)     { self.flat }
    method list(Rangeint:D:)         { self.flat }
    method bounds(Rangeint:D:)       { ($!min, $!max) }

    multi method ACCEPTS(Rangeint:D: Mu \topic) {
        (topic cmp $!min) > -1 and (topic cmp $!max) < 1;
    }
    multi method ACCEPTS(Rangeint:D: Rangeint:D \topic) {
        $!min >= topic.min && $!max <= topic.max;
    }
    multi method ACCEPTS(Rangeint:D: Range:D \topic) {
           (topic.min > $!min || topic.min == $!min && !topic.excludes-min)
        && (topic.max < $!max || topic.max == $!max && !topic.excludes-max)
    }

    multi method at_pos(Rangeint:D: int \pos)   { $!min + pos }
    multi method at_pos(Rangeint:D: Int:D \pos) { $!min + pos }

    multi method perl(Rangeint:D:) { "$!min..$!max" }
    multi method gist(Rangeint:D:) { $!min ?? "$!min..$!max" !! "^{$!max + 1}" }

    multi method roll(Rangeint:D:)          { $!min + nqp::rand_I($!elems,Int) }
    multi method roll(Rangeint:D: Whatever) { gather loop { take self.roll } }
    multi method roll(Rangeint:D: Cool $n)  {
        my int $todo = nqp::unbox_i(nqp::istype($n,Int) ?? $n !! $n.Int);
        if $todo == 1 {
            self.roll;
        }
        else {
            gather while $todo {
                take self.roll;
                $todo = $todo - 1;
            }
        }
    }

    multi method pick(Rangeint:D:)          { self.roll }
    multi method pick(Rangeint:D: Whatever) { self.pick($!elems) }
    multi method pick(Rangeint:D: Cool $n)  {
        my int $todo = nqp::unbox_i(nqp::istype($n,Int) ?? $n !! $n.Int);
        if $todo == 1 {
            self.roll;
        }
        else {
            my int $elems = $!elems;
            $todo = $elems if $todo > $elems;
            self!list;                   # make sure we populated $!list
            my Mu $empty := nqp::list(); # stuff to splice in

            gather {
                while $todo {
                    my Int $pos = nqp::unbox_i(nqp::rand_I($elems,Int));
                    take nqp::p6box_i(nqp::atpos($!list,$!min + $pos));

                    nqp::splice($!list,$empty,$pos,1);
                    $elems - $elems - 1;
                    $todo  - $todo  - 1;
                }
                $!list := nqp::null(); # clear out for next usage
            }
        }
    }

    method excludes_min() {
        DEPRECATED('excludes-min', |<2014.12 2015.12>);
        False;
    }
    method excludes_max() {
        DEPRECATED('excludes-max', |<2014.12 2015.12>);
        False;
    }
}

#sub infix:<..>($min, $max) {
#    Range.new($min, $max)
#}
#sub infix:<^..>($min, $max) {
#    Range.new($min, $max, :excludes-min)
#}
#sub infix:<..^>($min, $max) {
#    Range.new($min, $max, :excludes-max)
#}
#sub infix:<^..^>($min, $max) is pure {
#    Range.new($min, $max, :excludes-min, :excludes-max)
#}
#sub prefix:<^>($max) {
#    Range.new(0, $max.Numeric, :excludes-max)
#}

multi sub infix:<eqv>(Rangeint:D \a, Rangeint:D \b) {
   a.min eqv b.min && a.max eqv b.max
}

# vim: ft=perl6 expandtab sw=4
