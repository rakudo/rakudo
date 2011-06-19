class RangeIter is Iterator {
    has $!reified;
    has $.value;
    has $.max;

    method new(:$value, :$max) {
        pir::repr_instance_of__PP(RangeIter).BUILD($value, $max)
    }
    method BUILD($value, $max) {
        $!value = $value;
        $!max = $max;
        self
    }

    method reify($n is copy = 10) { 
        if !$!reified.defined {
            my Mu $rpa := pir::new__Ps('ResizablePMCArray');
            $n = $Inf if Whatever.ACCEPTS($n);
            fail "Infinite memory not available for Range generation" 
                if $n == $Inf && self.infinite;
            ( pir::push__vPP($rpa, $!value++); $n-- ) while $n > 0 && $!value <= $!max;
            pir::push__vPP($rpa, self.new(:value($!value), :max($!max)) )
              if $!value <= $!max;
            pir::setattribute__vPPsP(self, RangeIter, '$!reified',
                pir__perl6_box_rpa__PP($rpa));
        }
        $!reified
    }

    method infinite() { $!max == $Inf }

    multi method DUMP(RangeIter:D:) {
        self.DUMP-ID() ~ '('
          ~ ':reified(' ~ DUMP($!reified) ~ '), '
          ~ ':value(' ~ DUMP($!value) ~ '), '
          ~ ':max(' ~ DUMP($!max) ~ ') '
          ~ ')'
    }
}

