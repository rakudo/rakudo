# XXX Should be my...
our sub upgrade_to_num_if_needed($test) {
    Q:PIR {
        .local num test
        $P0 = find_lex '$test'
        test = $P0
        if test > 2147483647.0 goto upgrade
        if test < -2147483648.0 goto upgrade
        $I0 = test
        .return ($I0)
      upgrade:
        .return (test)
    }
}

augment class Int {
    multi method abs() {
        pir::box__PI(pir::abs__II(self))
    }

    our Int multi method Int() { self }

    our Num multi method Num() {
        pir::box__PN(pir::set__NP(self))
    }
 
    # Next has been moved to Rat.pm for the moment.
   # our Rat multi method Rat() { Rat.new(self, 1); }

#    our Complex multi method Complex() { Complex.new(self, 0); }

    our Str multi method Str() {
        ~self;
    }

    # Most of the trig functions for Int are in Any-num.pm, but
    # sec is a special case.
    our Num multi method sec($base = 'radians') {
        self.Num.sec($base);
    }

#    our Complex multi method unpolar($angle) is export {
#        Complex.new(self.Num * $angle.cos("radians"), self.Num * $angle.sin("radians"));
#    }
}

our multi sub abs(Int $x) { $x.abs }

our multi sub infix:<+>(Int $a, Int $b) {
    upgrade_to_num_if_needed(pir::add__NNN($a, $b))
}

our multi sub infix:<->(Int $a, Int $b) {
    upgrade_to_num_if_needed(pir::sub__NNN($a, $b))
}

our multi sub infix:<*>(Int $a, Int $b) {
    upgrade_to_num_if_needed(pir::mul__NNN($a, $b))
}

our multi sub infix:<div>(Int $a, Int $b) {
    pir::box__PI(pir::div__III($a, $b))
}

our multi sub infix:<%>(Int $a, Int $b) {
    upgrade_to_num_if_needed(pir::mod__NNN($a, $b))
}

our multi sub infix:<**>(Int $a, Int $b) {
    upgrade_to_num_if_needed(pir::pow__NNN($a, $b))
}

our multi sub prefix:<->(Int $a) {
    upgrade_to_num_if_needed(pir::neg__NN($a))
}
