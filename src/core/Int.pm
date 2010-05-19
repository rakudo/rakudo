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

augment class Int does Real {
    multi method ACCEPTS(Int $other) {
        self == $other;
    }

    multi method ACCEPTS($other) {
        self.Num.ACCEPTS($other);
    }

    method Bridge() {
        self.Num;
    }

    our Bool multi method Bool() { self != 0 ?? Bool::True !! Bool::False }

    our Int method Int() { self; }

    our Int method Rat(Real $epsilon = 1.0e-6) { self / 1; }

    our Num method Num() {
        pir::box__PN(pir::set__NP(self));
    }

    # Next has been moved to Rat.pm for the moment.
    # our Rat multi method Rat() { Rat.new(self, 1); }
}

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
    if $b >= 0 {
        upgrade_to_num_if_needed(pir::pow__NNN($a, $b))
    } else {
        pir::pow__NNN($a, $b)
    }
}

our multi sub prefix:<->(Int $a) {
    upgrade_to_num_if_needed(pir::neg__NN($a))
}
