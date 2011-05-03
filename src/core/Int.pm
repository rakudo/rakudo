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
    method Bridge() {
        self.Num;
    }

    our Int method Int() { self; }

    our Int method Rat(Real $epsilon = 1.0e-6) { self / 1; }

    our Num method Num() {
        pir::box__PN(pir::set__NP(self));
    }

    method sign(Int $x:) {
        $x.defined ?? $x <=> 0 !! Mu;
    }

    method gcd(Int $x: Int $y) {
        pir::gcd__iii($x, $y);
    }
}

multi sub infix:<cmp>(Int $a, Int $b) {
    pir::cmp__III($a, $b);
}

multi sub infix:«<=>»(Int $a, Int $b) {
    pir::cmp__III($a, $b);
}

multi sub infix:«==»(Int $a, Int $b) {
    pir::iseq__III( $a, $b) ?? True !! False
}

multi sub infix:«!=»(Int $a, Int $b) {
    pir::isne__III( $a, $b) ?? True !! False
}

multi sub infix:«<»(Int $a, Int $b) {
    pir::islt__III( $a, $b) ?? True !! False
}

multi sub infix:«<=»(Int $a, Int $b) {
    pir::isle__III( $a, $b) ?? True !! False
}

multi sub infix:«>»(Int $a, Int $b) {
    pir::isgt__III( $a, $b) ?? True !! False
}

multi sub infix:«>=»(Int $a, Int $b) {
    pir::isge__III( $a, $b) ?? True !! False
}

# Should pull along the other Int comparison operators at some point,
# but this is a great start.

our multi sub prefix:<->(Int $a) {
    upgrade_to_num_if_needed(pir::neg__NN($a))
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
    upgrade_to_num_if_needed(pir::fdiv__NNN($a, $b));
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

proto sub gcd($x, $y) { $x.gcd($y); }
