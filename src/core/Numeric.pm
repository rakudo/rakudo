role Numeric {
    INIT {
        our @trig-base-conversions = (1.0, pi / 180.0, pi / 200.0, 2.0 * pi);
    }

    # Used by the :Trig subs and methods in the Int and Num classes.
    method to-radians($base) {
        self * pir::get_global__Ps('@trig-base-conversions')[$base];
    }

    method from-radians($base) {
        self / pir::get_global__Ps('@trig-base-conversions')[$base];
    }

    method log10(Numeric $x:) {
        self.log(10);
    }
}
