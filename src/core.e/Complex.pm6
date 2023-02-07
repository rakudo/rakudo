augment class Complex {
    method sign(Complex:D: --> Complex:D) {
        my $abs = self.abs;
        $abs == 0 ?? 0i !! self / $abs
    }
}

# vim: expandtab shiftwidth=4
