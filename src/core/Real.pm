role Real does Numeric {
    method Bridge() {
        fail "Bridge must be defined for the Real type " ~ self.WHAT;
    }

    method abs() {
        self < 0 ?? -self !! self;
    }

    method sign {
        self.notdef ?? Mu
                    !! (self ~~ NaN ?? NaN !! self <=> 0);
    }
}
