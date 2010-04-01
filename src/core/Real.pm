role Real does Numeric {
    method abs() {
        self < 0 ?? -self !! self;
    }

    method sign {
        self.notdef ?? Mu
                    !! (self ~~ NaN ?? NaN !! self <=> 0);
    }
}
