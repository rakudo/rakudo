role Real does Numeric {
    method abs() {
        self < 0 ?? -self !! self;
    }
}
