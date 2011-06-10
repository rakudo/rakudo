my class Bool {
    method Bool() { self }

    multi method Str(Bool:D:) {
        self ?? 'Bool::True' !! 'Bool::False'
    }

    method Numeric() { self ?? 1 !! 0 }

    method pred() { 0.Bool }

    method succ() { 1.Bool }

}
