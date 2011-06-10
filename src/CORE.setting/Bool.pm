my class Bool {
    method Bool() { self }

    method Str(Bool:D:) {
        self ?? 'Bool::True' !! 'Bool::False'
    }

    method Numeric() { self ?? 1 !! 0 }
}
