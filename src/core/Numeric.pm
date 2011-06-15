# XXX role Numeric { ... }
my class Numeric {
    method Numeric() { self }

    proto method ACCEPTS(|$) { * }
    multi method ACCEPTS(Numeric:D: $a) { $a == self }
}
