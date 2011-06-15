# XXX role Numeric { ... }
my class Numeric {
    method Numeric() { self }

    multi method ACCEPTS(Numeric:D: $a) { $a == self }
}
