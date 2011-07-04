my class Whatever {
    multi method ACCEPTS(Whatever:D: $topic) { True }
    method new() { nqp::create(self) }
}
