my class Iterator { # declared in BOOTSTRAP
    # class Iterator is Iterable {

    method iterator() { nqp::decont(self) }
}

# vim: ft=perl6 expandtab sw=4
