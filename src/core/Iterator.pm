my class Iterator { # declared in BOOTSTRAP
    # class Iterator is Iterable {

    method iterator() { nqp::decont(self) }
}
