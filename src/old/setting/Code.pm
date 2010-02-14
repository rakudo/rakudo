class Code is also {

=begin item ACCEPTS

=end item
    method ACCEPTS(Object $topic) {
        self.count == 0 ?? self() !! self($topic)
    }

}

# vim: ft=perl6
