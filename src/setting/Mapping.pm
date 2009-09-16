class Mapping is also {
=begin item perl()

Returns a Perl representation of a Mapping.

=end item
    method perl() {
        return '{' ~ self.pairs.map({ .perl }).join(", ") ~ '}';
    }
}

