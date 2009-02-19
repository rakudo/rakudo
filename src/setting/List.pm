class List is also {

=begin item perl()

Returns a Perl representation of a List.

=end item
    method perl() {
        return '[' ~ self.map({ .perl }).join(", ") ~ ']';
    }

}
