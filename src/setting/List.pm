class List is also {

=begin item list

A List in list context returns itself.

=end item
    method list() {
        return self;
    }

=begin item perl()

Returns a Perl representation of a List.

=end item
    method perl() {
        return '[' ~ self.map({ .perl }).join(", ") ~ ']';
    }

    method reverse() {
        my @result;
        for self.iterator() {
            @result.unshift($_);
        }
        return @result;
    }

}

# vim: ft=perl6
