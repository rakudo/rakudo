class List is also {

=begin item fmt

 our Str multi List::fmt ( Str $format = '%s', $separator = ' ' )

Returns the invocant list formatted by an implicit call to C<sprintf> on each
of the elements, then joined with spaces or an explicitly given separator.

=end item
    multi method fmt(Str $format = '%s', $sep = ' ') is export {
        return join($sep, self.map({ sprintf($format, $^elem) }));
    }

=begin item iterator()

Returns an iterator for the list.

=end item
    method iterator() {
        return Q:PIR {
            self.'!flatten'()
            %r = iter self
        };
    }

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

    method rotate(Int $n = 1) is export {
        my Int $k = $n % $.elems;
        self.[$k .. $.elems-1, 0 .. $k-1]
    }

    method Capture(@self:) {
        Capture.new(|@self)
    }
}

# vim: ft=perl6
