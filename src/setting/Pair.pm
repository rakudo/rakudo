class Pair is also {

=begin item ACCEPTS()

Called from smartmatches '$_ ~~ X'.
Delegates on to a method call '.:Xkey(Xval)'.

=end item
    method ACCEPTS($topic) {
        my $meth_name = ':' ~ self.key;
        return $topic."$meth_name"(self.value);
    }

=begin item fmt

  our Str multi Pair::fmt ( Str $format )

Returns the invocant pair formatted by an implicit call to C<sprintf> on
the key and value.

=end item
    method fmt(Str $format) {
        return sprintf($format, self.key, self.value);
    }

=begin item key

Gets the key of the pair.

=end item
    method key() {
        return $!key;
    }

=begin item kv

Return key and value as a 2-element List.

=end item
    method kv() {
        return list(self.key, self.value);
    }

=begin item pairs

=end item
    method pairs() {
        return self.list();
    }

=begin item value

Gets the value of the pair.

=end item
    method value() {
        return $!value;
    }

=begin item perl

Returns a Perl code representation of the pair.

=end item
    method perl() {
        return self.key.perl ~ ' => ' ~ self.value.perl;
    }

}
