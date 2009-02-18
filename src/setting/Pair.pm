class Pair is also {

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
