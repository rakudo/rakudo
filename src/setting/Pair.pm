class Pair is also {

=begin item ACCEPTS()

Called from smartmatches '$_ ~~ X'.

For C<$_ ~~ Mapping> tests if C<$_{X.key} ~~ X.value>

Else it delegates to a  method call '.:Xkey(Xval)'
(TODO: should actually be .Xkey, not .:Xkey).

=end item

    multi method ACCEPTS(Mapping $topic) {
	$topic{$.key} ~~ $.value;
    }

    multi method ACCEPTS($topic) {
        my $meth_name = ':' ~ $.key;
        return $topic."$meth_name"($.value);
    }

=begin item fmt

  our Str multi Pair::fmt ( Str $format = "%s\t%s" )

Returns the invocant pair formatted by an implicit call to C<sprintf> on
the key and value.

=end item
    method fmt(Str $format = "%s\t%s") {
        return sprintf($format, $.key, $.value);
    }

=begin item kv

Return key and value as a 2-element List.

=end item
    method kv() {
        return list($.key, $.value);
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
        return $.key.perl ~ ' => ' ~ $.value.perl;
    }

}

# vim: ft=perl6
