augment class Enum {

=begin item ACCEPTS()

Called from smartmatches '$_ ~~ X'.

For C<$_ ~~ Mapping> tests if C<$_{X.key} ~~ X.value>

Else it delegates to a  method call '.:Xkey(Xval)'
(TODO: should actually be .Xkey, not .:Xkey).

=end item

    multi method ACCEPTS(EnumMap $topic) {
	    $topic{$.key} ~~ $.value;
    }

    multi method ACCEPTS($topic) {
        my $meth_name = $.key;
        return (?$topic."$meth_name"()) === (?$.value);
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
        ($.key, $.value);
    }

=begin item pairs

=end item
    method pairs() {
        return self.list();
    }
    
    method value() {
        $!value
    }

    multi method perl() {
        # $.key.perl ~ ' => ' ~ $.value.perl;
        "Pair.new(:key({$.key.perl}), :value({$.value.perl}))";
    }

    # I don't see anything in the spec about what
    # this should do.  This is doing the same thing
    # that ~Pair did in old Rakudo master, which didn't
    # seem to have Pair.Str.
    multi method Str() {
        "$.key\t$.value";
    }

    multi method hash() {
        {self};
    }
}
