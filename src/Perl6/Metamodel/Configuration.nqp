# Keeps track of various special types or other things that the MOP may be
# configured with.
class Perl6::Metamodel::Configuration {
    my $stash_type := nqp::null();
    my $stash_attr_type := nqp::null();
    method set_stash_type($type, $attr_type) {
        $stash_type := $type;
        $stash_attr_type := $attr_type;
    }
    method stash_type() { $stash_type }
    method stash_attr_type() { $stash_attr_type }

    my $submethod_type := nqp::null();
    method set_submethod_type($type) {
        $submethod_type := $type;
    }
    method submethod_type() { $submethod_type }
}
