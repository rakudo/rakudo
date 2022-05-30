# A HOW that represents a generic type. It's something of a
# placeholder for a type that we don't actually know yet.
# It sits anywhere that a type could, and possession of one
# of these confers genericity on the holder.
class Perl6::Metamodel::GenericHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::MethodDelegation
{
    my $archetypes := Perl6::Metamodel::Archetypes.new( :generic(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), %named)
    }

    # The name we're created with is both the name we'll claim
    # to be if asked, but also the name we'll look up in a
    # supplied type environment when we want to instantiate
    # ourself.
    method new_type(:$name) {
        my $meta := self.new();
        my $obj := nqp::settypehll(nqp::newtype($meta, 'Uninstantiable'), 'Raku');
        $meta.set_name($obj, $name);
        nqp::settypecache($obj, nqp::list(nqp::hllize(nqp::null()))); # magick a Mu
        nqp::settypecheckmode($obj,
            nqp::const::TYPE_CHECK_CACHE_DEFINITIVE)
    }

    method instantiate_generic($obj, $type_environment) {
        my $name := self.name($obj);
        my $found := nqp::getlexrel($type_environment, $name);
        nqp::isnull($found) ?? $obj !! $found
    }

    method compose($obj) {
    }

    method type_check($obj, $checkee) {
        0
    }

    method accepts_type($obj, $checkee) {
        1
    }
}

# vim: expandtab sw=4
