class Perl6::Metamodel::PackageHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::MethodDelegation
{

    my $archetypes := Perl6::Metamodel::Archetypes.new();
    method archetypes($obj?) {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), %named)
    }

    method new_type(:$name = '<anon>', :$repr, :$ver, :$auth) {
        if $repr { nqp::die("'package' does not support custom representations") }
        my $metaclass := nqp::create(self);
        my $obj := nqp::settypehll(nqp::newtype($metaclass, 'Uninstantiable'), 'Raku');
        $metaclass.set_name($obj, $name);
        self.add_stash($obj);
    }
}

# vim: expandtab sw=4
