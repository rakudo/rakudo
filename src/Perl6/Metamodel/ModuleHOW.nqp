class Perl6::Metamodel::ModuleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::MethodDelegation
{
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new( );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type(:$name = '<anon>', :$repr, :$ver, :$auth, :$api) {
        if $repr { nqp::die("'module' does not support custom representations") }
        my $metaclass := self.new();
        my $obj := nqp::settypehll(nqp::newtype($metaclass, 'Uninstantiable'), 'Raku');
        $metaclass.set_name($obj, $name);
        $metaclass.set_ver($obj, $ver);
        $metaclass.set_auth($obj, $auth) if $auth;
        $metaclass.set_api($obj, $api) if $api;
        self.add_stash($obj);
    }

    method compose($obj, :$compiler_services) {
        $!composed := 1;
    }

    method is_composed($obj) {
        $!composed
    }
}

# vim: expandtab sw=4
