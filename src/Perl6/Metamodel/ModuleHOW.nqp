#- Metamodel::ModuleHOW --------------------------------------------------------
class Perl6::Metamodel::ModuleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::MethodDelegation
{

    my $archetypes := Perl6::Metamodel::Archetypes.new;
    method archetypes($XXX?) { $archetypes }

    method new_type(:$name = '<anon>', :$repr, :$ver, :$auth, :$api) {
        nqp::die("'module' does not support custom representations")
          if $repr;

        my $HOW := nqp::create(self);
        my $target := nqp::settypehll(
          nqp::newtype($HOW, 'Uninstantiable'), 'Raku'
        );
        $HOW.set_name($target, $name);
        $HOW.set_ver( $target, $ver );
        $HOW.set_auth($target, $auth) if $auth;
        $HOW.set_api( $target, $api ) if $api;

        self.add_stash($target);
    }
}

# vim: expandtab sw=4
