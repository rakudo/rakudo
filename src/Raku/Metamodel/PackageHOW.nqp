#- Metamodel::PackageHOW -------------------------------------------------------
class Metamodel::PackageHOW
    does Metamodel::Naming
    does Metamodel::Documenting
    does Metamodel::Composing
    does Metamodel::Stashing
    does Metamodel::TypePretense
    does Metamodel::MethodDelegation
{

    my $archetypes := Metamodel::Archetypes.new;
    method archetypes($XXX?) { $archetypes }

    method new_type(:$name = '<anon>', :$repr, *%_) {
        nqp::die("'package' does not support custom representations")
          if $repr;

        my $HOW    := nqp::create(self);
        my $target := nqp::settypehll(
          nqp::newtype($HOW, 'Uninstantiable'), 'Raku'
        );

        $HOW.set_name($target, $name);
        $HOW.add_stash($target)
    }
}

# vim: expandtab sw=4
