#- Metamodel::ModuleHOW --------------------------------------------------------
class Metamodel::ModuleHOW
    does Metamodel::Naming
    does Metamodel::Documenting
    does Metamodel::Composing
    does Metamodel::Versioning
    does Metamodel::Stashing
    does Metamodel::TypePretense
    does Metamodel::MethodDelegation
{

    my $archetypes := Metamodel::Archetypes.new;
    method archetypes($XXX?) { $archetypes }

    method new_type(:$repr, *%_) {
        nqp::die("'module' does not support custom representations")
          if $repr;

        my $HOW    := nqp::create(self);
        my $target := nqp::settypehll(
          nqp::newtype($HOW, 'Uninstantiable'), 'Raku'
        );
        $HOW.set_identity($target, %_);
        $HOW.add_stash($target);
    }
}

# vim: expandtab sw=4
