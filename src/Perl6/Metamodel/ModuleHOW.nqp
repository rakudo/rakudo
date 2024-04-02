#- Metamodel::ModuleHOW --------------------------------------------------------
class Perl6::Metamodel::ModuleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::Versioning
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::MethodDelegation
{

    my $archetypes := Perl6::Metamodel::Archetypes.new;
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
