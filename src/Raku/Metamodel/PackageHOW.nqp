#- Metamodel::PackageHOW -------------------------------------------------------
class Perl6::Metamodel::PackageHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::MethodDelegation
{

    my $archetypes := Perl6::Metamodel::Archetypes.new;
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
