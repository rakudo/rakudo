#- Metamodel::GenericHOW -------------------------------------------------------
# A HOW that represents a generic type. It's something of a placeholder for
# a type that we don't actually know yet.  It sits anywhere that a type could,
# and possession of one of these confers genericity on the holder.
class Perl6::Metamodel::GenericHOW
    does Perl6::Metamodel::Naming
{
    my $archetypes := Perl6::Metamodel::Archetypes.new(:generic);
    method archetypes($XXX?) { $archetypes }

    # The name we're created with is both the name we'll claim
    # to be if asked, but also the name we'll look up in a
    # supplied type environment when we want to instantiate
    # ourself.
    method new_type(:$name) {
        my $HOW  := nqp::create(self);
        my $type := nqp::newtype($HOW, 'Uninstantiable');
        nqp::settypehll($type, 'Raku');

        $HOW.set_name($type, $name);
        $type
    }

    method instantiate_generic($target, $type_environment) {
        my str $name  := self.name($target);
        my str $kind  := $type_environment.HOW.name($type_environment);

#?if !jvm
        my $found := $kind eq 'BOOTContext'
#?endif
#?if jvm
        my $found := $kind eq 'ContextRef'
#?endif
          ?? nqp::getlexrel($type_environment, $name)
          !! $kind eq 'BOOTHash'
            ?? nqp::atkey($type_environment, $name)
            !! nqp::isconcrete($type_environment)
                 && $type_environment.EXISTS-KEY($name)
              ?? nqp::decont($type_environment.AT-KEY($name))
              !! nqp::null;

        nqp::isnull($found)
          ?? $target
          !! $found.HOW.archetypes($found).generic
            ?? $found.HOW.instantiate_generic($found, $type_environment)
            !! $found
    }

    # General methods that are expected to work
    method compose($target)              { $target   }
    method find_method($XXX, $name, *%_) { nqp::null }
    method type_check($XXX, $checkee)    { 0         }
}

# vim: expandtab sw=4
