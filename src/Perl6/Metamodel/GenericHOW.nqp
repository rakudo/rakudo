# A HOW that represents a generic type. It's something of a
# placeholder for a type that we don't actually know yet.
# It sits anywhere that a type could, and possession of one
# of these confers genericity on the holder.
class Perl6::Metamodel::GenericHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
{
    my $archetypes := Perl6::Metamodel::Archetypes.new( :generic(1) );
    method archetypes($XXX?) { $archetypes }

    # The name we're created with is both the name we'll claim
    # to be if asked, but also the name we'll look up in a
    # supplied type environment when we want to instantiate
    # ourself.
    method new_type(:$name) {
        my $meta := self.new();
        my $obj := nqp::settypehll(nqp::newtype($meta, 'Uninstantiable'), 'Raku');
        $meta.set_name($obj, $name);
        $obj
    }

    method instantiate_generic($target, $type_environment) {
        my $found := nqp::null();
        my $name := self.name($target);
        my $te-kind := $type_environment.HOW.name($type_environment);
#?if !jvm
        if $te-kind eq 'BOOTContext' {
#?endif
#?if jvm
        if $te-kind eq 'ContextRef' {
#?endif
            $found := nqp::getlexrel($type_environment, $name);
        }
        elsif $te-kind eq 'BOOTHash' {
            $found := nqp::atkey($type_environment, $name);
        }
        elsif nqp::isconcrete($type_environment) && $type_environment.EXISTS-KEY($name) {
            $found := nqp::decont($type_environment.AT-KEY($name));
        }
        nqp::isnull($found)
            ?? $target
            !! $found.HOW.archetypes($found).generic
                ?? $found.HOW.instantiate_generic($found, $type_environment)
                !! $found
    }

    method compose($target) { $target }

    method find_method($XXX, $name, *%c) {
        nqp::null()
    }

    method type_check($XXX, $checkee) {
        0
    }
}

# vim: expandtab sw=4
