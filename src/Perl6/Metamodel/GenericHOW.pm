# A HOW that represents a generic type. It's something of a
# placeholder for a type that we don't actually know yet.
# It sits anywhere that a type could, and possession of one
# of these confers genericity on the holder.
class Perl6::Metamodel::GenericHOW
    does Perl6::Metamodel::Naming
{
    my $archetypes := Perl6::Metamodel::Archetypes.new( :generic(1) );
    method archetypes() {
        $archetypes
    }
    
    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }
    
    # The name we're created with is both the name we'll claim
    # to be if asked, but also the name we'll look up in a
    # supplied type environment when we want to instantiate
    # ourself.
    method new_type(:$name) {
        my $meta := self.new();
        my $obj := pir::repr_type_object_for__PPS($meta, 'Uninstantiable');
        $meta.set_name($obj, $name);
        $obj
    }

    method instantiate_generic($obj, $type_environment) {
        my $name := self.name($obj);
        nqp::existskey($type_environment, $name) ?? $type_environment{$name} !! $obj
    }
    
    method compose($obj) {
    }
    
    method find_method($obj, $name) {
        nqp::die("Cannot call a method on type variable " ~ self.name($obj))
    }
    
    method type_check($obj, $checkee) {
        nqp::die("Cannot type check against type variable " ~ self.name($obj))
    }
}
