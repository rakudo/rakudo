class Perl6::Metamodel::ContainerDescriptor {
    has     $!of;
    has int $!rw;
    has str $!name;
    
    method BUILD(:$of, :$rw, :$name) {
        $!of := $of;
        $!rw := $rw;
        $!name := $name;
    }
    
    method of() { $!of }
    method rw() { $!rw }
    method name() { $!name }
    
    method set_of($of) { $!of := $of }
    method set_rw($rw) { $!rw := $rw }
    
    method is_generic() {
        $!of.HOW.archetypes.generic
    }
    
    method instantiate_generic($type_environment) {
        my $ins_of := $!of.HOW.instantiate_generic($!of, $type_environment);
        my $ins := nqp::clone(self);
        pir::setattribute__0PPsP($ins, $?CLASS, '$!of', $ins_of)
    }
}
