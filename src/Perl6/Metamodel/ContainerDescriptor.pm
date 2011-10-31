class Perl6::Metamodel::ContainerDescriptor {
    has     $!of;
    has int $!rw;
    has str $!name;
    
    method of() { $!of }
    method rw() { $!rw }
    method name() { $!name }
    
    method set_of($of) { $!of := $of }
    method set_rw($rw) { $!rw := $rw }
    
    method new(:$of, :$rw, :$name) {
        my $cd := nqp::create(self);
        $cd.BUILD($of, $rw, $name);
    }
    
    method BUILD($of, $rw, $name) {
        $!of := $of;
        $!rw := $rw;
        $!name := $name;
        self
    }
    
    method is_generic() {
        $!of.HOW.archetypes.generic
    }
    
    method instantiate_generic($type_environment) {
        my $ins_of := $!of.HOW.instantiate_generic($!of, $type_environment);
        my $ins := pir::repr_clone__PP(self);
        pir::setattribute__0PPsP($ins, $?CLASS, '$!of', $ins_of)
    }
}
