class Perl6::Metamodel::ContainerDescriptor {
    has     $!of;
    has int $!rw;
    has str $!name;
    has     $!default;
    has int $!dynamic;
    
    method BUILD(:$of, :$rw, :$name, :$default) {
        $!of := $of;
        $!rw := $rw;
        $!name := $name;
        $!default := $default;
    }
    
    method of() { $!of }
    method rw() { $!rw }
    method name() { $!name }
    method default() { $!default }
    method dynamic() { $!dynamic }
    
    method set_of($of) { $!of := $of }
    method set_rw($rw) { $!rw := $rw }
    method set_default($default) { $!default := $default }
    method set_dynamic($dynamic) { $!dynamic := $dynamic }
    
    method is_generic() {
        $!of.HOW.archetypes.generic
    }
    
    method instantiate_generic($type_environment) {
        my $ins_of := $!of.HOW.instantiate_generic($!of, $type_environment);
        my $ins := nqp::clone(self);
        nqp::bindattr($ins, $?CLASS, '$!of', $ins_of);
        $ins
    }
}
