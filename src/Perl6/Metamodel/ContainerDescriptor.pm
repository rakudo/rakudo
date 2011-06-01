class Perl6::Metamodel::ContainerDescriptor {
    has     $!of;
    has int $!rw;
    has str $!name;
    
    method of() { $!of }
    method rw() { $!rw }
    method name() { $!name }
    
    method set_of($of) { $!of := $of }
    method set_rw($rw) { $!rw := $rw }
}
