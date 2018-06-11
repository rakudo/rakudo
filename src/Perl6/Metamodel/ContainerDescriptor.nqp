class Perl6::Metamodel::ContainerDescriptor {
    has     $!of;
    has int $!rw;
    has str $!name;
    has     $!default;
    has int $!dynamic;

    method BUILD(:$of, int :$rw, str :$name, :$default, int :$dynamic) {
        $!of := $of;
        $!rw := $rw;
        $!name := $name;
        $!default := $default;
        $!dynamic := $dynamic;
    }

    method of() { $!of }
    method rw() { $!rw }
    method name() { $!name }
    method default() { $!default }
    method dynamic() { $!dynamic }

    method set_of($of) { $!of := $of; self }
    method set_rw($rw) { $!rw := $rw; self }
    method set_default($default) { $!default := $default; self }
    method set_dynamic($dynamic) { $!dynamic := $dynamic; self }

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
