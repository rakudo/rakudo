class Perl6::Metamodel::ContainerDescriptor {
    has     $!of;
    has str $!name;
    has     $!default;
    has int $!dynamic;

    method BUILD(:$of, str :$name, :$default, int :$dynamic) {
        $!of := $of;
        $!name := $name;
        $!default := $default;
        $!dynamic := $dynamic;
    }

    method of() { $!of }
    method name() { $!name }
    method default() { $!default }
    method dynamic() { $!dynamic }

    method set_of($of) { $!of := $of; self }
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

role Perl6::Metamodel::ContainerDescriptor::Whence {
    has $!next-descriptor;

    method next() {
        my $next := $!next-descriptor;
        nqp::isconcrete($next)
            ?? $next
            !! ($!next-descriptor := nqp::gethllsym('perl6', 'default_cont_spec'))
    }
    method of() { self.next.of }
    method default() { self.next.default }
    method dynamic() { self.next.dynamic }
}

class Perl6::Metamodel::ContainerDescriptor::BindArrayPos
        does Perl6::Metamodel::ContainerDescriptor::Whence {
    has $!target;
    has int $!pos;

    method new($desc, $target, int $pos) {
        my $self := nqp::create(self);
        nqp::bindattr($self, Perl6::Metamodel::ContainerDescriptor::BindArrayPos,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, Perl6::Metamodel::ContainerDescriptor::BindArrayPos,
            '$!target', $target);
        nqp::bindattr_i($self, Perl6::Metamodel::ContainerDescriptor::BindArrayPos,
            '$!pos', $pos);
        $self
    }

    method assigned($scalar) {
        nqp::bindpos($!target, $!pos, $scalar);
    }
}
