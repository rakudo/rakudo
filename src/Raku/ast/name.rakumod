# A name. Names range from simple (a single identifier) up to rather more
# complex (including pseudo-packages, interpolated parts, etc.)
class RakuAST::Name is RakuAST::Node {
    has List $!parts;

    method new(*@parts) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Name, '$!parts', @parts);
        $obj
    }

    method from-identifier(Str $identifier) {
        self.new(RakuAST::Name::Part::Simple.new($identifier))
    }

    method parts() {
        self.IMPL-WRAP-LIST($!parts)
    }

    method is-identifier() {
        nqp::elems($!parts) == 1 && nqp::istype($!parts[0], RakuAST::Name::Part::Simple)
    }
}

# Marker role for a part of a name.
class RakuAST::Name::Part {
}

# A simple name part, wrapping a string name.
class RakuAST::Name::Part::Simple is RakuAST::Name::Part {
    has str $.name;

    method new(Str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Name::Part::Simple, '$!name', $name);
        $obj
    }
}
