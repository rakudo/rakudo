role Perl6::Metamodel::ContainerSpecProtocol {
    has $!code_pair;

    method get_container_spec($obj) {
        $!code_pair
    }

    method set_container_spec($obj, $code_pair) {
        $!code_pair := $code_pair;
    }

    method publish_container_spec($obj) {
        for self.mro($obj) -> $class {
            if nqp::can($class.HOW, 'get_container_spec') {
                my $code_pair := $class.HOW.get_container_spec($class);
                if $code_pair {
                    nqp::setcontspec($obj, 'code_pair', $code_pair);
                    last;
                }
            }
        }
    }
}

# vim: expandtab sw=4
