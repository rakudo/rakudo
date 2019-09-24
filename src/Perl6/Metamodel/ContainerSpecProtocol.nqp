role Perl6::Metamodel::ContainerSpecProtocol {
    has $!code_pair;

    method get_container_spec($obj) {
        $!code_pair
    }

    method set_container_spec($obj, $code_pair) {
        $!code_pair := $code_pair;
    }

    method publish_container_spec($obj) {
        # If we have a container specification here, install that
        if $!code_pair {
            nqp::setcontspec($obj, 'code_pair', $!code_pair);
            $obj.HOW.compose_repr($obj);
        }

        # look in the parents for any if none here
        else {
            for self.mro($obj) -> $class {
                if nqp::can($class.HOW, 'get_container_spec') {
                    my $code_pair := $class.HOW.get_container_spec($class);
                    if $code_pair {
                        $!code_pair := $code_pair;
                        nqp::setcontspec($obj, 'code_pair', $!code_pair);
                        $obj.HOW.compose_repr($obj);
                        last;
                    }
                }
            }
        }
    }
}
