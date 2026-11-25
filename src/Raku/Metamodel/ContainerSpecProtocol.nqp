#- Metamodel::ContainerSpecProtocol --------------------------------------------
role Perl6::Metamodel::ContainerSpecProtocol {
    has $!code_pair;

    method get_container_spec($XXX?) { $!code_pair }

    method set_container_spec($XXX, $code_pair) {
        $!code_pair := $code_pair;
    }

    method publish_container_spec($target) {
        my @mro := self.mro($target);

        my int $m := nqp::elems(@mro);
        my int $i;
        while $i < $m {
            my $type := nqp::atpos(@mro, $i);
            nqp::can($type.HOW, 'get_container_spec')
              && (my $code_pair := $type.HOW.get_container_spec($type))
              ?? (return nqp::setcontspec($target, 'code_pair', $code_pair))
              !! ++$i;
        }
    }
}

# vim: expandtab sw=4
