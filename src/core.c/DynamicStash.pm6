my class DynamicStash { # Declared in BOOTSTRAP
    # class DynamicStash is Stash
    method ASSIGN-KEY(|) is raw {
        my $cont := callsame;
        nqp::stmts(
            (my $desc := nqp::getattr(nqp::decont($cont.VAR), Scalar, '$!descriptor')),
            nqp::if(
                nqp::isconcrete($desc),
                nqp::bindattr_i(
                    $desc,
                    ContainerDescriptor,
                    '$!dynamic',
                    1
                )
            )
        );
        $cont
    }
}
