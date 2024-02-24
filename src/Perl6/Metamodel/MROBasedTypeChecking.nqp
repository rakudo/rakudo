#- Metamodel::MROBasedTypeChecking ---------------------------------------------
role Perl6::Metamodel::MROBasedTypeChecking {
    method isa($target, $type) {
        $type   := nqp::decont($type);
        my $mro := self.mro($target);

        my int $m := nqp::elems($mro);
        my int $i;
        while $i < $m {
            nqp::eqaddr(nqp::decont(nqp::atpos($mro, $i)), $type)
              ?? (return 1)
              !! ++$i;
        }

        0
    }

    method does($target, $type) {
        # XXX why does this need to be HLLized here??
        nqp::hllboolfor(nqp::istype($target, $type), "Raku")
    }

    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);

        # The only time we end up in here is if the type check cache was
        # not yet published, which means the class isn't yet fully composed.
        # Just hunt through MRO.
        my $mro  := self.mro($target);

        my int $m := nqp::elems($mro);
        my int $i;
        while $i < $m {
            my $type := nqp::atpos($mro, $i);
            return 1 if nqp::eqaddr($type, $checkee);

            if nqp::can($type.HOW, 'role_typecheck_list') {
                my $types := $type.HOW.role_typecheck_list($type);

                my int $n := nqp::elems($types);
                my int $j;
                while $j < $n {
                    nqp::eqaddr(nqp::atpos($types, $j), $checkee)
                      ?? (return 1)
                      !! ++$j;
                }
            }

            ++$i;
        }

        0
    }

    method publish_type_cache($target) {
        my @tc;
        my $mro := self.mro($target);

        my int $m := nqp::elems($mro);
        my int $i;
        while $i < $m {
            my $type := nqp::atpos($mro, $i);
            nqp::push(@tc, $type);

            nqp::splice(
              @tc,
              $type.HOW.role_typecheck_list($type),
              nqp::elems(@tc),
              0
            ) if nqp::can($type.HOW, 'role_typecheck_list');

            ++$i;
        }
        nqp::settypecache($target, @tc)
    }
}

# vim: expandtab sw=4
