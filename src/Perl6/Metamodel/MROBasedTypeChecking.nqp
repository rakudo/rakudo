role Perl6::Metamodel::MROBasedTypeChecking {
    method isa($obj, $type) {
        my $decont := nqp::decont($type);
        for self.mro($obj) {
            if nqp::decont($_) =:= $decont { return 1 }
        }
        0
    }

    method does($obj, $type) {
        nqp::hllboolfor(nqp::istype($obj, $type), "Raku")
    }

    method type_check($obj, $checkee) {
        # The only time we end up in here is if the type check cache was
        # not yet published, which means the class isn't yet fully composed.
        # Just hunt through MRO.
        for self.mro($obj) {
            if $_ =:= $checkee {
                return 1;
            }
            if nqp::can($_.HOW, 'role_typecheck_list') {
                for $_.HOW.role_typecheck_list($_) {
                    if $_ =:= $checkee {
                        return 1;
                    }
                }
            }
        }
        0
    }

    method publish_type_cache($obj) {
        my @tc;
        for self.mro($obj) {
            @tc.push($_);
            if nqp::can($_.HOW, 'role_typecheck_list') {
                for $_.HOW.role_typecheck_list($_) {
                    @tc.push($_);
                }
            }
        }
        nqp::settypecache($obj, @tc)
    }
}

# vim: expandtab sw=4
