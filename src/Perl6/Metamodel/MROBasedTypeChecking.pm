role Perl6::Metamodel::MROBasedTypeChecking {
    method isa($obj, $type) {
        my $decont := pir::nqp_decontainerize__PP($type);
        for self.mro($obj) {
            if pir::nqp_decontainerize__PP($_) =:= $decont { return 1 }
        }
        0
    }
    
    method does($obj, $type) {
        pir::perl6_booleanize__Pi(nqp::istype($obj, $type))
    }
    
    method type_check($obj, $checkee) {
        # The only time we end up in here is if the type check cache was
        # not yet published, which means the class isn't yet fully composed.
        # Just hunt through MRO.
        for self.mro($obj) {
            if $_ =:= $checkee {
                return 1;
            }
            if pir::can($_.HOW, 'role_typecheck_list') {
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
            if pir::can($_.HOW, 'role_typecheck_list') {
                for $_.HOW.role_typecheck_list($_) {
                    @tc.push($_);
                }
            }
        }
        pir::publish_type_check_cache($obj, @tc)
    }
}
