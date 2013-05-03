# Implements managing trust relationships between types.
role Perl6::Metamodel::Trusting {
    # Who do we trust?
    has @!trustees;
    
    # Adds a type that we trust.
    method add_trustee($obj, $trustee) {
        @!trustees[+@!trustees] := $trustee;
    }
    
    # Introspect the types that we trust.
    method trusts($obj) {
        @!trustees
    }
    
    # Checks if we trust a certain type. Can be used by the compiler
    # to check if a private call is allowable.
    method is_trusted($obj, $claimant) {
        # Always trust ourself.
        if $claimant.WHAT =:= $obj.WHAT {
            return 1;
        }
        
        # Otherwise, look through our trustee list.
        for @!trustees {
            if $_.WHAT =:= $claimant.WHAT {
                return 1;
            }
        }
        
        # If we get here, not trusted.
        0
    }
}
